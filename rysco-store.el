(require 'helm-org-ql)

(defvar rysco-store-last-entry-name "")
(defvar rysco-store-templates nil)
(defvar rysco-store-templates--processed nil)
(defvar rysco-store-directory (concat org-directory "store"))

(defvar-local rysco-store-insert-after-capture nil)

;;;###autoload
(defun rysco-store-load-templates (&optional dir)
  (interactive)
  (let ((dir (or dir (expand-file-name (concat rysco-store-directory "/templates")))))
    (cl-loop
     for (key nice-name template) in rysco-store-templates
     as capture-template = `(,key ,(concat "[store] " nice-name)
                                  entry
                                  (function rysco-store-location)
                                  (file ,(expand-file-name template dir))
                                  :create-id t)
     do
     (add-to-list 'org-capture-templates capture-template)
     (add-to-list 'rysco-store-templates--processed capture-template))))

(defun rysco-store-create-file-name (title)
  (convert-standard-filename
   (expand-file-name
    (format "%s/%s-%s.org"
            rysco-store-directory
            (format-time-string "%Y%m%d")
            (s-replace "/" "-" title)))))

(defun rysco-store-location ()
  (let* (existing-node
         (title (funcall-interactively
                 'helm-rysco-store-ql
                 :name "Knowledge Store Query"
                 :actions `(("Use Existing" . ,(lambda (marker)
                                                 (setq existing-node t)
                                                 (buffer-file-name (marker-buffer marker)))))
                 :sources `(,(helm-build-dummy-source "Custom")))))

    (if (eq helm-exit-status 1)
        (keyboard-quit)

      ;; TODO: Existing node behavior

      (find-file
       (if existing-node
           title
         (rysco-store-create-file-name
          (setq rysco-store-last-entry-name
                (read-string "Confirm: " title))))))))

(defmacro with-store-directory (&rest forms)
  "Lexically binds `org-directory' to `rysco-store-directory' and executes FORMS"
  `(let ((org-directory rysco-store-directory))
     ,@forms))

(defmacro rysco-store--with-buffer-at-marker (marker &rest body)
  (declare (indent defun) (debug (form body)))
  `(with-current-buffer (marker-buffer ,marker)
     (save-excursion
       (goto-char ,marker)
       ,@body)))

;;;###autoload
(defun rysco-store-backlinks ()
  (interactive)
  (with-store-directory
   (funcall-interactively 'org-sidebar-backlinks)))

(defun rysco-store-query ()
  (interactive)
  (with-store-directory
   (funcall-interactively 'org-sidebar-ql)))

;;;###autoload
(defun rysco-store-create-and-insert ()
  (interactive)
  (setq rysco-store-insert-after-capture t)
  (let ((org-capture-templates rysco-store-templates--processed))
    (org-capture)))

;;;###autoload
(defun helm-rysco-store-query ()
  (interactive)
  (funcall-interactively
   'helm-rysco-store-ql
   :name "Knowledge Store Query"
   :actions `(,@helm-org-ql-actions
              ("Insert as link" . helm-rysco-store--insert-candidates))))

(cl-defun helm-rysco-store-ql (&key buffers-files (boolean 'and) (name "helm-org-ql") sources actions)
  "See: `helm-org-ql'."
  (interactive (list (current-buffer)))
  (with-store-directory
   (let ((boolean (if current-prefix-arg 'or boolean))
         (helm-input-idle-delay helm-org-ql-input-idle-delay)
         (helm-org-ql-actions actions)
         (buffers-files (or buffers-files (org-ql-search-directories-files))))

     (helm :prompt (format "Query (boolean %s): " (-> boolean symbol-name upcase))
           :sources `(,@sources
                      ,(helm-org-ql-source buffers-files :name name))))))

(defun helm-rysco-store--insert-candidates (&optional _)
  (rysco-store--insert-links (helm-marked-candidates :all-sources t)))

(defun rysco-store--insert-links (markers)
  (let* ((count (length markers))
         (replacing (org-region-active-p))
         (sep (cond
               ((and replacing (= count 1))
                "")
               (t "\n"))))

    (when replacing
      (delete-region (region-beginning) (region-end)))

    (loop
     for this-link in markers do
     (insert (rysco-store-get-marker-link this-link) sep))))

(defun rysco-store-get-marker-link (marker &optional link-text)
  (rysco-store--with-buffer-at-marker marker
    (org-link-make-string
     (format "id:%s" (org-id-get-create))
     (or link-text (org-display-outline-path nil t nil t)))))

(defun rysco-store-capture-create-id ()
  (when (org-capture-get :create-id)
    (org-id-get-create)))

(defun rysco-store-post-capture ()
  (when (and rysco-store-insert-after-capture (not org-note-abort))
    (setq rysco-store-insert-after-capture nil)
    (rysco-store--insert-links `(,org-capture-last-stored-marker))))

(add-hook 'org-capture-mode-hook #'rysco-store-capture-create-id)
(add-hook 'org-capture-after-finalize-hook 'rysco-store-post-capture)

(defun helm-rysco-store--heading (window-width)
  ""
  (font-lock-ensure (point-at-bol) (point-at-eol))
  (let* ((heading (org-get-heading t))
         (path (-> (org-get-outline-path)
                   (org-format-outline-path window-width nil "")
                 (org-split-string "")))
         (path (if helm-org-ql-reverse-paths
                   (concat heading "\\" (s-join "\\" (nreverse path)))
                 (concat (s-join "/" path) "/" heading))))
    (cons path (point-marker))))

(eval-after-load 'helm-org-ql
  (advice-add 'helm-org-ql--heading :override 'helm-rysco-store--heading))

;;;;;;;;;;;;;;;;;;;
(defun rysco-store-kindle-get-books-vocab (location)
  (with-temp-buffer
    (loop
     with default-directory = location
     with raw = (shell-command "sqlite3 -readonly -separator \"    \" vocab.db \"select * from book_info;\"" t nil)
     with data = (s-split "\n" (buffer-string))
     for line in data
     as entry = (s-split "    " line)
     as book = (-let [(_ id _ _ title author) entry]
                 (when (and id title author)
                   `(,id ,title ,author)))
     when book collect book)))

(defun rysco-store-kindle-get-vocab-list (location)
  (let ((default-directory location))
    (--group-by
     (upcase (substring it 0 1))
     (s-split
      "\n"
      (with-temp-buffer
        (shell-command "sqlite3 -readonly vocab.db \"select word from words order by word;\"" t nil)
        (buffer-string))
      t))))

(defun rysco-store-kindle-get-books-author-profiles (location)
  (loop
   for f in (directory-files-recursively location "AuthorProfile.profile.*.asc$")
   as data = (progn
               (with-temp-buffer
                 (insert-file-contents (expand-file-name f location))
                 (let* ((data (json-parse-buffer))
                        (authors (append (gethash "u" data) nil))
                        (book-list (append (gethash "o" data) nil))
                        (book-id (gethash "a" data))
                        (book (--first (string= (gethash "a" it) book-id) book-list))
                        (title (when book (gethash "t" book))))
                   (when title
                     `(,book-id
                       ,title
                       ,(gethash "n" (car authors)))))))
   when data collect data))

(defun rysco-store-kindle-extract-clippings (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (loop
     with out = (make-hash-table :test 'equal)

     for raw in (s-split "==========" (buffer-string)) do
     (pcase raw
       ((rx
         ;; HACK: Had to find a way to ignore a zero width unicode character character that on some entries
         (let title (seq (category ascii) (*? anything))) " (" (let author (*? anything)) ")" "\n"
         bol "- Your " (let type (*? anything)) " on "
         (** 0 1 "page " (let page (+ digit)) " | ")
         "Location " (or (seq (let beg (+ digit)) "-" (let end (+ digit)))
                         (let beg (+ digit)))
         " | Added on " (let added (*? anything)) eol
         (* "\n")
         (let text (*? anything))
         (* "\n") eos)

        (let* ((highlights (gethash title out)))
          (puthash title
                   (append highlights `((,type ,page (,beg ,(or end beg)) ,added ,text)))
                   out))))
     finally return out)))

(defun rysco-store-get-books ()
  (with-store-directory
   (loop
    for book in (org-ql-select
                  (org-ql-search-directories-files)
                  '(tags-local "book")
                  :action 'element-with-markers)

    as title = (car (org-element-property :title book))
    as marker = (org-element-property :org-marker book)
    collect
    `(,(substring-no-properties title)
      ,marker))))

(defun rysco-store-kindle-get-books (location)
  (let* ((author-profiles (rysco-store-kindle-get-books-author-profiles (concat location "/documents")))
         (vocab (rysco-store-kindle-get-books-vocab (concat location "/system/vocabulary")))
         (clippings (rysco-store-kindle-extract-clippings (concat location "/documents/My Clippings.txt")))
         matched
         (books (append
                 vocab
                 (loop
                  for entry in author-profiles
                  for (id title author) in author-profiles
                  as existing = (assoc id vocab 'string=)

                  do (push title matched)
                  unless existing collect entry)

                 ;; Loop through the clippings and create entries for anything not matched above
                 ;; .Add "Clippings - " to the title
                 (loop
                  for title being the hash-keys of clippings
                  as highlights = (gethash title clippings)

                  when (and (not (member title matched)) highlights) collect
                  `("???" ,(concat "Clippings - " title) "???" ,highlights)
                 ))))

    ;; Inject highlights/notes
    (loop
     for (id title author highlights) in books
     as highlights = (or highlights (gethash title clippings))
     collect `(,id ,title ,author ,highlights))))

(defun rysco-store-insert-vocab-kindle (location)
  (interactive "D")
  (with-store-directory
   (with-current-buffer (find-file (expand-file-name "kindle-vocab.org" org-directory))
     (erase-buffer)
     (insert
      (org-element-interpret-data
       `((headline
          (:level 1 :title "Kindle Vocab" :tags ("vocab")))

         ,@(loop
            with vocab = (rysco-store-kindle-get-vocab-list (concat location "/system/vocabulary"))
            for (group . words) in vocab collect
            `((headline (:level 2 :title ,group))
               ,@(loop
                  for word in words collect
                  `("  - "  ,word "\n"))
               "\n"))))))))

(defun rysco-store-insert-books-kindle (books)
  (loop
   with existing = (rysco-store-get-books)
   for (id title author highlights) in books
   as node = (assoc title existing)
   as marker = (cadr node)

   ;; Only create initial node if it doesn't exist
   unless node do
   (setq marker
         (with-current-buffer (find-file (rysco-store-create-file-name title))
           (insert
            (org-element-interpret-data
             `((headline
                (:level 1 :title ,title :tags ("book"))
                (property-drawer
                 nil
                 (node-property (:key AUTHOR :value ,author))
                 (node-property (:key PUBLISHED))
                 (node-property (:key AMAZON-ID :value ,id))))

               ,(format "\nhttps://www.amazon.com/dp/%s\n\n" id))))

           (org-id-get-create)
           (point-marker)))

   ;; If it does exist, create a backup
   when node do
   (progn
     ;; TODO:
     (message "IMPORTING EXISTING BOOK: %s" title))

   ;; Always create a new "Kindle Notes" headline
   do
   (rysco-store--with-buffer-at-marker marker
     ;; TODO: Should try and go to the end of this node and not the file
     ;; .Look into options for removing old kindle syncs
     (goto-char (point-max))

     (when highlights
       (insert
        (org-element-interpret-data
         `((headline
            (:level 2 :title "Kindle Notes")
            (property-drawer
             nil
             (node-property (:key SYNCED :value ,(format-time-string "%Y-%m-%d:%H:%M")))))

           "\n"

           ,@(loop
              for (type page location added text) in highlights
              as timestamp = (format-time-string
                              "%Y-%m-%d:%H:%M"
                              (date-to-time added))

              collect
              `((headline
                 (:level 3 :title ,(format "pg. %s - loc. %s" (or page "?") location))
                 (property-drawer
                  nil
                  (node-property (:key DATE :value ,timestamp))
                  (node-property (:key PAGE :value ,page))
                  (node-property (:key LOCATION :value ,location))))
                "\n"

                ,(pcase type
                   ("Highlight"
                    `(quote-block
                      nil
                      ,text
                      "\n"))

                   ("Note"
                    text))

                "\n"))))))

     (set-buffer-file-coding-system 'utf-8))))

(defun helm-rysco-store-kindle-import (location)
  (interactive "D")
  (helm :prompt "Kindle Import "
        :sources
        (helm-build-sync-source "Kindle Books"
          :candidates (lambda ()
                        (--map `(,(format "%s:\t%s" (caddr it) (cadr it)) . ,it)
                               (rysco-store-kindle-get-books location)))
          :action `(("Insert" . (lambda (&rest _)
                                  (rysco-store-insert-books-kindle (helm-marked-candidates))))))))

(provide 'rysco-org-store)
