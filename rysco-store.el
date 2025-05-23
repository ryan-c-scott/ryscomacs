(require 'helm-org-ql)

(defvar rysco-store-last-entry-name "")
(defvar rysco-store-templates nil)
(defvar rysco-store-templates--processed nil)

(defvar rysco-store-directories `(,(concat org-directory "store")))
(defvar rysco-store-default-capture-file (expand-file-name "db.org" (concat org-directory "store")))
(defvar rysco-store-default-export-buffer-name "*ql results*")
(defvar rysco-store-default-export-options '("toc:nil" "num:nil" "\\n:t"))
(defvar rysco-store-freshness-threshold "+7d" "Freshness query threshold as read by `org-read-date'")
(defvar rysco-store-freshness-schedule "+3m" "Future freshness check date as read by `org-read-date'")
(defvar rysco-store-initial-query nil)
(defvar rysco-store-kindle-vocab-file nil)
(defvar rysco-store-kindle-file nil)

(defvar-local rysco-store-insert-after-capture nil)

;;;###autoload
(defun rysco-store-load-templates ()
  (interactive)
  (cl-loop
   for (dir . store-templates) in rysco-store-templates do
   (cl-loop
    for (key nice-name template . rest) in store-templates
    as capture-template = `(,key ,(concat "[store] " nice-name)
                                 entry
                                 (function rysco-store-location)
                                 (file ,(expand-file-name template dir))
                                 :create-id t
                                 :add-timestamp t
                                 ,@rest)
    do
    (add-to-list 'org-capture-templates capture-template)
    (add-to-list 'rysco-store-templates--processed capture-template))))

;;;###autoload
(defun rysco-store-load ()
  (interactive)
  (let ((files (org-ql-search-directories-files
                :directories
                (rysco-store-existing-directories
                 rysco-store-directories))))
    (cl-loop
     for f in files
     as buff = (find-file-noselect f)
     do
     (with-current-buffer buff
       (unless (eq major-mode 'org-mode)
         (org-mode))))))

;;;###autoload
(defun rysco-store-directory-dired ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "Store Directories"
          :candidates rysco-store-directories
          :action `(("Dired" . ,(lambda (&rest dir)
                                  (dired dir)))))))

(defun rysco-store-location ()
  (let* (existing-node
         (template-target (org-capture-get :target-file))
         (title (funcall-interactively
                 'helm-rysco-store-ql
                 :name "Knowledge Store Query"
                 :actions `(("Use Existing" . ,(lambda (marker)
                                                 (setq existing-node marker)
                                                 (buffer-file-name (marker-buffer marker)))))
                 :sources `(,(helm-build-dummy-source "Custom")))))

    (if (eq helm-exit-status 1)
        (keyboard-quit)

      (setq rysco-store-last-entry-name nil)

      (find-file
       (if existing-node
           title
         (setq rysco-store-last-entry-name
                (read-string "Confirm: " title))
         (or template-target rysco-store-default-capture-file)))

      (goto-char (or existing-node (point-max))))))

(defun rysco-store-existing-directories (directories)
  (-filter #'f-exists? directories))

(defmacro rysco-store--with-buffer-at-marker (marker &rest body)
  (declare (indent defun) (debug (form body)))
  `(with-current-buffer (marker-buffer ,marker)
     (save-excursion
       (goto-char ,marker)
       ,@body)))

;;;###autoload
(defun rysco-store-backlinks ()
  (interactive)
  "See: `org-sidebar-backlinks'."
  (interactive)
  (let* ((id (org-entry-get (point) "ID"))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         ;; FIXME: Do CUSTOM_ID links also have an "id:" prefix?
         (query (cond ((and id custom-id)
                       ;; This will be slow because it isn't optimized to a single regexp.  :(
                       (warn "Entry has both ID and CUSTOM_ID set; query will be slow")
                       `(or (link :target ,(concat "id:" id))
                            (link :target ,(concat "id:" custom-id))))
                      ((or id custom-id)
                       `(link :target ,(concat "id:" (or id custom-id))))
                      (t (error "Entry has no ID nor CUSTOM_ID property")))))
    (org-sidebar-ql (org-ql-search-directories-files :directories (rysco-store-existing-directories rysco-store-directories))
      query :title (concat "Links to: " (org-get-heading t t)))))

;; NOTE: This seems wrong... org-sidebar-ql parameters don't match current definition
(defun rysco-store-query ()
  (interactive)
  (funcall-interactively 'org-sidebar-ql :directories (rysco-store-existing-directories rysco-store-directories)))

(cl-defun rysco-store-export-query-to-buffer (&key files query buffer)
  (let* ((files (or files
                    (org-ql-search-directories-files
                     :directories
                     (rysco-store-existing-directories
                      rysco-store-directories))))
         (query (pcase query
                  ('nil query)
                  ((pred stringp)
                   (org-ql--query-string-to-sexp query))
                  (_ query)))
         (buffer (or buffer rysco-store-default-export-buffer-name))
         (content (org-ql-select files query
                    :action '(let ((el (org-element-headline-parser)))
                               (buffer-substring-no-properties
                                (org-element-property :begin el)
                                (org-element-property :end el))))))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (format "#+title: %s\n" query))
      (cl-loop
       initially do (insert "#+options:")
       for opt in rysco-store-default-export-options do
       (insert " " opt)
       finally do (insert "\n\n"))

      (cl-loop
       for chunk in content do
       (insert chunk "\n"))
      (org-mode)
      (org-fold-show-all)

      ;; Remove all IDs
      (org-element-map (org-element-parse-buffer) '(property-drawer drawer)
        (lambda (drawer)
          (let ((begin (org-element-property :begin drawer))
                (end (org-element-property :end drawer)))
            (goto-char begin)
            (replace-string ":ID: " ":ORIGINAL-ID: ")
          nil))))
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun rysco-store-create-and-insert ()
  (interactive)
  (setq rysco-store-insert-after-capture t)
  (let ((org-capture-templates rysco-store-templates--processed))
    (org-capture)))

;;;###autoload
(defun rysco-store-org-stamp-freshness ()
  (interactive)
  (if-let ((marker (org-get-at-bol 'org-marker)))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (rysco-store-org-stamp-freshness)))
    (when (derived-mode-p 'org-mode)
      (org-toggle-tag "draft" 'on)
      (org-schedule nil rysco-store-freshness-schedule))))

;;;###autoload
(defun rysco-store-rebuild-links ()
  "Helper function to rebuild org ID database using `org-id-update-id-locations'"
  (interactive)
  (cl-loop
   for dir in (rysco-store-existing-directories rysco-store-directories) do
   (org-id-update-id-locations
    (directory-files-recursively dir ".org"))))

(defun rysco-store--insert-links (markers)
  (let* ((count (length markers))
         (replacing (org-region-active-p))
         (sep (cond
               ((and replacing (= count 1))
                "")
               (t "\n"))))

    (when replacing
      (delete-region (region-beginning) (region-end)))

    (cl-loop
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

(defun rysco-store-capture-add-timestamp ()
  (when (org-capture-get :add-timestamp)
    (org-set-property "CREATED" (format-time-string "%F"))))

(defun rysco-store-post-capture ()
  (when (and rysco-store-insert-after-capture (not org-note-abort))
    (setq rysco-store-insert-after-capture nil)
    (rysco-store--insert-links `(,org-capture-last-stored-marker))))

(add-hook 'org-capture-mode-hook #'rysco-store-capture-create-id)
(add-hook 'org-capture-mode-hook #'rysco-store-capture-add-timestamp)
(add-hook 'org-capture-after-finalize-hook 'rysco-store-post-capture)

;;;;;;;;;;;;;;;;;;;
(defun rysco-store-kindle-get-books-vocab (location)
  (with-temp-buffer
    (cl-loop
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
  (cl-loop
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
    (cl-loop
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
  (cl-loop
   for book in (org-ql-select
                 (org-ql-search-directories-files :directories (rysco-store-existing-directories rysco-store-directories))
                 '(tags-local "book")
                 :action 'element-with-markers)

   as title = (car (org-element-property :title book))
   as marker = (org-element-property :org-marker book)
   collect
   `(,(substring-no-properties title)
     ,marker)))

(defun rysco-store-kindle-get-books (location)
  (let* ((author-profiles (rysco-store-kindle-get-books-author-profiles (concat location "/documents")))
         (vocab (rysco-store-kindle-get-books-vocab (concat location "/system/vocabulary")))
         (clippings (rysco-store-kindle-extract-clippings (concat location "/documents/My Clippings.txt")))
         matched
         (books (append
                 vocab
                 (cl-loop
                  for entry in author-profiles
                  for (id title author) in author-profiles
                  as existing = (assoc id vocab 'string=)

                  do (push title matched)
                  unless existing collect entry)

                 ;; Loop through the clippings and create entries for anything not matched above
                 ;; .Add "Clippings - " to the title
                 (cl-loop
                  for title being the hash-keys of clippings
                  as highlights = (gethash title clippings)

                  when (and (not (member title matched)) highlights) collect
                  `("???" ,(concat "Clippings - " title) "???" ,highlights)
                 ))))

    ;; Inject highlights/notes
    (cl-loop
     for (id title author highlights) in books
     as highlights = (or highlights (gethash title clippings))
     collect `(,id ,title ,author ,highlights))))

(defun rysco-store-insert-vocab-kindle (location)
  (interactive "D")

  (unless rysco-store-kindle-vocab-file
    (error "`rysco-store-kindle-vocab-file' not set"))

  (with-current-buffer (find-file rysco-store-kindle-vocab-file)
    (erase-buffer)
    (insert
     (org-element-interpret-data
      `((headline
         (:level 1 :title "Kindle Vocab" :tags ("vocab")))

        ,@(cl-loop
           with vocab = (rysco-store-kindle-get-vocab-list (concat location "/system/vocabulary"))
           for (group . words) in vocab collect
           `((headline (:level 2 :title ,group))
             ,@(cl-loop
                for word in words collect
                `("  - "  ,word "\n"))
             "\n")))))))

(defun rysco-store-insert-books-kindle (books)
  (cl-loop
   with existing = (rysco-store-get-books)
   for (id title author highlights) in books
   as node = (assoc title existing)
   as marker = (cadr node)

   ;; Only create initial node if it doesn't exist
   unless node do
   (setq marker
         (with-current-buffer (find-file rysco-store-kindle-file)
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

           ,@(cl-loop
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

(provide 'rysco-store)
