;;; -*- lexical-binding: t; -*-

(require 'dash)

(defun rysco-semantic-mode (&optional state)
  (if state
      (with-demoted-errors
          (semantic-mode t))
    (semantic-mode -1)))

;;;;;;;;;
(defun rysco-split-dwim (dir &optional switch)
  ""
  (let* ((can-horizontal (not (or (windmove-find-other-window 'left) (windmove-find-other-window 'right))))
         (window-below (windmove-find-other-window 'down))
         (can-vertical (not (or (windmove-find-other-window 'up) (and window-below (not (window-minibuffer-p window-below)))))))

    (cond ((and (or (eq dir 'left) (eq dir 'right)) can-horizontal) (split-window-right))
          ((and (or (eq dir 'up) (eq dir 'down)) can-vertical) (split-window-below)))

         (windmove-do-window-select dir)))

(defun rysco-split-up-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'up t))

(defun rysco-split-down-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'down t))

(defun rysco-split-right-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'right t))

(defun rysco-split-left-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'left t))

(cl-defun rysco-delete-or-clone-window-dwim (arg)
  (interactive "P")
  (if arg
      (rysco-clone-and-narrow)
    (delete-window)))

(cl-defun rysco-delete-or-kill-other-windows-dwim (arg)
  (interactive "P")
  (if arg
      (rysco-kill-all-clones)
    (delete-other-windows)))

(cl-defun rysco-clone-and-narrow ()
  (interactive)
  (let ((master
         (or
          (and (boundp 'rysco-cloned-from) rysco-cloned-from)
          (current-buffer))))

    (with-current-buffer (clone-indirect-buffer-other-window nil t)
      (setq-local rysco-cloned-from master)
      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (pcase major-mode
          ('org-mode (org-narrow-to-subtree))
          (_ (narrow-to-defun)))))))

(cl-defun rysco-kill-all-clones ()
  (interactive)
  (cl-loop
   with kill-list
   with get-master = (lambda (&optional buf)
                       (with-current-buffer (or buf (current-buffer))
                         (or
                          (and (boundp 'rysco-cloned-from) rysco-cloned-from)
                          (current-buffer))))
   with master = (funcall get-master)
   for buf being the buffers
   as this-master = (funcall get-master buf)
   if (and this-master
           (not (equal buf master))
           (equal this-master master))
   collect buf into kill-list

   finally do
   (progn
     (message "Killing %s clones %s" master kill-list)
     (cl-loop
      for buf in kill-list do
      (kill-buffer buf)))))

(defun rysco-comment-dwim (arg)
  ""
  (interactive "*P")
  (if mark-active
      (progn
        (when arg
          (kill-ring-save (region-beginning) (region-end)))
        (comment-dwim nil))
    (comment-line 1)
    (forward-line -1)))

(defun rysco-clear-undo ()
  ""
  (interactive)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun rysco-set-buffer-local-font (&optional font-family)
  "Sets font in current buffer"
  (interactive)
  (let ((font-family (or font-family
                         (helm :sources `(,(helm-build-sync-source "Font Families"
                                             :candidates (font-family-list)))))))
    (buffer-face-set `(:family ,font-family))))

(defun rysco-toggle-writing-face ()
  (interactive)
  (if (and (boundp 'buffer-face-mode) buffer-face-mode)
      (buffer-face-mode -1)
    (rysco-set-buffer-local-font rysco-writing-font)))

(defun replace-regexp-and-return (from to)
  (save-excursion
    (while (re-search-forward from nil t)
      (replace-match to))))

(defun rysco-calendar-exit-and-insert-date (arg)
  "Capture the date at point, exit the Calendar, and insert the date using 'org-date-from-calendar."
  (interactive "P")
  (when (equal major-mode 'calendar-mode)
    (calendar-exit)
    (if (or arg (equal major-mode 'org-mode))
        (org-date-from-calendar)
      (seq-let (month day year) (org-get-date-from-calendar)
        (insert (format "%d-%02d-%02d" year month day))))))

(cl-defun helm-rysco-rotate-windows ()
  (interactive)
  (helm
   :sources
   (helm-build-sync-source "Layouts"
     :candidates
     '(("even-horizontal" . rotate:even-horizontal)
       ("even-vertical" . rotate:even-vertical)
       ("main-horizontal" . rotate:main-horizontal)
       ("main-vertical" . rotate:main-vertical)
       ("tiled" . rotate:tiled)
       ("*next*" . rotate:layout))
     :action
     (lambda (func)
       (funcall-interactively func)))))

(cl-defun rysco-rotate-windows (arg)
  (interactive "P")
  (if arg
      (helm-rysco-rotate-windows)
    (--when-let (selected-window)
      (rotate-window)
      (select-window it))))

(defun vertical-windows-with-related()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (ff-find-related-file t t))

(defun ff-find-related-file-ignore-include()
  (interactive)
  (ff-find-related-file nil t))

(defun kill-all-matching (criteria)
  "Kills all buffers that the criteria function returns non-nil on"
  (mapc (lambda (buffer)
	  (when (funcall criteria buffer)
	    (kill-buffer buffer)))
	(buffer-list)))

(defun kill-all-matching-prefix (buffer-prefixes)
  (interactive "s")
  (kill-all-matching (lambda (buffer)
                       (let ((names buffer-prefixes)
                             (bufname (buffer-name buffer))
                             (found))
                         (while names
                           (if (string-match (car names) bufname)
                               (progn
                                 (setq found t)
                                 (setq names nil))
                             (setq names (cdr names))))
                         found))))

(defun killall()
  "Kill all non-system buffers"
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (not (string-match "^*" (buffer-name buffer)))))
  (when (fboundp 'dvc-kill-all-buffers)
    (dvc-kill-all-buffers))
  (switch-to-buffer "*scratch*")
  (delete-other-windows))

(defun kill-dired-buffers ()
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (eq 'dired-mode (buffer-local-value 'major-mode buffer)))))

(defun rysco-dired-os-open-dir ()
  (interactive)
  (run-associated-program default-directory))

(defun kill-rcirc-buffers ()
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (eq 'rcirc-mode (buffer-local-value 'major-mode buffer)))))

(defun rysco-file-name-to-sudo (&optional filename)
  (let ((filename (or filename buffer-file-name)))
    (if (tramp-tramp-file-p filename)
        (with-parsed-tramp-file-name filename this
          (concat
           (substring filename 0 (1- (* -1 (length this-localname))))
           "|sudo::"
           this-localname))
      ;;
      (concat "/sudo::" filename))))

(defun rysco-revert-buffer (&optional arg)
    "Revert buffer, prompting if it has been modified."
    (interactive "P")
    (let ((filename (buffer-file-name))
          (current-pos (point)))

      (cond
       ((equal major-mode 'dired-mode)
        (if arg
            (dired (rysco-file-name-to-sudo default-directory))
          (revert-buffer)))

       (filename
        (when (find-alternate-file
               (if arg
                   (rysco-file-name-to-sudo filename)
                 filename))
          (goto-char current-pos))))))

(defun markdown-toc ()
  (interactive)
  (let (res depth title)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\(#*\\) \\(.*\\)$" nil t)
	(add-to-list 'res `(,(match-string 1) ,(match-string 2)))))

    (dolist (elt (reverse res))
      (setq depth (replace-regexp-in-string (regexp-quote "#") (car elt) "\t" nil 'literal))
      (setq title (cadr elt))

      (insert (format "%s1. [%s]" depth title))
      (setq title (replace-regexp-in-string "\s+" "-" title))
      (setq title (replace-regexp-in-string "[:*]" "" title))
      (insert (format "(#%s)\n" (downcase title))))))

(defun markdown-insert-youtube (id &optional label)
  (interactive "sID: \nsLabel: ")
  (insert (format "[![%s](http://img.youtube.com/vi/%s/0.jpg)](http://www.youtube.com/watch?v=%s)" label id id)))

(defun org-archive-all-done-item ()
  "Archive all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ DONE" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ DONE" nil t)
            (org-advertized-archive-subtree))
          (org-display-all-todo-item)
          (message "Archive finished"))
      (org-display-all-todo-item)
      (message "No need to archive"))))

(defun rysco-org-export-writing-outline (&optional arg)
  "Exports the current org file to a master export (in org format) by traversing any headings tagged `:outline:' and outputing `\#+INCLUDE:' for every linked file found.  This is for ease of moving around sections and interspersing those references with notes as needed.
Normally the outline would also be tagged `:noexport:' so that it will be excluded from the output."
  (interactive "P")
  (require 'ox)
  
  (let (content)
    (setq content (buffer-string))
    
    (with-temp-buffer
      (insert content "\n")
      (goto-char (point-max))
      
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (el)
          (prog1 nil
            (when (member "outline" (org-element-property :tags (org-element-lineage el '(headline))))
              (insert (format "#+INCLUDE: \"%s%s\" :minlevel 1\n"
                              (org-element-property :path el)
                              (--if-let (org-element-property :search-option el)
                                  (concat "::" (org-link-unescape it))
                                "")))))))

      (org-mode)
      (org-export-expand-include-keyword)

      (if (not arg)
          (write-file (org-export-output-file-name ".org"))
        (let ((content (buffer-string))
              (buffer (get-buffer-create "*Writing Outline Master Export*")))
          (with-current-buffer buffer
            (erase-buffer)
            (insert content)
            (org-mode)
            (read-only-mode)
            (goto-char (point-min)))
          (switch-to-buffer-other-window buffer))))))

(defun rysco-edit-config()
  "Open the users 'ryscomacs/config.el under 'user-emacs-directory"
  (interactive)
  (find-file (expand-file-name "ryscomacs/config.el" user-emacs-directory)))

(defun rysco-generate-ssh-config ()
  "Generates ssh config file from snippets in directories specified in 'rysco-ssh-config-directories"
  (interactive)
  (let* ((out (find-file "~/.ssh/config"))
	 (directories
	  (cl-remove-if-not 'file-exists-p
			 (append '("~/.ssh/config.d/") rysco-ssh-config-directories))))
    (backup-buffer)
    (erase-buffer)

    (insert (format "### Generated by Ryscomacs.  Do not edit ###
### See snippets in %s\n\n" directories))
    
    (dolist (path directories)
      (dolist (include (directory-files path t))
	(when (not (file-directory-p include))
	  (insert (format "### %s\n" include))
	  (insert
	   (with-temp-buffer
	     (insert-file-contents include)

	     (if (equal "el" (file-name-extension include))
		 (eval (car
			(read-from-string
			 (concat "(with-output-to-string " (buffer-string) ")"))) t)
	     (buffer-string)))))))
    (save-buffer)
    (kill-buffer)))

(defun rysco-web-query (query)
  "Visits a search engine using `browse-url'.
The URL is created by calling `format' with the provided QUERY, using the format string `rysco-web-search-engine-string'."
  (interactive (-let [initial (or
                               (if(region-active-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (thing-at-point 'symbol))
                               "")]
                 `(,(read-string
                     "Query: "
                     (s-trim
                      (s-replace-regexp "[()]+" "" initial))))))
  (browse-url
   (format rysco-web-search-engine-string (url-encode-url query))))

(defun rysco-private-browsing-p ()
  (and
   (equal browse-url-chrome-program rysco-private-browser-program)
   (equal browse-url-chrome-arguments rysco-private-browser-arguments)
   (equal browse-url-browser-function 'rysco-browse-url-private)))

(defun rysco-browse-url-private (url &rest args)
  (cond
   ((or (not rysco-private-browser-program)
        (not rysco-private-browser-arguments))
    (error "Rysco private browsing not configured correctly"))

   ((not (rysco-private-browsing-p))
    (error "Private browsing not enabled.  See `rysco-private-browsing"))

   (t
    (apply 'browse-url-chrome url args))))

(defun rysco-private-browsing ()
  (interactive)
  "Makes overrides global browsing settings with local values that will use configured private browsing"
  (set (make-local-variable 'browse-url-chrome-program) rysco-private-browser-program)
  (set (make-local-variable 'browse-url-chrome-arguments) rysco-private-browser-arguments)
  (set (make-local-variable 'browse-url-browser-function) 'rysco-browse-url-private))

(defun rysco-make-buffer-utf8 (&optional buf)
  "Tidy up a buffer by replacing all special Unicode characters
   (smart quotes, etc.) with their more sane cousins.
If region is active, narrow to the region boundaries first."
  (interactive)
 
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("[\u2013\|\u2014]" . "-")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))

    (with-current-buffer (or buf (current-buffer))
      (save-restriction
        (when (region-active-p)
          (narrow-to-region (region-beginning) (region-end)))

        (save-excursion
          (cl-loop
           with content = (buffer-string)
           for (key . value) in unicode-map do
           (goto-char (point-min))
           (cl-loop while (re-search-forward key nil t) do
                    (replace-match value))))))))

(defun rysco-repo-status (&optional dir)
  "Run either `monky-status' or `magit-status' for hg or git repositories respectively"
  (interactive)
  (helm :sources
        (helm-build-sync-source "Projects"
          :candidates (cons "." projectile-known-projects)
          :action
          (lambda (dir)
            (cond
             ((--when-let (vc-find-root dir ".hg")
                (monky-status it)))
             ((--when-let (vc-find-root dir ".git")
                (magit-status it)))
             (t
              (message "Not a git or hg repository")))))))

(defun color-invert (color)
  (cl-loop for el in (if (stringp color) (color-name-to-rgb color) color)
           collect (- 1 el)))

(defun color-lerp (start end mix)
  (cl-loop for scol in (if (stringp start) (color-name-to-rgb start) start)
           for ecol in (if (stringp end) (color-name-to-rgb end) end)
           collect (+ (* scol (- 1 mix))
                      (* ecol mix))))

(defun color-ramp (start end steps)
  (cl-loop
   with start = (color-name-to-rgb start)
   with end = (color-name-to-rgb end)
   for i from 0 to (1- steps)
   collect
   (apply
    'color-rgb-to-hex
    (color-lerp start end (/ (float i) steps)))))

(defun int-to-bin-string (val &optional pad set-char unset-char)
  (let ((pad (or pad 1))
        (count (logb val))
        (set-char (or set-char "1"))
        (unset-char (or unset-char "0")))

    (cl-loop
     with out
     for pos from 0 to count concat
     (prog1 (if (= (logand 1 val) 1) set-char unset-char)
       (setq val (lsh val -1)))
     into out
     finally return
     (let ((out-length (length out)))
       (concat
        (make-string (max 0 (- (or pad out-length) out-length)) (string-to-char unset-char))
        (reverse out))))))

(defun rysco-parse-command-string (args)
  "Returns a list of arguments from ARGS following elisp syntax.
All items are converted to strings.
Quoted strings are returned as a single element."
  (loop
   for (a . pos) = (ignore-errors (read-from-string args pos))
   while a
   collect (format "%s" a)))

(defun rysco-json-pretty-string (text)
  (with-temp-buffer
    (rysco-json-pretty-insert text)
    (json-mode)
    (font-lock-default-function 'json-mode)
    (font-lock-default-fontify-region
     (point-min)
     (point-max)
     nil)
    (rysco-fontify-using-faces (buffer-string))))

(defun rysco-json-pretty-insert (text)
  (loop
   with text = (s-replace "\n" "" text)
   with array = 0
   with obj = 0
   with last
   with capture

   for c across text
   as before = nil
   as after = nil
   as skip = nil

   do
   (pcase c
     (?\"
      (unless (equal last ?\\)
        (setq capture (not capture)))))

   do
   (pcase c
     ((or ?\s ?\t)
      (unless (or capture (equal last ?\:))
        (setq skip t)))

     (?\n
      )

     (?\,
      (setq after t))

     (?\{
      (incf obj)
      (setq after t))

     (?\}
      (decf obj)
      (setq after t before t))

     (?\[
      (incf array)
      (setq after t))

     (?\]
      (decf array)
      (setq before t after t))
     )

   as depth = (make-string (* (+ array obj) 2) ?\s)

   if before do (insert depth "\n" depth)

   unless skip do (insert-char c)

   if after do (insert depth "\n" depth)

   do (setq last c)
))

(defun rysco-fontify-using-faces (text)
  (let ((pos 0))
    (while (setq next (next-single-property-change pos 'face text))
      (put-text-property pos next 'font-lock-face (get-text-property pos 'face text) text)
      (setq pos next))
    (add-text-properties 0 (length text) '(fontified t) text)
    text))

(defun pivot-table-columns (data &optional add-hlines)
  (let ((result
         (cl-loop
          for i from 0 below (length (car data)) collect
          (-select-column i data))))
    (if add-hlines
        (add-table-hlines result)
      result)))

(defun add-table-hlines (tbl)
  `(hline
    ,(car tbl)
    hline
    ,@(cdr tbl)
    hline))

(defun rysco-ido-fix ()
  (interactive)
  "Enable ido-mode with 'files option.
For some reason, currently, ido-mode will get disabled..."
  (ido-mode 'files))

;;;###autoload
(defun helm-rysco-project-ag (arg)
  "Preconfigured helm for grepping with AG in (projectile-project-root).
With prefix-arg prompt for type if available with your AG version."
  (interactive "P")
  (--when-let (projectile-project-root)
    (require 'helm-files)
    (helm-grep-ag it arg)))

(defun helm-rysco-occur-or-resume (arg)
  (interactive "P")
  (if arg
      (helm-resume nil)
    (helm-occur)))

(defun rysco-name-frame-project ()
  (interactive)
  (let ((name (projectile-project-name)))
    (if (string= name "-")
        (--when-let
            (pcase major-mode
              ('org-agenda-mode "AGENDA")
              (_ (read-string "Name: ")))
          (set-frame-name it))
      (set-frame-name (format "[ %s ]" (upcase name))))))

(defun rysco-frame-by-name (arg)
  (interactive "P")
  (if arg
      (call-interactively 'rysco-name-frame-project arg)
    (call-interactively 'select-frame-by-name)))

(defun helm-rysco-semantic-or-imenu (arg)
  (interactive "P")
  (if arg
      (call-interactively 'helm-imenu-in-all-buffers)
    (call-interactively 'helm-semantic-or-imenu)))

(defvar helm-rysco-insert-icon--last nil)

(defun helm-rysco-insert-icon (arg)
  "Helper for discovering fonts from all-the-icons"
  (interactive "P")

  (when arg
    (setq helm-rysco-insert-icon--last nil))

  (helm :candidate-number-limit nil
        :preselect helm-rysco-insert-icon--last
        :sources
        (cl-loop for family in '(alltheicon faicon fileicon octicon material wicon) collect
                 (helm-build-sync-source (format "%s" family)
                   :candidates
                   (let* ((data-f (all-the-icons--data-name family))
                          (insert-f (all-the-icons--function-name family))
                          (data (funcall data-f)))
                     (mapcar
                      (lambda (it)
                        (let ((entry
                               `(,(format "%s - %s"
                                          (funcall
                                           insert-f
                                           (car it))
                                          (car it))
                                 ,family
                                 ,(car it)
                                 ,(funcall
                                   insert-f
                                   (car it)))))
                                                                  
                          (cons
                           (car entry)
                           entry)))
                      data))
                   
                   :action
                   `(("Insert Icon & Label" .
                      (lambda (item)
                        (cl-loop
                         for (label . parts) in (helm-marked-candidates :all-sources t) do
                         (progn
                           (setq helm-rysco-insert-icon--last label)
                           (insert label "\n")))))

                     ("Insert Icon Only" .
                      (lambda (item)
                        (cl-loop
                         for (label _ _ icon) in (helm-marked-candidates :all-sources t) do
                         (progn
                           (setq helm-rysco-insert-icon--last label)
                           (insert icon "\n")))))
                        
                     ("Insert Code" .
                      (lambda (item)
                        (cl-loop
                         for (label family name icon) in (helm-marked-candidates :all-sources t) do
                        (insert
                         (format "(all-the-icons-%s \"%s\")\n" family name))))))))))

(defface rysco-common-links-tag
  '((t :foreground "white"
       :background "Cyan4"
       :slant italic
       :box 1))
  ""
  :group 'rysco-common-links)

(defface rysco-common-links-url-type
  '((t :foreground "Darkslategray4"
       :underline t
       :slant italic
       :box nil))
  ""
  :group 'rysco-common-links)

(defface rysco-common-links-title
  '((t :foreground "#F92672"
       :slant italic))
  ""
  :group 'rysco-common-links)

(defvar rysco-common-links-link-converter 'identity)

(defun helm-rysco-goto-common-links--pattern (pattern)
  (replace-regexp-in-string
   (rx (or line-start whitespace)
        ?:
      (group (optional ?^))
      (group (1+ (not whitespace))))
   (lambda (m)
     (let ((expand-start (string-empty-p (match-string 1 m)))
           (tag (match-string 2 m)))
       (concat ":"
               (when expand-start
                 (rx (0+ (not whitespace))))
               tag)))
   pattern))

(defun helm-rysco-goto-common-links--title (label tags &optional type location)
  (let ((tags-list
         (s-join
          " "
          (sort
           (loop for it in tags collect
                 (propertize
                  (format
                   "%s"
                   it)
                  'face 'rysco-common-links-tag
                  :tags t))
           'string<))))

    (when (equal type 'org)
      (setq location
            (cadr
             (s-match
              (rx "[[" (zero-or-more anything) "][" (group (zero-or-more anything)) "]]")
              location))))

    (format
     "%-25s %s%s%s"
     (propertize
      label
      'face 'rysco-common-links-title
      :tag-list tags-list)
     tags-list
     (make-string (--when-let (- 25 (length tags-list))
                    (if (> it 0)
                        it
                      1))
                  ?\s)
     (concat
      (when type
        ;; TODO:  Expand 'url types
        (propertize (format "%s" type) 'face 'rysco-common-links-url-type))
      " "
      location))))

(defun rysco-goto-common-links--type (link &optional allow-remote)
  (cond
   ((functionp link)
    'func)
   ((and (tramp-tramp-file-p link) (not allow-remote))
    'tramp)
   ((f-directory? link)
    'dired)
   ((listp link)
    ;; TODO: Allow for multiple links
    )
   ((s-starts-with? "[[" link)
    'org)
   ((f-exists? link)
    'file)
   (t
    'url)))

(defun rysco-goto-common-links--execute (link)
  (let* ((link (funcall rysco-common-links-link-converter link))
         (type (rysco-goto-common-links--type link t)))
    (pcase type
      ('func (funcall link))
      ('dired (dired link))
      ('org (org-link-open-from-string link))
      ('file (find-file link))
      ('url (browse-url link)))))

(defun helm-rysco-goto-common-links-default-action (candidate)
  (--if-let (helm-marked-candidates)
      (loop for link in it do (rysco-goto-common-links--execute link))
    (rysco-goto-common-links--execute candidate)))

(defun helm-rysco-goto-common-links ()
  (interactive)
  (let ((action 'helm-rysco-goto-common-links-default-action))
    (helm
     :truncate-lines t
     :sources
     `(,(helm-build-sync-source "Calendars"
          :candidates
          (loop
           for (id _ _ url) in rysco-gcal-calendars collect
           `(,(propertize
               id
               'face 'rysco-common-links-title) .
               ,url))
          :action action)

       ,(helm-build-sync-source "Bookmarks"
          :candidates
          (when (boundp 'bookmark-alist)
            (loop
             for (name . data) in bookmark-alist
             as tags = (car (assq 'tags data))
             collect
             `(,(helm-rysco-goto-common-links--title name tags) .
               ,name)))
          :action 'bookmark-jump-other-window)

       ,(helm-build-sync-source "Links"
          :pattern-transformer 'helm-rysco-goto-common-links--pattern
          :candidates
          (loop
           for (label link . tags) in rysco-common-links
           as type = (rysco-goto-common-links--type link)
           as url = (when (stringp link) (url-generic-parse-url link))
           as location = (when url (or (url-host url) link))

           collect
           `(,(helm-rysco-goto-common-links--title
               label tags type location) .
               ,link))
          :action action)))))

(defun rysco-load-theme (&optional theme)
  (interactive)
  (let ((current (cl-copy-list custom-enabled-themes)))
    (--if-let theme
        (load-theme theme)
      (call-interactively 'load-theme))
    
      (cl-loop for current-theme in current do
               (disable-theme current-theme))))

(defun rysco-load-theme-default ()
  (interactive)
  (rysco-load-theme rysco-theme))

(defun rysco-insert-current-theme-name ()
  (interactive)
  (--when-let (car custom-enabled-themes)
    (insert (format "'%s" it))))

(defun rysco-system-open-current-dir ()
  (interactive)
  (--when-let
   (cond
    ((derived-mode-p 'dired-mode)
     dired-directory)
    (t
     (let ((path (buffer-file-name)))
       (when path
         (f-dirname path)))))

   (if (eq system-type 'windows-nt)
       (w32-shell-execute "open" (convert-standard-filename it))
     (start-process "open" nil "open" (expand-file-name it)))))

(cl-defun rysco-eshell-new ()
 "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(cl-defun rysco-eshell-rename (name)
  "Open a new instance of eshell."
  (interactive "sName: ")
  (rename-buffer (format "*eshell <%s>*" name)))

(defun rysco-eshell-prompt ()
  (cl-loop
   with vc-backend = (when (fboundp 'vc-deduce-backend)
                       (vc-deduce-backend))
   with default-face = '(:height 0.9 :background "DarkSlateGray" :foreground "white" :box nil)
   with dir-face = '(:foreground "salmon" :background "gray22")
   with vc-face = `(:foreground "Gray47" :background "gray22" :height 0.8)
   with components = `("["
                       ,(format-time-string "%H:%M ")
                       (,(eshell/whoami) . (:inherit ,default-face :foreground "darkgoldenrod1" :slant italic :weight bold))
                       "@"
                       ,(system-name)
                       "]"
                       (" " . ,dir-face)
                       (,(--when-let vc-backend
                           (cond
                            ((equal it 'Hg)
                             (all-the-icons-faicon "mercury" :face vc-face))
                            ((equal it 'Git)
                             (all-the-icons-alltheicon "git" :face vc-face))))
                        . nil)
                       (,(when vc-backend " ") . ,dir-face)
                       (,(s-replace (expand-file-name "~") "~" (eshell/pwd)) . ,dir-face)
                       (" " . ,dir-face)
                       ("$" . ,dir-face)
                       (" " . nil))

                       for piece in components if (or
                                                   (stringp piece)
                                                   (and (listp piece) (car piece)))
                       concat
                       (cond
                        ((consp piece)
                         (--if-let (cdr piece)
                             (propertize (car piece) 'face it)
                           (car piece)))
                        (t
                         (propertize piece 'face default-face)))))

(cl-defstruct rysco-magit-origin host host-type path url)

(defun rysco-magit-get-origin (&optional origin-url)
  (let* ((origin-url (or origin-url (magit-get "remote" "origin" "url")))
         (parts (cdr (s-match "\\([^@]+\\)@\\([^:]+\\):\\(.*\\).git" origin-url)))
         (user (first parts))
         (host (nth 1 parts))
         (path (nth 2 parts)))
    (make-rysco-magit-origin
     :host host
     :host-type (pcase host
                  ("github.com" 'gh)
                  ("bitbucket.org" 'bb))
     :path path
     :url (format "https://%s/%s" host path))))

(defun rysco-magit-goto-compare (&optional head base)
  (interactive)
  (let* ((origin (rysco-magit-get-origin))
         (url (rysco-magit-origin-url origin))
         (source
          `(,(helm-build-sync-source "Comman Branches"
               :candidates '("master"))
            ,(helm-build-sync-source "Other Branches"
               :candidates
               (cl-loop for branch in (magit-list-remote-branch-names) collect
                        (substring branch (length "origin/"))))))
         (head (or head (helm :sources source)))
         (base (or base (when head (helm :sources source)))))
    (when (and head base)
      (browse-url
       (pcase (rysco-magit-origin-host-type origin)
         ('gh (format "%s/compare/%s...%s?expand=1" url base head))
         ('bb (format "%s/branches/compare/%s..%s" url base head)))))))

(defun rysco-magit-pull-request ()
  (interactive)
  (rysco-magit-goto-compare (magit-get-current-branch)))

(defun rysco-magit-goto-branch ()
  (interactive)
  (let* ((origin (rysco-magit-get-origin))
         (url (rysco-magit-origin-url origin)))

    (browse-url
     (pcase (rysco-magit-origin-host-type origin)
       ('gh (format "%s/tree/%s" url (magit-get-current-branch)))
       ('bb (format "%s/branch/%s" url (magit-get-current-branch)))))))

(defun rysco-magit-goto-origin ()
  (interactive)
  (browse-url
   (rysco-magit-origin-url
    (rysco-magit-get-origin))))

(defun rysco-export-org-to-pdf ()
  "Searches for a file named `.pdf-master' in ancestor directories and uses the file name specified inside of that file as the file to open and export using `org-latex-export-to-pdf'"
  (interactive)
  (--if-let (locate-dominating-file (buffer-file-name) ".pdf-master")
      (let ((path
             (concat
              it
              (with-temp-buffer
                (insert-file-contents (concat it ".pdf-master"))
                (s-trim
                 (buffer-string))))))
        (message "Found .pdf-master in %s.  Exporting." it)
        (with-current-buffer (find-file path)
          (let ((org-confirm-babel-evaluate nil))
            (org-latex-export-to-pdf))))
    (message "No .pdf-master found in directory ancestors.")))

(defun rysco-agenda-revert-files ()
  (interactive)
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-redo-all)))

;;;;;; Graphing utilities
(require 'graphviz-dot-mode)

(cl-defun rysco-simple-graph--generate-color (&optional rand-state)
  (apply
   'color-rgb-to-hex
   `(,@(color-hsl-to-rgb
        (cl-random 1.0 rand-state)
        (cl-random 1.0 rand-state)
        0.5)
     2)))

(defun rysco-simple-graph--layer-visible (all-layers obj-layer)
  (or (or (eq all-layers nil)
          (eq obj-layer nil))
      (-let [current-layers (split-string
                             (format "%s" obj-layer)
                             "[:]"
                             t
                             "[ ]+")]
        (--any? (cl-member it all-layers :test 'string-equal) current-layers))))

(cl-defun rysco-simple-graph--plist-to-settings (data &optional id color-cache rand-state layers)
  (loop
   with out
   with obj-layer = (plist-get data :layer)
   with visible = (rysco-simple-graph--layer-visible layers obj-layer)

   for k in data by 'cddr
   as k = (format "%s" k)
   for v in (cdr data) by 'cddr
   as v = (pcase v
            ('RCOL (rysco-simple-graph--generate-color rand-state))
            ('UCOL
             (when color-cache
               (--if-let (gethash id color-cache)
                   it
                 (puthash
                  id
                  (setq color
                        (rysco-simple-graph--generate-color rand-state))
                  color-cache))))
            (_ v))

   when (and k v)
   concat
   (format "%s=\"%s\"," (substring k 1) v)
   into out

   finally return
   (concat
    out
    (when (not visible)
      "fontcolor=\"#FF000000\", bgcolor=\"#FF000000\", color=\"#FF000000\""))))

(defun rysco-simple-graph--properties (data &optional layers)
  (loop
   with out
   with obj-layer = (plist-get data :layer)
   with visible = (rysco-simple-graph--layer-visible layers obj-layer)

   for (k v) on data by 'cddr
   as k = (format "%s" k)
   as k = (if (equal (substring k 0 1) ":")
              (substring k 1)
            k)
   concat
   (format "%s=%s;\n" k (prin1-to-string v))
   into out

   finally return
   (concat
    out
    (when (not visible)
      "fontcolor=\"#FF000000\"; bgcolor=\"#FF000000\"; color=\"#FF000000\";\n"))))

(cl-defun rysco-simple-graph--nodes (patch &key path name subgraph prefix properties rand-state color-cache layers)
  (loop
   with entry-prefix = (if path (format "%s_" path) "")

   for entry in patch do
   (pcase entry
     (`(,(and (or :group :cluster) type) ,(and (or (pred stringp) (pred symbolp)) name) . ,group-data)
      (insert (format "subgraph cluster_%s%s {\n" entry-prefix name))

      (rysco-simple-graph--nodes
       group-data
       :name name
       :path (if (eq type :cluster)
                 (if path (format "%s_%s" path name) name)
               path)
       :subgraph t
       :prefix (eq type :cluster)
       :layers layers)

      (insert (format "}\n")))

     (`(:properties . ,property-data)
      (insert
       (rysco-simple-graph--properties property-data layers)))

     (`(,(and (or (pred stringp) (pred symbolp)) mod-name) . ,_)
      (insert (format "\"%s%s\";\n" entry-prefix mod-name)))

     (`((,(and (or (pred stringp) (pred symbolp)) mod-name) . ,data) . ,_)
      (insert
       (format
        "\"%s%s\" [%s];\n"
        entry-prefix
        mod-name
        (rysco-simple-graph--plist-to-settings
         data
         mod-name
         color-cache
         rand-state
         layers)))))))

(cl-defun rysco-simple-graph--guess-filename (&optional ext)
  (when (boundp 'out)
    out)
  (when (equal major-mode 'org-mode)
    (concat
     (replace-regexp-in-string
      "[/\\:]" "-"
      (s-join "-" (org-get-outline-path t)))
     (or ext ".png"))))

(cl-defun rysco-simple-graph (patch &key filename graph-code rand-seed layers as-code)
  "Calls Graphviz and generates a graph from the provided, simplified graph format.

More Graphviz Dot formatting information at URL `https://graphviz.org/doc/info/attrs.html'

FILENAME will be used to set the output filename and return that as a string.
GRAPH-CODE is a string that will be inserted verbatim into the generated Graphviz Dot code after the default values.

RAND-SEED is the random seed used for color generation.  Specifying this will cause the colors used for any stable graph to be the same.

PATCH is the graph data in the following form.

Nodes and connections can be specified as (node CONNECTION1 CONNECTION2 ... CONNECTION3)
Nodes can be a symbol, string, or a list of the form (ID KEY1 VALUE1 ... KEYN VALUEN)
Node data is specified as a plist.
Connections can be a symbol, a string, or a list of form (DEST LABEL . PROPERTIES)
PROPERTIES is an optional plist of keys/values that are set for the connection entry in the resultant dot output.

Nodes can be grouped into subgraphs using an entry (:group NAME ...)

Dot settings for the current graph can be specified using an entry (:properties KEY1 VALUE1 ... KEYN VALUEN)

Object layers can be set using (... :layer NAME) where NAME is a symbol, string, or number.
NAME can also specify multiple layers as a string with layer names separated by `?:'.
When LAYERS is provided, any objects not specified as being on at least one of the active layers will be drawn transparently such that it is not seen, but the graph layout is stable.

If LAYERS is omitted all layers are drawn.
Objects with no layer specified are always drawn.

Some common properties:
          rankdir=LR
          rank=source
          concentrate=true
          size = \"13\"
          label = \"Test Label\"
          fontname = \"Arial\"
          fontsize = 30.0
          fontcolor = \"#3b3b3b\"
          color = \"#aaaaaa\"
          fillcolor = \"#B0e0e6\"
          style = filled
          style = bold
          penwidth = 0
          weight = 100


Example:
  (rysco-simple-graph
   '(
     (:properties
      label \"Graph Title\"
      labeljust l
      labelloc t
      rankdir LR
      size 13
      concentrate true)

     (other)
     (a)
     (b)
     (:group \"platform\"
             (:properties
              label \"Platform Label\"
              style filled
              penwidth 0
              fillcolor \"#B0e0e6\")

             (tc)
             (server))

     (other
      (a \"tc\")
      (b \"tc\"))

     (a
      (tc \"label\"))

     (tc
      (b \"other label\"))

     (b
      (server \"3rd label\"))

     (server
      (b \"Denied\"))))"

  (-let* ((temp-path (make-temp-file "patch" nil ".dot"))
          (color-cache (make-hash-table :test 'equal))
          (rand-state (cl-make-random-state rand-seed))
          (layers (when layers
                    (-flatten
                    (--map
                     (split-string (format "%s" it) "[:]" t "[ ]+")
                     (if (listp layers)
                         layers
                       `(,layers))))))
          (filename (or filename
                        (rysco-simple-graph--guess-filename)))
          (out-path (format "%s.png" (file-name-sans-extension temp-path)))
          out-code)

    (with-temp-buffer
      (insert "digraph patch {\n"
              "\nnode  [style=\"rounded,filled,bold\", shape=box, fixedsize=true, width=1.3, fontname=\"Arial\"];\n"
              (or graph-code "")
              "\n")

      ;; Insert nodes
      (rysco-simple-graph--nodes
       patch
       :color-cache color-cache
       :rand-state rand-state
       :layers layers)

      ;; Insert connections
      (cl-loop
       for entry in patch
       when (and (listp entry)
                 (not (memq (car entry) '(:group :cluster :properties))))
       for (mod . connections) in patch
       as forward = t
       as mod = (if (listp mod)
                    (car mod)
                  mod)

       do
       (cl-loop
        for dest in connections do
        (insert
         (pcase dest
           ('> (setq forward t) "#")
           ('< (setq forward nil) "#")
           (`(,dest ,comment . ,settings)
            (let ((color (gethash comment color-cache)))
              (unless color
                (setq color
                      (or (plist-get settings :color)
                          (rysco-simple-graph--generate-color rand-state)))
                (puthash comment color color-cache))
              (format
               "\"%s\" -> \"%s\" [label=\"%s\", color=\"%s\", fontcolor=\"%s\",%s]"
               (if forward mod dest)
               (if forward dest mod)
               comment color color
               (or
                (rysco-simple-graph--plist-to-settings
                 settings comment color-cache rand-state layers)
                ""))))
           (dest
            (format
             "\"%s\" -> \"%s\""
             (if forward mod dest)
             (if forward dest mod))))
         ";\n")))

      (insert "\n}\n")

      (if as-code
          (setq out-code (buffer-string))
        (write-file temp-path)))

    (if as-code
        out-code
      (-let ((command-result (string-trim
                              (shell-command-to-string
                               (graphviz-compile-command temp-path)))))
        (if (string-prefix-p "Error:" command-result)
            (message command-result)

          (delete-file temp-path)

          (rename-file out-path filename t)
          filename)))))

(cl-defun rysco-simple-graph-animated (patch frames &key filename graph-code rand-seed delay loop debug)
  "Generates an animated gif from the provided graph.
See `rysco-simple-graph' for more information.

FRAMES is a list of all layers to render in order.
DELAY is the delay between frames (passed to Imagemagick).
LOOP is the number of loop cycles (passed to Imagemagick).
DEBUG set to non-nil will create a single frame gif with all of the specified layers present.  Useful for seeing all of the objects at once.
"
  (loop
   with filename = (or filename (rysco-simple-graph--guess-filename ".gif"))
   with delay = (or delay 100)
   with loop = (or loop 0)
   with frames = (if debug
                     (list (s-join ":" (--map (format "%s" it) frames)))
                   frames)

   with frame-list

   for layer in frames
   as frame-name = (make-temp-file "patch" nil ".png")

   collect
   (rysco-simple-graph patch
                       :filename frame-name
                       :graph-code graph-code
                       :rand-seed rand-seed
                       :layers layer)
   into frame-list

   finally return
   (let* ((cmd (format
                "%s -delay %s -loop %s %s \"%s\""
                rysco-imagemagick-executable delay loop
                (apply 'concat (--map (format "\"%s\" " it) frame-list))
                filename))

          (command-result (string-trim (shell-command-to-string cmd))))

     (loop for temp in frame-list do
           (delete-file temp))

     filename)))

(cl-defun rysco-graph--fan (data anchor connection-properties)
  (loop
   with connections
   with heads
   for entry in data
   as skip = (pcase entry
               ((pred vectorp)
                (setq connection-properties (append entry nil))))

   unless skip
   collect entry into heads

   unless skip
   append (loop
           for a in anchor collect
           `(,a ,(if connection-properties
                     (cons entry connection-properties)
                   entry)))
   into connections

   finally return
   (cons heads connections)))

(cl-defun rysco-graph--chain (data &optional anchor)
  (loop
   with anchor = anchor
   with connection-properties

   for it in data
   as out = nil
   do (pcase it
        (:back (pop anchor))
        (:break (setq anchor nil))
        ((pred vectorp) (setq connection-properties (append it nil)))
        (`(:fan . ,rest)
         (-let [(top . conns) (rysco-graph--fan rest (car anchor) connection-properties)]
           (push top anchor)
           (setq out conns
                 connection-properties nil)))
        (_ (when anchor
             (setq out
                   (loop
                    for a in (car anchor) collect
                    `(,a ,(if connection-properties
                              (cons it connection-properties)
                            it)))
                   connection-properties nil))
           (push `(,it) anchor)))

   if out append out))

(cl-defun rysco-graph--process (forms)
  "Returns a plist of data from processing graph data in FORMS"
  (loop
    for f in forms append
    (pcase f
      (`(:chain . ,rest) (rysco-graph--chain rest))
      (_ `(,f)))))

(cl-defmacro rysco-graph (args &rest forms)
  "Macro that provides a more structured syntax on top of 'rysco-simple-graph.
ARGS is a plist that will be passed to 'rysco-simple-graph as &key parameters.
FORMS is a list of graphing specifications."

  `(rysco-simple-graph
    '
    ,(rysco-graph--process forms)
    ,@args))

(defun rysco-add-hunspell-dictionaries ()
  (interactive)
  (let ((dictionary-dir (f-join user-emacs-directory "dictionaries")))
    (when (and (eq system-type 'windows-nt)
               (f-exists? dictionary-dir))

      (setenv "DICPATH" (s-replace "/" "\\" dictionary-dir))

      (unless (boundp 'ispell-hunspell-dict-paths-alist)
        (setq ispell-hunspell-dict-paths-alist nil))

      (loop
       for path in (f-entries dictionary-dir)
       if (f-ext? path "aff") do
       (add-to-list 'ispell-hunspell-dict-paths-alist
                    `(,(f-base path) ,path))))))


(defun rysco-download-hunspell-dictionaries ()
  (interactive)
  (when (eq system-type 'windows-nt)
    (let ((base-dir (f-join user-emacs-directory "dictionaries"))
          (dictionaries
           '(("en_US.aff" "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff?id=a4473e06b56bfe35187e302754f6baaa8d75e54f")
             ("en_US.dic" "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic?id=a4473e06b56bfe35187e302754f6baaa8d75e54f"))))

      (loop
       initially do
       (f-mkdir base-dir)

       for (file url) in dictionaries do
       (url-copy-file
        url (expand-file-name file base-dir) t)))))

;;
(provide 'rysco-util)
