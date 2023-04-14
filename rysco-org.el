(require 'org-agenda)
(require 'helm)

(defvar rysco-org-effective-time-override nil)

(defvar rysco-org-refile-targets nil)
(defvar rysco-org-agenda-status-overlay nil)
(defvar rysco-org-agenda-status-count-code-base #x278a) ; ➊

(defvar rysco-org-agenda-columns 3)
(defvar rysco-org-agenda-margin-col 2)
(defvar rysco-org-agenda-margin-left 2)
(defvar rysco-org-agenda-excess-threshold 4)

(defface rysco-org-agenda-status-title
  '((t :underline "grey20"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-project
  '((t :inherit 'rysco-org-agenda-status-title
       :foreground "grey40"
       :slant italic))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-base
  '((t :inherit 'rysco-org-agenda-status-title
       :slant italic
       :height 0.75
       :foreground "#54ff9f"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-active
  '((t :inherit 'rysco-org-agenda-status-base
       :foreground "gray80"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-excess
  '((t :inherit 'rysco-org-agenda-status-base
       :foreground "yellow"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-blocked
  '((t :inherit 'rysco-org-agenda-status-base
       :foreground  "#F92672"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-stalled
  '((t :inherit 'rysco-org-agenda-status-base
       :weight bold
       :foreground "DarkTurquoise"))
  ""
  :group 'rysco-org-agenda-faces)

(set-face-attribute 'org-agenda-clocking nil :box "cyan4")

(defun helm-rysco-org-agenda-buffer-items (&optional arg)
  (interactive "P")
  (--when-let
      (and (derived-mode-p 'org-agenda-mode)
           (save-excursion
             (goto-char (point-min))
             (cl-loop
              with action = `(("Go to item" . (lambda (pos) (goto-char pos)))
                              ("Go to heading" . (lambda (pos)
                                                   (goto-char pos)
                                                   (org-agenda-goto))))

              do (org-agenda-forward-block)
              as heading = (s-replace "\n" "" (thing-at-point 'line))
              do (forward-line 1)
              while (< (point) (point-max))
              as end = (save-excursion
                         (org-agenda-forward-block)
                         (if (= (point) (point-max))
                             (point-max)
                           (forward-line -1)
                           (point)))

              collect
              (helm-build-sync-source heading
                :candidates
                (save-excursion
                  (cl-loop
                   while (< (point) end)
                   collect
                   (cons
                    (s-replace "\n" "" (thing-at-point 'line))
                    (point))
                   do (forward-line 1)))

                :action action))))

    (or
     (helm :sources it)
     t)))

(defun rysco-org-agenda ()
  (interactive)
  (with-try-switch-existing-buffer "*Org Agenda*"
    (org-agenda)))

(defun rysco-org-agenda-get-projects ()
  "Return status of all projects, as specified by the org property `projectid' listed in the buffer"
  (interactive)
  (--when-let
      (and (derived-mode-p 'org-agenda-mode)
           (save-excursion
             (goto-char (point-min))
             (org-agenda-forward-block)
             (forward-line 1)

             (cl-loop
              with status = (make-hash-table :test 'equal)
              with next-count = (make-hash-table :test 'equal)

              until (eobp)
              as marker = (org-get-at-bol 'org-marker)
              when marker do
              (let* ((project (org-entry-get marker "PROJECTID" t))
                     (todo (substring-no-properties
                            (org-get-at-bol 'todo-state)))
                     (state (gethash project status))
                     (count (or (gethash project next-count) 0)))


                (when (or (equal todo "NOW")
                          (equal todo "NEXT"))
                  (cl-incf count)
                  (puthash project count next-count))

                (unless (or (equal state 'ACTIVE)
                            (equal state 'EXCESS))
                  (setq state
                        (puthash
                         project
                         (pcase todo
                           ((or "NOW" "NEXT") 'ACTIVE)
                           ("WAITING" 'BLOCKED))
                         status)))

                (when (and (equal state 'ACTIVE)
                           (or (equal todo "NOW")
                               (equal todo "NEXT")))

                  (when (> count rysco-org-agenda-excess-threshold)
                    (puthash project 'EXCESS status))))

              do (forward-line 1)
              finally return
              (cl-loop
               for k being the hash-keys of status
               collect
               `(,k ,(gethash k status) ,(or (gethash k next-count) 0))))))
    it))

(defun rysco-org-agenda-goto-first-section ()
  (interactive)
  (goto-char (point-min))
  (org-agenda-next-item 1))

(defun rysco-org-agenda-find-first-todo (state)
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     with found
     until (or found (eobp))
     as todo-state = (org-get-at-bol 'todo-state)
     do (if (equal todo-state state )
            (setq found (point))
          (forward-line))
     finally return found)))

(defun rysco-org-agenda--status-face (status)
  (pcase status
    ('ACTIVE 'rysco-org-agenda-status-active)
    ('BLOCKED 'rysco-org-agenda-status-blocked)
    ('EXCESS 'rysco-org-agenda-status-excess)
    (_ 'rysco-org-agenda-status-stalled)))

(defun rysco-org-agenda--status-string (status count)
  (if status
      (format
       "%s %s"
       (make-string 1 (+ rysco-org-agenda-status-count-code-base (1- count)))
       status)
    "STALLED"))

(defun rysco-org-agenda--status-entry (project status count)
  (concat
   (propertize (format "%-10s"
                       (rysco-org-agenda--status-string status count))
               'face (rysco-org-agenda--status-face status))
   (propertize (format "%-12s" project)
               'face 'rysco-org-agenda-status-project)))

(defun rysco-org-agenda-insert-status (&rest _)
  (interactive)
  (save-excursion
    (when rysco-org-agenda-status-overlay
      (setq rysco-org-agenda-status-overlay
            (delete-overlay rysco-org-agenda-status-overlay)))

    (rysco-org-agenda-goto-first-section)
    (forward-line -2)

    (unless org-agenda-current-span
      (setq rysco-org-agenda-status-overlay
            (make-overlay (point) (+ (point) 2)))

      (let* ((show-status (= (or (get-char-property 1 'org-last-args) 0) 0))
             (status (and show-status (rysco-org-agenda-get-projects)))
             (buffer-read-only nil)
             (status-overlay rysco-org-agenda-status-overlay)
             (col-count rysco-org-agenda-columns)
             (margin-col rysco-org-agenda-margin-col)
             (margin-col-str (make-string margin-col ?\s))
             (margin-left rysco-org-agenda-margin-left)
             (margin-left-str (concat "\n" (make-string margin-left ?\s)))
             (margin-right-str ""))

        (overlay-put rysco-org-agenda-status-overlay 'invisible t)
        (overlay-put rysco-org-agenda-status-overlay 'display
                     'rysco-org-agenda-status-title)

        (overlay-put
         rysco-org-agenda-status-overlay 'before-string
         (concat
          (when show-status
            (cl-loop
             with i = 0
             for (k state count) in status
             as col = (% i col-count)

             when k do (cl-incf i)

             when k concat
             (if (= col 0)
                 margin-left-str
               margin-col-str)

             when k concat
             (rysco-org-agenda--status-entry k state count)

             when (= col (1- col-count)) concat margin-right-str))

          "\n\n"))))))

;;;###autoload
(defun rysco-agenda-project-header (str)
  (-if-let* ((marker (get-text-property 0 'org-marker str))
             (face-name (org-entry-get marker "projectface" t))
             (face (intern face-name))
             (header-width (-if-let (header-text (car (s-match "^.*:\s+" str)))
                               (1- (length header-text))
                             (+ 3 (length (format "%s" (get-text-property 0 'org-category str)))))))
      (prog1 str
        (add-text-properties 0 header-width `(face ,face) str))
    str))

(defun rysco-agenda-refile-wrapper (old &rest args)
  (let ((org-refile-targets (or rysco-org-refile-targets org-refile-targets)))
    (apply old args)))

(defun rysco-org-get-path-string ()
  (s-replace
   "\n" ""
   (base64-encode-string
    (org-format-outline-path (org-get-outline-path t t) nil nil "-"))))

(defun rysco-org-insert-path-string ()
  (interactive)
  (insert (rysco-org-get-path-string)))

(defun rysco-org-element-value-to-kill-ring ()
  (interactive)
  (--when-let (and (derived-mode-p 'org-mode)
                   (org-element-property :value (org-element-at-point)))
    (kill-new it)))

(defun rysco-org-src-execute (&rest _)
  (interactive)
  (let ((block-point org-src--beg-marker))
    (with-current-buffer (org-src-source-buffer)
      (save-excursion
        (goto-char block-point)
        (call-interactively 'org-ctrl-c-ctrl-c)))))

(defmacro with-rysco-org-result-src-block (&rest forms)
  `(save-excursion
     (when-let* ((current (point))
                 (src-start (let ((case-fold-search t))
                                  (save-excursion
                                    (re-search-backward
                                     (rx (seq
                                          line-start
                                          (* whitespace)
                                          "#+"
                                          (or "BEGIN_SRC" "CALL:")))
                                     nil t))))
                 (_ (goto-char src-start))
                 (result-start (org-babel-where-is-src-block-result))
                 (_ (goto-char result-start))
                 (_ (forward-line))
                 (result-end (org-babel-result-end)))
       (when (and (>= current result-start)
                  (<= current result-end))
         (goto-char src-start)
         ,@forms))))

;;;###autoload
(defun rysco-org-result-execute-src ()
  (interactive)
  (let ((current (point)))
    (when (with-rysco-org-result-src-block
           (let* ((context
	           (org-element-lineage
	            (org-element-context)
	            '(babel-call inline-babel-call inline-src-block)
	            t))
                  (type (org-element-type context)))
             (pcase type
               ((or `babel-call `inline-babel-call)
                (let ((info (org-babel-lob-get-info context)))
	          (when info (org-babel-execute-src-block nil info nil type))))
               (_
                (org-babel-execute-src-block)))))
      (goto-char current))))

;;;###autoload
(defun rysco-org-result-edit-src ()
  (interactive)
  (with-rysco-org-result-src-block
   (org-edit-special)))

(add-hook 'org-ctrl-c-ctrl-c-final-hook 'rysco-org-result-execute-src)

;;;###autoload
(cl-defun rysco-org-process-date-log (data windows &key value-column degrade)
  (cl-loop
   with rolling = (--map (rysco-rolling-average it) windows)

   with dates = (--map (cons (format-time-string
                              "%F" (org-read-date nil t (car it))) (cdr it)) data)
   with today-stamp = (format-time-string "%F")
   with last-stamp = (if (string= today-stamp (car (-last-item dates)))
                         today-stamp
                       (format-time-string
                        "%F"
                        (org-read-date nil t "--1" nil)))
   with first = (org-read-date nil t (caar dates))
   with last = (org-read-date nil t "++1" nil
                              (org-read-date
                               nil t
                               last-stamp))

   with carried = 0

   for i from 0
   as this-date = (org-read-date nil t (format "++%s" i) nil first)
   as this-date-string = (format-time-string "%F" this-date)

   while (time-less-p this-date last)
   as entry-data = (cl-assoc this-date-string dates :test 'string=)
   as this-value = (or (if value-column
                           (nth value-column entry-data)
                         (when entry-data 1.0))
                       carried)

   collect
   `(,this-date-string
     ,this-value
     ,@(--map (funcall it this-value) rolling))
   do (when degrade
        (setf carried (max 0 (* this-value (- 1 degrade)))))))

(defun rysco-org-agenda-post-clock-in (&optional _)
  (org-agenda-redo-all)
  (--if-let (rysco-org-agenda-find-first-todo "NOW")
      (goto-char it)
    (rysco-org-agenda-goto-first-section)))

(advice-add #'org-agenda-redo-all :after 'rysco-org-agenda-insert-status)
(advice-add #'org-agenda-redo :after 'rysco-org-agenda-insert-status)
(advice-add #'org-agenda-todo :after 'org-agenda-redo-all)
(advice-add #'org-agenda-clock-in :after 'rysco-org-agenda-post-clock-in)
(advice-add #'org-agenda-clock-out :after 'org-agenda-redo-all)
(advice-add #'org-todo-list :after 'rysco-org-agenda-insert-status)

(advice-add 'org-agenda-refile :around 'rysco-agenda-refile-wrapper)

(advice-add 'org-edit-src-save :after 'rysco-org-src-execute)

(defun rysco-org-clock-heading ()
  (or
   (org-entry-get (point) "PROJECTID" t)
   ""))

(setq org-clock-heading-function 'rysco-org-clock-heading)

(defun rysco-org-agenda-entry-text-show-here ()
  "Add logbook from the entry as context to the current line."
  (let (m txt o)
    (setq m (org-get-at-bol 'org-hd-marker))
    (unless (marker-buffer m)
      (error "No marker points to an entry here"))
    (setq txt
          (concat
           "\n"
           (save-excursion
             (with-current-buffer (marker-buffer m)
               (org-with-wide-buffer
                (goto-char m)
                (-when-let* ((el (org-element-at-point))
                             (begin (org-element-property :contents-begin el))
                             (end (org-element-property :contents-end el))
                             (content (buffer-substring begin end))
                             (log (with-temp-buffer
                                    (insert content)
                                    (org-element-map (org-element-parse-buffer) 'drawer
                                      (lambda (drawer)
                                        (when (string= "LOGBOOK" (org-element-property :drawer-name drawer))
                                          (let ((line-prefix "\t")
                                                entries)
                                            (org-element-map drawer org-element-all-elements
                                              (lambda (el)
                                                (--when-let
                                                    (pcase (car el)
                                                      ('clock)
                                                      ('plain-list
                                                       (concat
                                                        line-prefix
                                                        (s-replace
                                                         "\n" (concat "\n" line-prefix)
                                                       (buffer-substring
                                                        (org-element-property :contents-begin el)
                                                        (org-element-property :contents-end el))))))
                                                  (push it entries))))
                                            (apply 'concat (reverse entries)))))
                                      nil t))))
                  log))))))

    (when (string-match "\\S-" txt)
      (setq o (make-overlay (point-at-bol) (point-at-eol)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'org-overlay-type 'agenda-entry-content)
      (overlay-put o 'after-string txt))))

(advice-add 'org-agenda-entry-text-show-here :override 'rysco-org-agenda-entry-text-show-here)

(defun rysco-org-current-effective-time-advice (func &rest rest)
  (if rysco-org-effective-time-override
      rysco-org-effective-time-override
    (apply func rest)))

(advice-add 'org-current-effective-time :around 'rysco-org-current-effective-time-advice)

(defun rysco-org-todo-on-date (arg)
  (interactive "P")
  (let ((rysco-org-effective-time-override (org-read-date nil t)))
    (pcase major-mode
      ('org-mode (org-todo arg))
      ('org-agenda-mode (org-agenda-todo arg)))))

(defun rysco-org-todo-yesterday (arg)
  (interactive "P")
  (let ((rysco-org-effective-time-override (org-read-date nil t "-1")))
    (pcase major-mode
      ('org-mode (org-todo arg))
      ('org-agenda-mode (org-agenda-todo arg)))))

;; Embedded images in HTML export as new backend
;; NOTE: From https://niklasfasching.de/posts/org-html-export-inline-images/
(defun rysco-org-html-export-to-embedded-html (&optional async subtree visible body)
  (cl-letf (((symbol-function 'org-html--format-image) 'rysco-format-image-inline))
    (org-html-export-to-html nil subtree visible body)))

(defun rysco-format-image-inline (source attributes info)
  (let* ((ext (file-name-extension source))
         (prefix (if (string= "svg" ext)
                     "data:image/svg+xml;base64,"
                   "data:;base64,"))
         (data (with-temp-buffer
                 (url-insert-file-contents source)
                 (buffer-string)))
         (data-url (concat prefix (base64-encode-string data)))
         (attributes (org-combine-plists `(:src ,data-url) attributes)))
    (org-html-close-tag "img" (org-html--make-attribute-string attributes) info)))

(with-eval-after-load 'ox
  (org-export-define-derived-backend 'html-inline-images 'html
    :menu-entry
    '(?h "Export to HTML"
         ((?e "As MHTML file" rysco-org-html-export-to-embedded-html)))))


(defun rysco-org-add-triage-tag ()
  (interactive)
  (org-toggle-tag "triage"))

(defun rysco-org-agenda-add-triage-tag ()
  (interactive)
    (--when-let (org-get-at-bol 'org-marker)
      (with-current-buffer (marker-buffer it)
        (goto-char (marker-position it))
        (rysco-org-add-triage-tag))))

;;
(provide 'rysco-org)
