(require 'org-agenda)
(require 'helm)

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

                  (incf count)
                  (puthash project count next-count)

                  (when (> count rysco-org-agenda-excess-threshold)
                    (puthash project 'EXCESS status))))

              do (forward-line 1)
              finally return
              (loop
               for k being the hash-keys of status
               collect
               `(,k ,(gethash k status) ,(or (gethash k next-count) 0))))))
    it))

(defun rysco-org-agenda-goto-first-section ()
  (interactive)
  (goto-char (point-min))
  (org-agenda-next-item 1))

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
    (rysco-org-agenda-goto-first-section)
    (forward-line -2)

    (when rysco-org-agenda-status-overlay
      (setq rysco-org-agenda-status-overlay
            (delete-overlay rysco-org-agenda-status-overlay)))

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
            (loop
             with i = 0
             for (k state count) in status
             as col = (% i col-count)

             when k do (incf i)

             when k concat
             (if (= col 0)
                 margin-left-str
               margin-col-str)

             when k concat
             (rysco-org-agenda--status-entry k state count)

             when (= col (1- col-count)) concat margin-right-str))

          "\n\n")))))

;;;###autoload
(defun rysco-agenda-project-header (str)
  (-if-let* ((marker (get-text-property 0 'org-marker str))
             (face-name (org-entry-get marker "projectface" t))
             (face (intern face-name))
             (header-width (length (car (s-match "^.*:\s+" str)))))
      (prog1 str
        (add-text-properties 0 (1- header-width) `(face ,face) str))
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

(defun rysco-org-src-execute (&rest _)
  (interactive)
  (let ((block-point org-src--beg-marker))
    (with-current-buffer (org-src-source-buffer)
      (save-excursion
        (goto-char block-point)
        (call-interactively 'org-ctrl-c-ctrl-c)))))

(advice-add #'org-agenda-redo-all :after 'rysco-org-agenda-insert-status)
(advice-add #'org-agenda-redo :after 'rysco-org-agenda-insert-status)
(advice-add #'org-todo-list :after 'rysco-org-agenda-insert-status)

(advice-add 'org-agenda-refile :around 'rysco-agenda-refile-wrapper)

(advice-add 'org-edit-src-save :after 'rysco-org-src-execute)

(defun rysco-org-clock-heading ()
  (or
   (org-entry-get (point) "PROJECTID" t)
   ""))

(setq org-clock-heading-function 'rysco-org-clock-heading)

;;
(provide 'rysco-org)
