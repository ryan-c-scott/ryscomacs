(defvar rysco-org-store-last-entry-name "")
(defvar rysco-org-store-templates nil)
(defvar rysco-org-store-templates--processed nil)
(defvar rysco-org-store-directory (concat org-directory "store"))

(defvar-local rysco-org-store-insert-after-capture nil)

(defun rysco-org-store-load-templates (&optional dir)
  (interactive)
  (let ((dir (or dir (expand-file-name (concat rysco-org-store-directory "/templates")))))
    (cl-loop
     for (key nice-name template) in rysco-org-store-templates
     as capture-template = `(,key ,(concat "[store] " nice-name)
                                  entry
                                  (function rysco-org-store-location)
                                  (file ,(expand-file-name template dir))
                                  :create-id t)
     do
     (add-to-list 'org-capture-templates capture-template)
     (add-to-list 'rysco-org-store-templates--processed capture-template))))

(defun rysco-org-store-location ()
  (let* (existing-node
         (title (funcall-interactively
                 'helm-rysco-org-store-ql
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
         (expand-file-name
          (format "%s/%s-%s.org"
                  rysco-org-store-directory
                  (format-time-string "%Y%m%d")
                  (s-replace "/" "-"
                             (setq rysco-org-store-last-entry-name
                                   (read-string "Confirm: " title))))))))))

(defmacro with-store-directory (&rest forms)
  "Lexically binds `org-directory' to `rysco-org-store-directory' and executes FORMS"
  `(let ((org-directory rysco-org-store-directory))
     ,@forms))

(defmacro rysco-org-store--with-buffer-at-marker (marker &rest body)
  (declare (indent defun) (debug (form body)))
  `(progn
     (with-current-buffer (marker-buffer ,marker)
       (save-excursion
         (goto-char ,marker)
         ,@body))))

(defun rysco-org-store-backlinks ()
  (interactive)
  (with-store-directory
   (funcall-interactively 'org-sidebar-backlinks)))

(defun rysco-org-store-query ()
  (interactive)
  (with-store-directory
   (funcall-interactively 'org-sidebar-ql)))

(defun rysco-org-store-create-and-insert ()
  (interactive)
  (setq rysco-org-store-insert-after-capture t)
  (let ((org-capture-templates rysco-org-store-templates--processed))
    (org-capture)))

(defun helm-rysco-org-store-query ()
  (interactive)
  (funcall-interactively
   'helm-rysco-org-store-ql
   :name "Knowledge Store Query"
   :actions `(,@helm-org-ql-actions
              ("Insert as link" . helm-rysco-org-store--insert-candidates))))

(cl-defun helm-rysco-org-store-ql (&key buffers-files (boolean 'and) (name "helm-org-ql") sources actions)
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

(defun helm-rysco-org-store--insert-candidates (&optional _)
  (rysco-org-store--insert-links (helm-marked-candidates :all-sources t)))

(defun rysco-org-store--insert-links (markers)
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
     (insert (rysco-org-store-get-marker-link this-link) sep))))

(defun rysco-org-store-get-marker-link (marker &optional link-text)
  (rysco-org-store--with-buffer-at-marker marker
    (org-link-make-string
     (format "id:%s" (org-id-get-create))
     (or link-text (org-display-outline-path nil t nil t)))))

(defun rysco-org-store-capture-create-id ()
  (when (org-capture-get :create-id)
    (org-id-get-create)))

(defun rysco-org-store-post-capture ()
  (when (and rysco-org-store-insert-after-capture (not org-note-abort))
    (setq rysco-org-store-insert-after-capture nil)
    (rysco-org-store--insert-links `(,org-capture-last-stored-marker))))

(add-hook 'org-capture-mode-hook #'rysco-org-store-capture-create-id)
(add-hook 'org-capture-after-finalize-hook 'rysco-org-store-post-capture)

(provide 'rysco-org-store)
