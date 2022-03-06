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
         (expand-file-name
          (format "%s/%s-%s.org"
                  rysco-store-directory
                  (format-time-string "%Y%m%d")
                  (s-replace "/" "-"
                             (setq rysco-store-last-entry-name
                                   (read-string "Confirm: " title))))))))))

(defmacro with-store-directory (&rest forms)
  "Lexically binds `org-directory' to `rysco-store-directory' and executes FORMS"
  `(let ((org-directory rysco-store-directory))
     ,@forms))

(defmacro rysco-store--with-buffer-at-marker (marker &rest body)
  (declare (indent defun) (debug (form body)))
  `(progn
     (with-current-buffer (marker-buffer ,marker)
       (save-excursion
         (goto-char ,marker)
         ,@body))))

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

(provide 'rysco-org-store)
