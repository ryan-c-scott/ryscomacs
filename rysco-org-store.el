(defvar rysco-org-store-last-entry-name "")
(defvar rysco-org-store-templates nil)
(defvar rysco-org-store-directory (concat org-directory "store"))

(defun rysco-org-store-load-templates (&optional dir)
  (interactive)
  (let ((dir (or dir (expand-file-name (concat rysco-org-store-directory "/templates")))))
    (cl-loop
     for (key nice-name template) in rysco-org-store-templates do
     (add-to-list
      'org-capture-templates
      `(,key ,(concat "[store] " nice-name)
        entry
        (function rysco-org-store-location)
        (file ,(expand-file-name template dir))
        :create-id t)))))

(defun rysco-org-store-location ()
  ;; TODO: Verify that file doesn't exist
  ;; .Somehow abort capture but visit file?
  ;; .Need to properly support aborting process
  (find-file
   (expand-file-name
    (format "%s/%s-%s.org"
            rysco-org-store-directory
            (format-time-string "%Y%m%d")
            (s-replace "/" "-"
                       (setq rysco-org-store-last-entry-name
                             (read-string "Title: ")))))))

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

(defun helm-rysco-org-store-query ()
  (interactive)
  (with-store-directory
   (let ((helm-org-ql-actions
          (append
           helm-org-ql-actions
           '(("Insert as link" . helm-rysco-org-store--insert-candidates)))))
     (funcall-interactively 'helm-org-ql (org-ql-search-directories-files)
                            :name "Knowledge Store Query"))))

(defun helm-rysco-org-store--insert-candidates (&optional _)
  (loop
   for candidate in (helm-marked-candidates :all-sources t) do
     (insert
      (rysco-org-store--with-buffer-at-marker candidate
        (org-link-make-string
         (format "id:%s" (org-id-get-create))
         (org-display-outline-path nil t nil t)))
      "\n")))

(defun rysco-org-store-capture-create-id ()
  (when (org-capture-get :create-id)
    (org-id-get-create)))

(add-hook 'org-capture-mode-hook #'rysco-org-store-capture-create-id)

(provide 'rysco-org-store)
