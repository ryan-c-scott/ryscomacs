(require 'dash)
(require 'eglot)
(require 'helm)

(defun rysco-imenu-eglot-items (&optional alist path results)
  (let* ((alist (or alist (eglot-imenu))))
    (dolist (entry alist)
      (let* ((type (get-text-property 0 'imenu-kind (car entry)))
             (type-name (upcase (or
                                 (cdr (assoc type '(("Class" . "STRUCT")
                                                    ("Method" . "FUNC")
                                                    ("Function" . "FUNC"))))
                                 type)))
             (type-face (or (cdr (assoc type-name '(("STRUCT" . font-lock-type-face)
                                                    ("ENUM" . font-lock-type-face)
                                                    ("FUNC" . font-lock-function-name-face)
                                                    )))
                            font-lock-constant-face))
             (has-sub (imenu--subalist-p entry))
             (stop (member type '("Enum")))
             (parent (member type '("Class" "Struct")))
             (include (member type '("Method" "Enum" "Class" "Function" "Struct"))))

        (push
         (propertize (car entry) 'face type-face)
         path)

        ;; NOTE: Parent entries have no position of their own, so we're using their first child's position
        (when (and include (or (not parent) has-sub))
          (push (cons
                 (concat
                  (s-pad-right 10 " "
                               (propertize (format "[%s]" type-name) 'face type-face))
                  (s-join "." (reverse path)))
                 (if has-sub
                     (cdadr entry)
                   (cdr entry)))
                results))

        (when (and has-sub (not stop))
          (setq results (rysco-imenu-eglot-items (cdr entry) path results)))
        (pop path))))
          
  results)

;;;###autoload
(defun helm-rysco-imenu (arg)
  (interactive "P")
  (helm
   :sources
   `(,(helm-build-sync-source "Stuff"
        :candidates
        (lambda ()
          (--sort
           (string< (format "%s_%s" (substring (car it) 0 12) (cdr it))
                    (format "%s_%s" (substring (car other) 0 12) (cdr other)))
           (with-helm-current-buffer
             (rysco-imenu-eglot-items))))

        :action `(("Go" .
                   ,(lambda (pos) (goto-char pos)))

                  ("Narrow" .
                   ,(lambda (pos)
                      (widen)
                      (goto-char pos)
                      (narrow-to-defun t)))

                  ("Clone & Narrow" .
                   ,(lambda (pos)
                      (widen)
                      (goto-char pos)
                      (rysco-clone-and-narrow))))))))

(defun helm-rysco-semantic-or-imenu (arg)
  (interactive "P")
  (if (and (fboundp 'eglot-current-server) (eglot-current-server))
      (call-interactively 'helm-rysco-imenu)
    (call-interactively 'helm-semantic-or-imenu)))

(provide 'helm-rysco-imenu)
