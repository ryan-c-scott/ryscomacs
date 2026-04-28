(require 'dash)
(require 'eglot)
(require 'helm)

(defun rysco-imenu-items (&optional alist path results)
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
          (setq results (rysco-imenu-items (cdr entry) path results)))
        (pop path))))
          
  results)

;;;###autoload
(defun rysco-imenu-candidates ()
  (interactive)
  (helm
   :sources
   `(,(helm-build-sync-source "Stuff"
        :candidates
        (--sort
         (string< (format "%s_%s" (substring (car it) 0 12) (cdr it))
                  (format "%s_%s" (substring (car other) 0 12) (cdr other)))
         (rysco-imenu-items))

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

(defun helm-rysco-imenu (&optional arg)
  (interactive "P")
  (defun rysco-imenu-kinds (&optional alist depth)
  (interactive)
)


(provide 'helm-rysco-imenu)
