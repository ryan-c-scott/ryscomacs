(require 'dash)
(require 'helm)

(defun rysco-cpp-breadcrumb (node)
  "Walk up the tree from NODE collecting enclosing names."
  (let ((parts '())
        (current (treesit-node-parent node)))
    (while current
      (let ((type (treesit-node-type current)))
        (cond
         ((member type '("function_definition" "class_specifier"
                         "struct_specifier" "namespace_definition"))
          (let ((name-node (or (treesit-node-child-by-field-name current "name")
                               ;; function_definition name is nested deeper
                               (treesit-node-child-by-field-name
                                (treesit-node-child-by-field-name current "declarator")
                                "declarator"))))
            (when name-node
              (push (treesit-node-text name-node t) parts))))))
      (setq current (treesit-node-parent current)))
    parts))

(defun rysco-cpp-collect-items ()
  "Return a list of (breadcrumb-string . buffer-position) for all functions and types."
  (let ((captures
         (treesit-query-capture
          (treesit-buffer-root-node 'cpp)
          '((function_definition
             declarator: (function_declarator
                          declarator: (_) @func.name)) @func.def

                          (class_specifier
                           name: (_) @class.name) @class.def

                          (struct_specifier
                           name: (_) @struct.name) @struct.def

                          (enum_specifier
                           name: (_) @enum.name) @enum.def

                          (namespace_definition
                           name: (_) @ns.name) @ns.def))))
    (nreverse
     (cl-loop
      for ((def-sym . def-node) (name-sym . name-node)) on captures by 'cddr
      as name = (treesit-node-text name-node)
      as pos =  (treesit-node-start def-node)
      as breadcrumb = (rysco-cpp-breadcrumb name-node)
      as full-name = (mapconcat #'identity
                                (append (butlast breadcrumb) (list name))
                                ".")
      as type-name = (upcase (or
                              (cdr (assoc def-sym '((class.def . "STRUCT")
                                                    (struct.def . "STRUCT")
                                                    (enum.def . "ENUM")
                                                    (func.def . "FUNC")
                                                    )))
                              (format "%s" def-sym)))
      as type-face = (cdr (assoc def-sym '((class.def . font-lock-type-face)
                                           (struct.def . font-lock-type-face)
                                           (enum.def . font-lock-type-face)
                                           (func.def . font-lock-function-name-face)
                                           )))

      unless (member def-sym '(ns.def))
      collect (cons
               (concat
                (propertize
                 (s-center 8 (format "%s" type-name))
                 'face 'gnus-emphasis-underline-italic)
                " "
                (propertize
                 full-name
                 'face type-face))
               (set-marker (make-marker) pos))))))

;;;###autoload
(defun helm-rysco-imenu (arg)
  (interactive "P")
  (helm
   :sources
   `(,(helm-build-sync-source "Stuff"
        :candidates
        (lambda ()
          (--sort
           (string< (format "%s_%s" (substring (car it) 0 8) (cdr it))
                    (format "%s_%s" (substring (car other) 0 8) (cdr other)))
           (with-helm-current-buffer
             (rysco-cpp-collect-items))))

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
  (if (derived-mode-p 'c++-ts-mode)
      (call-interactively 'helm-rysco-imenu)
    (call-interactively 'helm-semantic-or-imenu)))

(provide 'helm-rysco-imenu)
