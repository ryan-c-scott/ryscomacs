(cl-defmacro rysco-bind-keys (lead &rest bindings)
  `(progn
     ,@(cl-loop
        for (key bind) in bindings collect
        `(global-set-key
          (kbd (concat ,lead " " ,key))
          ,bind))))

(cl-defmacro rysco-auto-modes (list &rest elements)
  `(progn
     ,@(cl-loop
        for entry in elements collect
        `(add-to-list 'auto-mode-alist ',entry))))

(cl-defmacro rysco-add-to-list (list &rest entries)
  `(progn
     ,@(reverse
        (cl-loop
         for it in entries collect
         `(add-to-list ',list ',it)))))

(cl-defmacro rysco-add-to-loadpath (&rest pathlist)
  `(progn
     ,@(reverse
        (cl-loop
         with base = nil
         for path in pathlist
         if (and (listp path) (equal (car path) :base)) do
         (setq base (cadr path))
         else collect
         `(add-to-list 'load-path ,(concat base "/" path))))))

(cl-defmacro rysco-autoloads (&rest entries)
  `(progn
     ,@(cl-loop
        for (sym file doc) in entries collect
        `(autoload ',sym ,file ,doc t))))

(cl-defmacro rysco-packages (&rest packages)
  (cl-loop
   with loads
   for pkg in packages
   collect `(straight-use-package ',pkg) into loads
   finally return
   `(progn
      (message "Loading straight.el packages:")
      (let ((straight-current-profile 'rysco))
        ,@loads))))

(cl-defmacro rysco-use-package (pkg &rest args)
  (let ((name (if (listp pkg) (car pkg) pkg))
        (straight (if (listp pkg) pkg t)))
  `(progn
     (let ((straight-current-profile 'rysco))
       (use-package
           ,name
           :straight ,straight
           ,@args)))))

(cl-defmacro rysco-exec-path (&rest paths)
  `(prog1 nil
     ,@(cl-loop
        with path-form
        with path-env
        for p in paths
        collect `(add-to-list 'exec-path ,p) into path-form
        concat (format
                "%s%s"
                p
                (if (eq system-type 'windows-nt)
                    ";"
                  ":"))
        into path-env
        finally return
        (prog1 path-form
          (setenv "PATH" (concat path-env (getenv "PATH")))))))


;;;;
(provide 'rysco-system)