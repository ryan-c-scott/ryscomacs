;;; -*- lexical-binding: t; -*-

;; UTF-8 default
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(cl-defmacro rysco-bind-keys (lead &rest bindings)
  `(progn
     ,@(cl-loop
        for (key bind) in bindings collect
        `(global-set-key
          (kbd (concat ,lead " " ,key))
          ,bind))))

(cl-defmacro rysco-auto-modes (&rest elements)
  `(progn
     ,@(cl-loop
        for entry in elements collect
        `(add-to-list 'auto-mode-alist ',entry))))

(cl-defmacro rysco-add-to-list (list &rest entries)
  `(progn
     (unless (boundp ',list)
       (setq ,list nil))

     ,@(reverse
        (cl-loop
         for it in entries collect
         `(add-to-list ',list ',it)))))

(cl-defmacro rysco-set-list (list &rest entries)
  `(progn
     (setq ,list nil)
     (rysco-add-to-list ,list ,@entries)))

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

(cl-defun rysco-flat-concat (&rest forms)
  (cl-loop
   with queue
   with results
   with ancestors
   with root
   with current = forms

   unless current do
   (progn
     (pop ancestors)
     (setq current (pop queue)
           root (s-join "" (reverse ancestors))))

   while current
   as entry = (pop current)
   do
   (cond
    ((listp entry)
     (push current queue)
     (push (car entry) ancestors)
     (setq current (cdr entry)
           root (s-join "" (reverse ancestors))))

    (t
     (push
      (concat root entry)
      results)))

   finally return (nreverse results)))

(cl-defmacro with-try-switch-existing-buffer (buffer &rest body)
  (declare (indent defun) (debug (form body)))
  `(let ((existing (get-buffer ,buffer)))
     (if existing
         (switch-to-buffer existing)
       ,@body)))

(cl-defmacro rysco-org-agenda-files (&rest forms)
  (let ((files (apply 'rysco-flat-concat forms)))
    `(setq rysco-agenda-files
           ',files
           org-agenda-files
           ',files)))

(defmacro rysco-filter-agenda (&optional form)
  `(setq org-agenda-files
         ,(if (not form)
              'rysco-agenda-files
            `(--remove ,form rysco-agenda-files))))

;;;;
(provide 'rysco-system)
