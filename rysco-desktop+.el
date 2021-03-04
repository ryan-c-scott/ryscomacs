;;; -*- lexical-binding: t; -*-

(require 'desktop+)

;;;###autoload
(defun rysco-desktop+-create ()
  (interactive)
  (--when-let
      (helm
       :sources
       `(,(helm-build-sync-source "Existing"
            :candidates
            (remove "."
                    (remove ".."
                            (directory-files desktop+-base-dir))))
         ,(helm-build-dummy-source "New")))
    (desktop+-create it)))

(defun rysco-desktop+-magit-save ()
  `(,(vc-find-root "." ".git")))

(defun rysco-desktop+-magit-restore (name &rest args)
  (magit-status-internal (car args)))

(defun rysco-desktop+-disable-auto-save (&rest args)
  (desktop-save-mode-off))

(advice-add 'desktop+--set-frame-title :override (lambda ()))
(advice-add 'desktop+-create :after #'rysco-desktop+-disable-auto-save)
(advice-add 'desktop+-load :after #'rysco-desktop+-disable-auto-save)

(add-to-list 'desktop+-special-buffer-handlers 'magit-status-mode)
(add-to-list 'desktop+-special-buffer-handlers 'eshell-mode)

(desktop+-add-handler
    'magit-status-mode
  (lambda () (eq major-mode 'magit-status-mode))
  'rysco-desktop+-magit-save
  'rysco-desktop+-magit-restore)

(desktop+-add-handler
    'eshell-mode
  (lambda () (eq major-mode 'eshell-mode))
  (lambda () '())
  (lambda (name &rest args)
    (eshell)))

;; Preserve frame titles
(push '(name . nil) frameset-filter-alist)

;;;;
(provide 'rysco-desktop+)
