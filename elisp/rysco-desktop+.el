(require 'desktop+)

(defun rysco-desktop+-magit-save ()
  `(,(vc-find-root "." ".git")))

(defun rysco-desktop+-magit-restore (name &rest args)
  (magit-status (car args)))

(defun rysco-desktop+-monky-save ()
  `(,(vc-find-root "." ".hg")))

(defun rysco-desktop+-monky-restore (name &rest args)
  (monky-status (car args)))

;;;;
(add-to-list 'desktop+-special-buffer-handlers
             'magit-status-mode)

(add-to-list 'desktop+-special-buffer-handlers
             'monky-mode)
  
(desktop+-add-handler
    'magit-status-mode
  (lambda () (eq major-mode 'magit-status-mode))
  'rysco-magit-save
  'rysco-magit-restore)

(desktop+-add-handler
    'monky-mode
  (lambda () (eq major-mode 'monky-mode))
  'rysco-desktop+-monky-save
  'rysco-desktop+-monky-restore)

;;;;
(provide 'rysco-desktop+)
