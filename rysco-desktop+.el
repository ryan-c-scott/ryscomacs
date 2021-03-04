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

(defun rysco-desktop+-disable-auto-save (&rest args)
  (desktop-save-mode-off))

(advice-add 'desktop+--set-frame-title :override (lambda ()))
(advice-add 'desktop+-create :after #'rysco-desktop+-disable-auto-save)
(advice-add 'desktop+-load :after #'rysco-desktop+-disable-auto-save)

(defun rysco-desktop+-magit-save ()
  `(,(vc-find-root "." ".git")))

(defun rysco-desktop+-magit-restore (name &rest args)
  (magit-status-setup-buffer (car args)))

(add-to-list 'desktop+-special-buffer-handlers 'magit-status-mode)

;; magit
(desktop+-add-handler
    'magit-status-mode
  (lambda () (eq major-mode 'magit-status-mode))
  'rysco-desktop+-magit-save
  'rysco-desktop+-magit-restore)

;; eshell
(desktop+-add-handler
    'eshell-mode
  (lambda () (eq major-mode 'eshell-mode))
  (lambda () '())
  (lambda (name &rest args)
    (eshell)))

(add-to-list 'desktop+-special-buffer-handlers 'eshell-mode)

;; docker
(loop
 for (mode . creator) in '((docker-container-mode . docker-containers)
                           (docker-image-mode . docker-images)
                           (docker-network-mode . docker-networks)
                           (docker-volume-mode . docker-volumes))
 do
 (desktop+-add-handler
     mode
   `(lambda () (eq major-mode ',mode))
   (lambda () '())
   `(lambda (&rest _)
      (funcall ',creator)))

 do (add-to-list 'desktop+-special-buffer-handlers mode))

;; Preserve frame titles
(push '(name . nil) frameset-filter-alist)

;;;;
(provide 'rysco-desktop+)
