(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq straight-use-symlinks t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;
(setq rysco-theme :none
      org-confirm-babel-evaluate nil
      user-emacs-directory "/root/.emacs.d/"
      user-init-file (expand-file-name "init.el" user-emacs-directory))

(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(require 'rysco-docker-autoloads)
(require 'rysco-core)
