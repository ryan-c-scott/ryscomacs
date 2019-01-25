;; This is here to disable these as soon as possible (although duplicated in rysco-core)
(when window-system
    (scroll-bar-mode -1)
    (tool-bar-mode -1))

(add-to-list 'load-path "~/ryscomacs/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp")

(setq exec-path (cons "/usr/local/bin" exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(require 'rysco-core)
;;;;;;;;;;;;
