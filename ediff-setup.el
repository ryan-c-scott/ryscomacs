;; This is here to disable these as soon as possible (although duplicated in rysco-core)
(when window-system
    (scroll-bar-mode -1)
    (tool-bar-mode -1))

(add-to-list 'load-path "~/ryscomacs/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'rysco-core)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("a0432a35716d4e77d954f702401abde456a51e114d9567afa7a906306fd53b6c" "b936b76d83fa0559eb1445fd4424ca2f6f25de1fe95d3a3825454b7b958646fb" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" default))))
(custom-set-faces)

;;;;;;;;;;;;
(load-theme 'molokai)

;;;;;;;;;;;;
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-quit-hook 'save-buffers-kill-emacs)

