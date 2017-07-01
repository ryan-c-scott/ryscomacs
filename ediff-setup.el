(add-to-list 'load-path "~/ryscomacs/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'rysco-core)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("b936b76d83fa0559eb1445fd4424ca2f6f25de1fe95d3a3825454b7b958646fb" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" default))))
(custom-set-faces)

;;;;;;;;;;;;
(load-theme 'molokai)

;;;;;;;;;;;;
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-quit-hook 'save-buffers-kill-emacs)

