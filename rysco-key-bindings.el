(require 'rysco-system)

(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;;;;;
; Bind various keys
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\eg" 'goto-line)
(global-set-key "\C-c \r" 'mark-defun)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key "\C-c \t" 'indent-region)
(global-set-key (kbd "<C-tab>") 'complete-tag)
(global-set-key "\C-c\C-c" 'rysco-comment-dwim)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c h") 'rysco-org-element-value-to-kill)
(global-set-key "\C-cs" 'helm-rysco-store-query)
(global-set-key "\C-cj" 'pop-to-mark-command)
(global-set-key (kbd (concat "<f1> " rysco-lead-key)) 'helm-apropos)

(rysco-bind-keys
 rysco-lead-key

 ("f" 'find-tag)
 ("." 'god-mode-all)
 (rysco-lead-key 'rysco-main-transient)
 ("<tab>" 'rysco-personal-transient)
 ("SPC" 'helm-rysco-semantic-or-imenu)
 ("<RET>" 'helm-mini)
 ("s" 'helm-rysco-occur-or-resume)
 ("w" 'helm-rysco-project-ag)
 ("M-w" 'helm-do-grep-ag)
 ("y" 'helm-show-kill-ring)
 ("v" 'rysco-revert-buffer)
 ("p" 'rysco-repo-status)

 ;;Windows
 ("<right>" 'split-window-right)
 ("<down>" 'split-window-below)
 ("<left>" 'delete-window)
 ("<up>" 'delete-other-windows)

 ;; Layouts
 ("l" 'rysco-rotate-windows)
 ("[" 'rotate-frame-clockwise)

 ;; Home-row bindings for rysco features (dvorak)
 ("g" 'rysco-delete-or-clone-window-dwim)
 ("r" 'rysco-delete-or-kill-other-windows-dwim)
 ("n" 'rysco-split-right-dwim)
 ("t" 'rysco-split-down-dwim)
 ("h" 'rysco-split-left-dwim)
 ("c" 'rysco-split-up-dwim)

 ;; Frames
 ("/" 'make-frame)
 ("=" 'delete-frame)
 ("\\" 'other-frame)
 ("-" 'rysco-frame-by-name))

(define-key god-local-mode-map (kbd "i") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)

;; Extra bindings for more natural help commands while in god mode
(define-key god-local-mode-map (kbd "C-<f1> C-f") 'describe-function)
(define-key god-local-mode-map (kbd "C-<f1> C-v") 'describe-variable)
(define-key god-local-mode-map (kbd "C-<f1> C-c") 'describe-key-briefly)
(define-key god-local-mode-map (kbd "C-<f1> C-b") 'describe-bindings)

;; HACK:  C-i is bound to tab in some deep ways; this maps it elsewhere and then uses C-i to toggle god mode
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'god-mode-all)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Multiple-Cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'rysco-mc-transient)
(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "C-c n") 'mc/insert-numbers)
  (define-key mc/keymap (kbd "C-c a") 'mc/insert-letters))

;; Helpers for things that have a lot of muscle memory
(global-set-key (kbd "<escape>d") 'kill-word)
(global-set-key (kbd "<escape>DEL") 'backward-kill-word)

;; Unsetting things I don't use that accidentally get hit
(global-unset-key (kbd "C-x C-b"))

(add-hook 'eshell-hist-mode-hook
          (lambda ()
            (define-key eshell-hist-mode-map (kbd "M-r") 'helm-eshell-history)))

(defun rysco-help-movement-hook ()
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line))

(add-hook 'help-mode-hook 'rysco-help-movement-hook)
(add-hook 'Info-mode-hook 'rysco-help-movement-hook)
(add-hook 'helm-grep-mode-hook 'rysco-help-movement-hook)

(define-key messages-buffer-mode-map "n" 'next-line)
(define-key messages-buffer-mode-map "p" 'previous-line)
(define-key messages-buffer-mode-map "a" 'move-beginning-of-line)
(define-key messages-buffer-mode-map "e" 'move-end-of-line)

(with-eval-after-load 'org-colview
  (define-key org-columns-map "n" nil)
  (define-key org-columns-map "p" nil))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t") 'rysco-org-add-triage-tag)
  (define-key org-mode-map (kbd "C-c <") 'rysco-org-recapture)
  (define-key
   org-read-date-minibuffer-local-map (kbd "C-b")
   (lambda ()
     (interactive)
     (org-eval-in-calendar '(calendar-backward-day 1))))

  (define-key
   org-read-date-minibuffer-local-map (kbd "C-f")
   (lambda ()
     (interactive)
     (org-eval-in-calendar '(calendar-forward-day 1)))))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map [mouse-1] 'org-agenda-goto)
  (define-key org-agenda-mode-map "n" 'org-agenda-next-item)
  (define-key org-agenda-mode-map "p" 'org-agenda-previous-item)
  (define-key org-agenda-mode-map (kbd "C-c t") 'rysco-org-agenda-add-triage-tag)
  (define-key org-agenda-mode-map (kbd "C-c r") 'rysco-org-agenda-goto-last-refile)
  (define-key org-agenda-mode-map (kbd "C-c f") 'rysco-org-agenda-insert-clock-entry)
  (define-key org-agenda-mode-map (kbd "C-c <") 'rysco-agenda-recapture)
  (define-key org-agenda-mode-map (kbd "C-c y") 'rysco-org-todo-yesterday)
  (define-key org-agenda-mode-map ")" 'rysco-org-agenda-goto-first-section)
  (define-key org-super-agenda-header-map ")" 'rysco-org-agenda-goto-first-section))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-e") 'macrostep-expand)
  (define-key emacs-lisp-mode-map (kbd "C-c C-q") 'macrostep-collapse)
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  (define-key emacs-lisp-mode-map (kbd "C-c q") 'macrostep-collapse-all))

(defun markdown-unset-move-keys ()
  ""
  (interactive)
    (local-unset-key (kbd "M-<up>"))
    (local-unset-key (kbd "M-<down>"))
    (local-unset-key (kbd "M-<left>"))
    (local-unset-key (kbd "M-<right>"))
    (local-unset-key (kbd "M-p"))
    (local-unset-key (kbd "M-n"))
    (local-set-key (kbd "M-p") 'markdown-beginning-of-block)
    (local-set-key (kbd "M-n") 'markdown-end-of-block))

(add-hook 'markdown-mode-hook 'markdown-unset-move-keys)
(add-hook 'gfm-mode-hook 'markdown-unset-move-keys)

(with-eval-after-load 'ediff
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
            'add-d-to-ediff-mode-map))

;; Timestamp insertion
(add-hook 'calendar-mode-hook
          (lambda ()
            (define-key calendar-mode-map (kbd "RET") 'rysco-calendar-exit-and-insert-date)))

(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd "C-o") 'magit-diff-visit-file-other-window)))

(with-eval-after-load 'magit
  (add-hook 'magit-status-sections-hook 'rysco-magit-status-additions 100))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c i") 'dired-subtree-toggle)
            (local-set-key (kbd "<tab>") 'dired-subtree-toggle)
            (local-set-key [C-M-return] 'rysco-dired-os-open-dir)))

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<RET>") 'indent-new-comment-line)
            (local-set-key (kbd "C-<tab>") 'rysco-treesit-fold-toggle)
            (local-set-key (kbd "C-c C-<tab>") 'rysco-treesit-fold-unfold-all)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-related-file-ignore-include)
            (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'csharp-mode-hook
          (lambda ()
            ;; No idea why this isn't the default, but here we are...
            (local-set-key "{" 'c-electric-brace)
            (local-set-key "}" 'c-electric-brace)

            ;; with point inside the block, use these keys to hide/show
            (local-set-key "\C-c>"  'hs-hide-block)
            (local-set-key "\C-c<"  'hs-show-block)
            (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'js-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'find-tag)
            (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'graphviz-dot-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-unset-key (kbd "<S-left>"))
            (local-unset-key (kbd "<S-down>"))
            (local-unset-key (kbd "<S-right>"))
            (local-unset-key (kbd "<S-up>"))
            (local-set-key (kbd (concat rysco-lead-key " SPC")) 'helm-org-in-buffer-headings)
            (local-set-key (kbd "C-c i") 'rysco-store-create-and-insert)
            (local-set-key (kbd "C-c e") 'rysco-org-result-edit-src)
            (local-set-key (kbd "C-c M-h") 'org-fold-hide-block-all)))

;;;;
(provide 'rysco-key-bindings)
