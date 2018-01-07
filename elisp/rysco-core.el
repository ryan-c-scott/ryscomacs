;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars
(defvar rysco-fancy-modeline nil)
(defvar rysco-fancy-modeline-theme 'ocodo-minimal-light-smt)
(defvar rysco-capslock-mapped nil)
(defvar rysco-ssh-config-directories nil)
;; (defvar enableP4 nil)
(defvar effective-capslock-key "<escape>")
(defvar rysco-font "Source Code Pro")
(defvar rysco-font-size "15.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load paths
(add-to-list 'load-path "~/ryscomacs/elisp/")
(add-to-list 'load-path "~/ryscomacs/elisp/helm/")
(add-to-list 'load-path "~/ryscomacs/elisp/dash")
(add-to-list 'load-path "~/ryscomacs/elisp/go-mode/")
(add-to-list 'load-path "~/ryscomacs/elisp/kodi/")
(add-to-list 'load-path "~/ryscomacs/elisp/expand-region/")
(add-to-list 'load-path "~/ryscomacs/elisp/dvc/lisp")
(add-to-list 'load-path "~/ryscomacs/elisp/emacs-async/")
(add-to-list 'load-path "~/ryscomacs/elisp/svg-mode-line-themes")
(add-to-list 'load-path "~/ryscomacs/elisp/ocodo-svg-modelines")
(add-to-list 'load-path "~/ryscomacs/elisp/powerline")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; requires
(require 'rysco-util)
(require 'async)
(require 'dvc-autoloads)
(require 'dvc-helpers)

;; (if enableP4
;;     (require 'p4))

(require 'expand-region)
(require 'multi)
(require 's)
(require 'go-mode-autoloads)
(require 'helm-config)
(require 'projectile)
(require 'helm-projectile)
(require 'uniquify)
(require 'windmove)
(require 'ispell)

(require 'run-assoc)
(setq associated-program-alist
      '(((lambda (file)
           (when (eq system-type 'windows-nt)
             (w32-shell-execute "open" (convert-standard-filename file)))) "\\.*$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-loads
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(autoload 'cg-mode "cg-mode" "Major mode for editing CG program code." t)
(autoload 'json-mode "json-mode" "Major mode for editing json data." t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(autoload 'php-mode "php-mode" "PHP editing mode." t)
(autoload 'protobuf-mode "protobuf-mode" "Protobuf editing mode." t)
(autoload 'screenwriter-mode "screenwriter" "Major mode for the screenwriter tool." t)
(autoload 'helm-screenwriter-init "helm-screenwriter" "Helm routines for screenwriter-mode." t)
(autoload 'csv-mode "csv-mode" "Major mode for dealing with CSV data." t)
(autoload 'writegood-mode "writegood-mode" "Minor mode for identifying poorly constructed prose." t)
(autoload 'markdown-mode "markdown-mode" "Major mode for the Markdown format." t)
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Major mode for working with graphviz dot files" t)

(autoload 'helm-kodi-shows "helm-kodi" "" t)
(autoload 'helm-kodi-movies "helm-kodi" "" t)
(autoload 'kodi-connect "kodi" "" t)

(autoload 'shift-number-up "shift-number" nil t)
(autoload 'shift-number-down "shift-number" nil t)

(setq auto-mode-alist
      (append
       (list
	'("\\.h$" . c++-mode)
	'("\\.ino$" . c++-mode)
        '("\\.xml$" . xml-mode)
        '("\\.css$" . css-mode)
        '("\\.cs$" . csharp-mode)
        '("\\.cg$" . cg-mode)
        '("\\.glsl$" . cg-mode)
        '("\\.shader$" . lua-mode)
        '("\\.fs$" . c++-mode)
        '("\\.vs$" . c++-mode)
        '("\\.lua$" . lua-mode)
        '("\\.particle$" . lua-mode)
        '("\\.material$" . lua-mode)
        '("\\.prefab$" . lua-mode)
        '("\\.scene$" . lua-mode)
        '("\\.deps$" . lua-mode)
	'("\\.item_list$" . lua-mode)
        '("\\.json$" . json-mode)
        '("\\.php$" . php-mode)
        '("\\.proto$" . protobuf-mode)
	'("\\.markdown$" . gfm-mode)
	'("\\.md$" . gfm-mode)
	'("\\.screenplay$" . screenwriter-mode)
	'("\\.csv$" . csv-mode)
        '("\\.dot$" . graphviz-dot-mode)
        '("\\.gv$" . graphviz-dot-mode)
		)
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
(setq ns-use-srgb-colorspace nil)

(set-variable 'inhibit-splash-screen "True")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Windows specific setup for locales under cygwin
(when (eq system-type 'windows-nt)
  (setenv "LANG" "C"))

(when window-system
    (scroll-bar-mode -1)
    (tool-bar-mode -1))
(setq make-backup-files nil)

(setq helm-split-window-default-side 'other)
(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-tags-command "ctags -Re --exclude='\.svn' --extra=q -f \"%s\" %s")
(helm-projectile-on)
;(setq projectile-enable-caching t)

(when (string-equal system-type "windows-nt")
  (setq sql-mysql-options '("-C" "-t" "-f" "-n")))

(setq custom-theme-directory "~/ryscomacs/themes/")
(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)
(setq markdown-asymmetric-header t)
(setq markdown-header-scaling t)
(setq markdown-command "pandoc --smart -r markdown_github -w html")
(setq dvc-tips-enabled nil)
(setq graphviz-dot-auto-indent-on-braces nil)
(setq graphviz-dot-auto-indent-on-semi nil)
(setq graphviz-dot-auto-indent-on-newline nil)

;; OSX Specific key bindings/fixes
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(transient-mark-mode t)
(global-font-lock-mode t)
(normal-erase-is-backspace-mode 1)
(show-paren-mode t)
(menu-bar-mode -1)
(toggle-indicate-empty-lines)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq indent-tabs-mode nil)
(setq truncate-partial-width-windows nil)
(setq ring-bell-function 'ignore)
(setq eshell-prefer-lisp-functions t)
(setq dired-recursive-deletes 'always)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(windmove-default-keybindings 'meta)
(setq ispell-program-name "aspell")

;; Local config
(load "localconfig" :missing-ok t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks/setups

;; Font 
(let ((font (concat rysco-font "-" rysco-font-size)))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font))

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function) ;We don't want buffers opened with emacsclient to give us that warning...

(add-hook 'c-mode-common-hook
	  '(lambda () (c-set-style "stroustrup")
             (semantic-mode t)
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil)
             (local-set-key (kbd "C-c o") 'ff-find-related-file-ignore-include)
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'csharp-mode-hook
	  '(lambda ()
	     ;;    			; for hide/show support
	     ;; (hs-minor-mode 1)
	     ;; (setq hs-isearch-open t)
	     (c-set-style "c#")
	     (setq indent-tabs-mode nil)
	     (setq tab-width 4)
					; with point inside the block, use these keys to hide/show
	     (local-set-key "\C-c>"  'hs-hide-block)
	     (local-set-key "\C-c<"  'hs-show-block)
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (local-set-key (kbd "M-.") 'find-tag)))

(add-hook 'json-mode-hook
	  '(lambda ()
	     (setq-default indent-tabs-mode nil)
             (setq js-indent-level 2)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(add-hook 'screenwriter-mode-hook
	  '(lambda ()
             (helm-screenwriter-init)
	     (require 'fill-column-indicator)
	     (fci-mode)))

(add-hook 'graphviz-dot-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(defun org-unset-move-keys ()
  ""
  (interactive)
  (local-unset-key (kbd "<S-left>"))
  (local-unset-key (kbd "<S-down>"))
  (local-unset-key (kbd "<S-right>"))
  (local-unset-key (kbd "<S-up>"))
  (local-unset-key (kbd "<M-left>"))
  (local-unset-key (kbd "<M-down>"))
  (local-unset-key (kbd "<M-right>"))
  (local-unset-key (kbd "<M-up>")))

(add-hook 'org-mode-hook 'org-unset-move-keys)
(add-hook 'org-agenda-mode-hook 'org-unset-move-keys)

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

(add-hook 'markdown-mode-hook
          (lambda ()
            (markdown-update-header-faces markdown-header-scaling)
            (visual-line-mode t)
            ;(writegood-mode t)
            (flyspell-mode t)))

(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :background "grey18" :foreground "cyan3")
  (set-face-attribute
   'diff-removed nil :background "grey13" :foreground "yellow3")
  (set-face-attribute
   'diff-changed nil :background "grey5" :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))

(eval-after-load "dvc-diff"
  '((lambda ()
     (define-key dvc-diff-mode-map "m" 'dvc-diff-mark-file-dwim)
     (define-key dvc-diff-mode-map "u" 'dvc-diff-unmark-file-dwim)
     (define-key dvc-diff-mode-map "s" 'dvc-diff-mark-files-subdirectory)
     (define-key dvc-diff-mode-map "f" 'dvc-diff-unmark-files-subdirectory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme/Modeline
(if (and rysco-fancy-modeline (image-type-available-p 'svg))
    (progn
      ;; Fancy SVG based mode lines
      (require 'svg-mode-line-themes)
      (smt/enable)
      (set-face-attribute 'mode-line nil :box nil)
      (set-face-attribute 'mode-line-inactive nil :box nil)

      (require 'ocodo-svg-modelines)
      (ocodo-svg-modelines-init)
      (smt/set-theme rysco-fancy-modeline-theme)

      ;; HACK:  The unicode characters that ocodo uses for modified buffer status are maybe not so universal
      (smt/defwidget buffer-dirty
	:text (lambda (widget)
		(when (buffer-file-name)
		  (if (and (buffer-modified-p) (or buffer-file-name buffer-offer-save))
		      " ▣ " " √ ")))))

  ;; Powerline
  (require 'powerline)
  (require 'powerline-rysco-themes)
  (setq powerline-default-separator 'slant)
  (powerline-rysco-theme))

;;;;;;;;;;;;;;;;;;;;
; IDO buffer switching crap
(require 'ido) 
(ido-mode 'files)

(setq 
  ido-save-directory-list-file nil ;"~/.emacs.d/ido.last"
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)

  ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

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

(global-set-key (kbd (concat effective-capslock-key " .")) 'find-tag)
(global-set-key (kbd (concat effective-capslock-key " " effective-capslock-key)) 'helm-mini)
(global-set-key (kbd (concat effective-capslock-key " SPC")) 'helm-semantic-or-imenu)
(global-set-key (kbd (concat effective-capslock-key " <RET>")) 'helm-resume)
(global-set-key (kbd (concat effective-capslock-key " s")) 'helm-occur)
(global-set-key (kbd (concat effective-capslock-key " y")) 'helm-show-kill-ring)

(global-set-key (kbd (concat effective-capslock-key " <right>")) 'split-window-right)
(global-set-key (kbd (concat effective-capslock-key " <down>")) 'split-window-below)
(global-set-key (kbd (concat effective-capslock-key " <left>")) 'delete-window)
(global-set-key (kbd (concat effective-capslock-key " <up>")) 'delete-other-windows)

;; Testing:  Experimental home-row bindings for rysco features
(global-set-key (kbd (concat effective-capslock-key " g")) 'delete-window)
(global-set-key (kbd (concat effective-capslock-key " r")) 'delete-other-windows)

(global-set-key (kbd (concat effective-capslock-key " n")) 'rysco-split-right-dwim)
(global-set-key (kbd (concat effective-capslock-key " t")) 'rysco-split-down-dwim)
(global-set-key (kbd (concat effective-capslock-key " h")) 'rysco-split-left-dwim)
(global-set-key (kbd (concat effective-capslock-key " c")) 'rysco-split-up-dwim)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Helpers for things that have a lot of muscle memory
(global-set-key (kbd "<escape>d") 'kill-word)
(global-set-key (kbd "<escape>DEL") 'backward-kill-word)

;;;;;;;;;;;;
(provide 'rysco-core)
