(setq ns-use-srgb-colorspace nil)

(set-variable 'inhibit-splash-screen "True")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when window-system
    (scroll-bar-mode -1)
    (tool-bar-mode -1))
(setq make-backup-files nil)

(add-to-list 'load-path "~/ryscomacs/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp")

(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function) ;We don't want buffers opened with emacsclient to give us that warning...

;; Vars
(defvar rysco-fancy-modeline nil)
(defvar rysco-fancy-modeline-theme 'ocodo-minimal-light-smt)
(defvar rysco-capslock-mapped nil)
(defvar enableP4 nil)
(defvar effective-capslock-key "<escape>")
(defvar rysco-font "Source Code Pro")
(defvar rysco-font-size "15.0")

(setq custom-theme-directory "~/ryscomacs/themes/")

;; Helm
(add-to-list 'load-path "~/ryscomacs/elisp/helm/")
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-split-window-default-side 'other)
(helm-mode 1)

;; Projectile
(add-to-list 'load-path "~/ryscomacs/elisp/dash")
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-tags-command "ctags -Re --exclude='\.svn' --extra=q -f \"%s\" %s")
(helm-projectile-on)
;(setq projectile-enable-caching t)

;; Local config
(load "localconfig" :missing-ok t)

; Font settings
(let ((font (concat rysco-font "-" rysco-font-size)))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font))

; Random setting
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

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'windmove)
(windmove-default-keybindings 'meta)

; ispell
(setq ispell-program-name "aspell")
(require 'ispell)

(if (and rysco-fancy-modeline (image-type-available-p 'svg))
    (progn
      ;; Fancy SVG based mode lines
      (add-to-list 'load-path "~/ryscomacs/elisp/svg-mode-line-themes")
      (require 'svg-mode-line-themes)
      (smt/enable)
      (set-face-attribute 'mode-line nil :box nil)
      (set-face-attribute 'mode-line-inactive nil :box nil)

      (add-to-list 'load-path "~/ryscomacs/elisp/ocodo-svg-modelines")
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
  (add-to-list 'load-path "~/ryscomacs/elisp/powerline")
  (require 'powerline)
  (require 'powerline-rysco-themes)
  (setq powerline-default-separator 'slant)
  (powerline-rysco-theme))

;; SQL
(when (string-equal system-type "windows-nt")
  (setq sql-mysql-options '("-C" "-t" "-f" "-n")))

; Bind various keys
(global-set-key "\eg" 'goto-line)
(global-set-key "\C-c \r" 'mark-defun)
(global-set-key "\C-c \t" 'indent-region)
(global-set-key (kbd "<C-tab>") 'complete-tag)
(global-set-key "\C-c\C-c" 'comment-dwim)

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
(global-set-key (kbd (concat effective-capslock-key " M-" "n")) 'windmove-right)
(global-set-key (kbd (concat effective-capslock-key " M-" "t")) 'windmove-down)
(global-set-key (kbd (concat effective-capslock-key " M-" "h")) 'windmove-left)
(global-set-key (kbd (concat effective-capslock-key " M-" "c")) 'windmove-up)

(global-set-key (kbd (concat effective-capslock-key " n")) 'split-window-right)
(global-set-key (kbd (concat effective-capslock-key " t")) 'split-window-below)
(global-set-key (kbd (concat effective-capslock-key " h")) 'delete-window)
(global-set-key (kbd (concat effective-capslock-key " c")) 'delete-other-windows)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Helpers for things that have a lot of muscle memory
(global-set-key (kbd "<escape>d") 'kill-word)
(global-set-key (kbd "<escape>DEL") 'backward-kill-word)

;; Windows specific setup for locales under cygwin
(when (eq system-type 'windows-nt)
  (setenv "LANG" "C"))

;; OSX Specific key bindings/fixes
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Modes
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


(setq auto-mode-alist
      (append
       (list
	'("\\.h$" . c++-mode)
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
		)
       auto-mode-alist))


(add-hook 'c-mode-common-hook
	  '(lambda () (c-set-style "stroustrup")
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil)
             (local-set-key (kbd "C-c o") 'ff-find-related-file-ignore-include)
	     (local-set-key "\C-c\C-c" 'comment-dwim)))

(add-hook 'csharp-mode-hook
	  '(lambda ()
					; for hide/show support
	     (hs-minor-mode 1)
	     (setq hs-isearch-open t)
	     (c-set-style "c#")
	     (setq indent-tabs-mode nil)
	     (setq tab-width 4)
					; with point inside the block, use these keys to hide/show
	     (local-set-key "\C-c>"  'hs-hide-block)
	     (local-set-key "\C-c<"  'hs-show-block)
	     (local-set-key "\C-c\C-c" 'comment-dwim)))

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


; Org mode
(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)

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

(global-set-key "\C-h" 'backward-delete-char)

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/emacs-async/")
(require 'async)

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/dvc/lisp")
(require 'dvc-autoloads)
(setq dvc-tips-enabled nil)

(require 'dvc-helpers)

(eval-after-load "dvc-diff"
  '((lambda ()
     (define-key dvc-diff-mode-map "m" 'dvc-diff-mark-file-dwim)
     (define-key dvc-diff-mode-map "u" 'dvc-diff-unmark-file-dwim)
     (define-key dvc-diff-mode-map "s" 'dvc-diff-mark-files-subdirectory)
     (define-key dvc-diff-mode-map "f" 'dvc-diff-unmark-files-subdirectory))))

;;;;;;;;;
(if enableP4
    (require 'p4))

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/expand-region/")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;
(require 'multi)
(require 's)

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/kodi/")
(autoload 'helm-kodi-shows "helm-kodi" "" t)
(autoload 'helm-kodi-movies "helm-kodi" "" t)
(autoload 'kodi-connect "kodi" "" t)

;;;;;;;;; Customizing colors used in diff mode
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :background "grey18" :foreground "cyan3")
  (set-face-attribute
   'diff-removed nil :background "grey13" :foreground "yellow3")
  (set-face-attribute
   'diff-changed nil :background "grey5" :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))

(defun replace-regexp-and-return (from to)
  (save-excursion
    (while (re-search-forward from nil t)
      (replace-match to))))

(defun insert-standard-date ()
  "Inserts standard date time string." 
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun ryscomacs-compile()
  "Byte compiles all of ~/ryscomacs/elisp."
  (interactive)
  (byte-recompile-directory "~/ryscomacs/elisp" 0))

(defun vertical-windows-with-related()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (ff-find-related-file t t))

(defun ff-find-related-file-ignore-include()
  (interactive)
  (ff-find-related-file nil t))

(defun kill-all-matching (criteria)
  "Kills all buffers that the criteria function returns non-nil on"
  (mapc (lambda (buffer)
	  (when (funcall criteria buffer)
	    (kill-buffer buffer)))
	(buffer-list)))

(defun kill-all-matching-prefix (buffer-prefixes)
  (interactive)
  (kill-all-matching (lambda (buffer)
                       (let ((names buffer-prefixes)
                             (bufname (buffer-name buffer))
                             (found))
                         (while names
                           (if (string-match (car names) bufname)
                               (progn
                                 (setq found t)
                                 (setq names nil))
                             (setq names (cdr names))))
                         found))))

(defun killall()
  "Kill all non-system buffers"
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (not (string-match "^*" (buffer-name buffer)))))
  (dvc-kill-all-buffers)
  (switch-to-buffer "*scratch*")
  (delete-other-windows))

(defun kill-dired-buffers ()
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (eq 'dired-mode (buffer-local-value 'major-mode buffer)))))

(defun kill-p4-buffers ()
  "Kill all *P4 buffers"
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (string-match "^*P4 " (buffer-name buffer)))))

; Helper function for inserting a table of contents in a markdown file
(defun markdown-unset-move-keys ()
  ""
  (interactive)
    (local-unset-key (kbd "M-<up>"))
    (local-unset-key (kbd "M-<down>"))
    (local-unset-key (kbd "M-<left>"))
    (local-unset-key (kbd "M-<right>"))
    (local-unset-key (kbd "M-p"))
    (local-unset-key (kbd "M-n")))

(add-hook 'markdown-mode-hook 'markdown-unset-move-keys)
(add-hook 'gfm-mode-hook 'markdown-unset-move-keys)

(setq markdown-asymmetric-header t)
(setq markdown-header-scaling t)
(add-hook 'markdown-mode-hook
          (lambda ()
            (markdown-update-header-faces markdown-header-scaling)
            (visual-line-mode t)
            ;(writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -r markdown_github -w html")

(defun markdown-toc ()
  (interactive)
  (let (res depth title)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\(#*\\) \\(.*\\)$" nil t)
	(add-to-list 'res `(,(match-string 1) ,(match-string 2)))))

    (dolist (elt (reverse res))
      (setq depth (replace-in-string (car elt) "#" "\t"))
      (setq title (cadr elt))

      (insert (format "%s1. [%s]" depth title))
      (setq title (replace-regexp-in-string "\s+" "-" title))
      (setq title (replace-regexp-in-string "[:*]" "" title))
      (insert (format "(#%s)\n" (downcase title))))))

(defun markdown-insert-youtube (id &optional label)
  (interactive "sID: \nsLabel: ")
  (insert (format "[![%s](http://img.youtube.com/vi/%s/0.jpg)](http://www.youtube.com/watch?v=%s)" label id id)))

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
(defun edit-local-config()
  "Open ~/.emacs.d/elisp/localconfig.el"
  (interactive)
  (find-file "~/.emacs.d/elisp/localconfig.el"))

;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b936b76d83fa0559eb1445fd4424ca2f6f25de1fe95d3a3825454b7b958646fb" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;
(load-theme 'molokai)
