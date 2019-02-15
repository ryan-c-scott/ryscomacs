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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars
(defvar rysco-use-straight-packages nil)
(defvar rysco-fancy-modeline nil)
(defvar rysco-fancy-modeline-theme 'ocodo-minimal-light-smt)
(defvar rysco-theme nil)
(defvar rysco-capslock-mapped nil)
(defvar rysco-ssh-config-directories nil)
(defvar rysco-lead-key "<escape>")
(defvar rysco-font "Source Code Pro")
(defvar rysco-font-size "15.0")
(defvar rysco-writing-font "Georgia")
(defvar-local rysco-modeline-extras nil)

(require 'cl)
(require 'rysco-util)

;; Local config
(load "localconfig" :missing-ok t)

;; HACK:  Org-mode has issues installing via straight.el
(when rysco-use-straight-packages
  (require 'subr-x)
  (straight-use-package 'git)

  (defun org-git-version ()
    "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (git-run "describe"
                "--match=release\*"
                "--abbrev=6"
                "HEAD"))))

  (defun org-release ()
    "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))

  (provide 'org-version)

  (straight-use-package 'org))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load paths
(rysco-add-to-loadpath
 (:base "~/ryscomacs/elisp")
 ""
 "kodi/"
 "dired-hacks/"
 "monky")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(if (not rysco-use-straight-packages)
    (rysco-packages :manual "~/ryscomacs/elisp/packages")

  ;;
  (rysco-packages
   :packages
   (autothemer
    cg
    csharp-mode
    csv-mode
    dash
    dired-hacks-utils
    eimp
    async
    doom-themes
    kaolin-themes
    expand-region
    f
    fill-column-indicator
    glsl-mode
    go-mode
    graphviz-dot-mode
    helm
    helm-projectile
    json-mode
    lua-mode
    magit
    markdown-mode
    memoize
    multiple-cursors
    ocodo-svg-modelines
    php-mode
    powerline
    projectile
    protobuf-mode
    rust-mode
    s
    shift-number
    svg-mode-line-themes
    websocket
    writegood-mode
    xmlgen
    god-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; requires
(setq load-prefer-newer t)
(setq inhibit-compacting-font-caches t) ;; Fixes hiccups on certain unicode characters

(require 'doom-themes)
(doom-themes-org-config)
(require 'kaolin-themes)

(require 'all-the-icons)
(require 'async)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-loads
(rysco-autoloads
 (screenwriter-mode "screenwriter")
 (helm-screenwriter-init "helm-screenwriter")
 (helm-kodi-shows "helm-kodi")
 (helm-kodi-movies "helm-kodi")
 (kodi-connect "kodi"))

(require 'monky)
(require 'multi)
(require 's)
(require 'helm-config)
(require 'projectile)
(require 'helm-projectile)
(require 'uniquify)
(require 'windmove)
;; (require 'ispell)
(require 'multiple-cursors)
(require 'bluedot)
(require 'font-lock+)

(require 'run-assoc)
(setq associated-program-alist
      '(((lambda (file)
           (when (eq system-type 'windows-nt)
             (w32-shell-execute "open" (convert-standard-filename file)))) "\\.*$")))

(require 'dired+)
(require 'dired-subtree)
(require 'dired-collapse)
(require 'dired-filter)
(add-hook 'dired-mode-hook (lambda ()
                            (local-set-key (kbd "C-c i") 'dired-subtree-toggle)
                            (local-set-key (kbd "<tab>") 'dired-subtree-toggle)))

(setq dired-filter-group-saved-groups
      `(("default"
         ("PDF"
          (extension . "pdf"))
         ("LaTeX"
          (extension "tex" "bib"))
         ("Org"
          (extension . "org"))
         ("Image"
          (extension "jpg" "JPG" "png" "dds" "bmp"))
         ("Media"
          (extension "mp3" "mp4" "avi" "ogg" "ogv" "mkv" "mpg" "flv" "mov"))
         ("Markdown"
          (extension . "md"))
         ("Archives"
          (extension "zip" "rar" "gz" "bz2" "tar" "dmg"))
         ("Source"
          (extension "py" "c" "cpp" "h" "hpp" "lua" "go" "el" "glsl" "rs"))
         ("Exe"
          (extension "exe" "sh" "bat")))))

(rysco-auto-modes
 ("\\.h$" . c++-mode)
 ("\\.ino$" . c++-mode)
 ("\\.xml$" . xml-mode)
 ("\\.css$" . css-mode)
 ("\\.cs$" . csharp-mode)
 ("\\.cg$" . cg-mode)
 ("\\.glsl$" . cg-mode)
 ("\\.shader$" . lua-mode)
 ("\\.fs$" . c++-mode)
 ("\\.vs$" . c++-mode)
 ("\\.lua$" . lua-mode)
 ("\\.particle$" . lua-mode)
 ("\\.material$" . lua-mode)
 ("\\.prefab$" . lua-mode)
 ("\\.scene$" . lua-mode)
 ("\\.deps$" . lua-mode)
 ("\\.item_list$" . lua-mode)
 ("\\.json$" . json-mode)
 ("\\.php$" . php-mode)
 ("\\.proto$" . protobuf-mode)
 ("\\.markdown$" . gfm-mode)
 ("\\.md$" . gfm-mode)
 ("\\.screenplay$" . screenwriter-mode)
 ("\\.csv$" . csv-mode)
 ("\\.dot$" . graphviz-dot-mode)
 ("\\.gv$" . graphviz-dot-mode)
 ("\\.rs$" . rust-mode))

;;;; Icons for modes
(rysco-add-to-list
 all-the-icons-mode-icon-alist
 (monky-mode all-the-icons-faicon "mercury")
 (kodi-mode all-the-icons-material "tv")
 (lua-mode all-the-icons-fileicon "lua"))

;; Note:  Need to restore and re-memoize this function in order to get the changes to take effect
(memoize-restore 'all-the-icons-icon-for-mode)
(memoize 'all-the-icons-icon-for-mode)

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

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-completion-system 'helm
      projectile-indexing-method 'alien
      projectile-switch-project-action 'helm-projectile
      projectile-tags-command "ctags -Re --exclude='\.svn' --extra=q -f \"%s\" %s")
(helm-projectile-on)
;(setq projectile-enable-caching t)
(setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")

(when (string-equal system-type "windows-nt")
  (setq sql-mysql-options '("-C" "-t" "-f" "-n")))

(setq custom-theme-directory "~/ryscomacs/themes/"
      org-export-allow-bind-keywords t
      org-log-done 'time
      org-enforce-todo-dependencies t
      markdown-asymmetric-header t
      markdown-header-scaling t
      markdown-command "pandoc --smart -r markdown_github -w html"
      graphviz-dot-auto-indent-on-braces nil
      graphviz-dot-auto-indent-on-semi nil
      graphviz-dot-auto-indent-on-newline nil)

;; OSX Specific key bindings/fixes
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil)

(transient-mark-mode t)
(global-font-lock-mode t)
(jit-lock-mode t)
(normal-erase-is-backspace-mode 1)
(show-paren-mode t)
(menu-bar-mode -1)
(toggle-indicate-empty-lines)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq tooltip-use-echo-area t
      indent-tabs-mode nil
      truncate-partial-width-windows nil
      ring-bell-function 'ignore
      eshell-prefer-lisp-functions t
      pcomplete-cycle-completions nil
      eshell-prompt-function 'rysco-eshell-prompt
      dired-recursive-deletes 'always
      uniquify-buffer-name-style 'reverse
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"
      ispell-program-name "aspell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks/setups

(add-hook 'eshell-mode-hook
          (lambda ()
            (cl-loop
             with aliases = '(("d" "dired-other-window $1")
                              ("ff" "find-file-other-window $1")
                              ("dir" "ls $*"))
             for alias in aliases do
             (add-to-list 'eshell-command-aliases-list alias))

            ;; HACK:  vc is not currently eshell aware
            (require 'vc)
            (defun vc-deduce-backend ()
              (cond ((derived-mode-p 'vc-dir-mode)   vc-dir-backend)
	            ((derived-mode-p 'log-view-mode) log-view-vc-backend)
	            ((derived-mode-p 'log-edit-mode) log-edit-vc-backend)
	            ((derived-mode-p 'diff-mode)     diff-vc-backend)
                    ;; Maybe we could even use comint-mode rather than shell-mode?
	            ((derived-mode-p 'dired-mode 'shell-mode 'compilation-mode 'eshell-mode)
	             (ignore-errors (vc-responsible-backend default-directory)))
	            (vc-mode (vc-backend buffer-file-name))))))

(add-hook 'monky-mode-hook
          (lambda ()
            (visual-line-mode)
            
            (set-face-attribute
             'monky-section-title nil :foreground "#F92672" :height 1.2 :underline t)
            (set-face-attribute
             'monky-diff-add nil :background "grey18" :foreground "cyan3")
            (set-face-attribute
             'monky-diff-del nil :background "grey13" :foreground "yellow3")
            (set-face-attribute
             'monky-diff-title nil :background "darkgreen" :box t :foreground "white")
            (set-face-attribute
             'monky-diff-hunk-header nil :background "grey18" :foreground "purple")
            (set-face-attribute
             'monky-log-sha1 nil :background nil :foreground "yellow")
            (set-face-attribute
             'monky-log-author nil :background nil :foreground "grey55" :slant 'italic)
            (set-face-attribute
             'monky-log-head-label-local nil :background nil :foreground "sky blue" :slant 'italic)
            (set-face-attribute
             'monky-log-head-label-tags nil :foreground "black" :weight 'normal :slant 'italic)
            (set-face-attribute
             'monky-log-head-label-phase nil :foreground "lightgreen" :background nil :height 1 :weight 'normal :box t :underline nil :slant 'normal)))

;; Font 
(let ((font (concat rysco-font "-" rysco-font-size)))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font))

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function) ;We don't want buffers opened with emacsclient to give us that warning...

;; Do not use tabs
(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-common-hook
	  '(lambda () (c-set-style "stroustrup")
             (semantic-mode t)
	     (setq tab-width 4
	           indent-tabs-mode nil)
             (local-set-key (kbd "C-c o") 'ff-find-related-file-ignore-include)
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'csharp-mode-hook
	  '(lambda ()
	     ;; for hide/show support
	     ;; (hs-minor-mode 1)
	     ;; (setq hs-isearch-open t)
             (semantic-mode t)
	     (c-set-style "c#")
	     (setq indent-tabs-mode nil
	           tab-width 4)
             
             ;; No idea why this isn't the default, but here we are...
             (local-set-key "{" 'c-electric-brace)
             (local-set-key "}" 'c-electric-brace)

             ;; with point inside the block, use these keys to hide/show
	     (local-set-key "\C-c>"  'hs-hide-block)
	     (local-set-key "\C-c<"  'hs-show-block)
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
                   tab-width 4
                   js-indent-level 2)
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

(defun rysco-org-hook ()
  ""
  (interactive)
  (visual-line-mode t)
  (local-unset-key (kbd "<S-left>"))
  (local-unset-key (kbd "<S-down>"))
  (local-unset-key (kbd "<S-right>"))
  (local-unset-key (kbd "<S-up>"))
  (local-set-key (kbd (concat rysco-lead-key " SPC")) 'helm-org-in-buffer-headings))

(add-hook 'org-mode-hook 'rysco-org-hook)
(add-hook 'org-agenda-mode-hook 'rysco-org-hook)

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
            ;;(writegood-mode t)
            
            (when (not (eq system-type 'windows-nt))
              (flyspell-mode t))
            (require 'hugo)))

(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :background "grey18" :foreground "cyan3")
  (set-face-attribute
   'diff-removed nil :background "grey13" :foreground "yellow3")
  (set-face-attribute
   'diff-changed nil :background "grey5" :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))

;; HACK:  I don't like way that this function would call org-show-entry at the end.
(defun helm-org-goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (re-search-backward "^\\*+ " nil t))

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
      (smt/set-theme rysco-fancy-modeline-theme))

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
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cj" 'pop-to-mark-command)
(global-set-key (kbd (concat "<f1> " rysco-lead-key)) 'helm-apropos)

(rysco-bind-keys
 rysco-lead-key

 ("f" 'find-tag)
 ("." 'god-local-mode)
 (rysco-lead-key 'helm-mini)
 ("SPC" 'helm-semantic-or-imenu)
 ("<RET>" 'helm-resume)
 ("s" 'helm-occur)
 ("y" 'helm-show-kill-ring)
 ("v" 'rysco-revert-buffer)
 ("p" 'rysco-repo-status)
 
 ;;Windows
 ("<right>" 'split-window-right)
 ("<down>" 'split-window-below)
 ("<left>" 'delete-window)
 ("<up>" 'delete-other-windows)

 ;; Home-row bindings for rysco features (dvorak)
 ("g" 'delete-window)
 ("r" 'delete-other-windows)
 ("n" 'rysco-split-right-dwim)
 ("t" 'rysco-split-down-dwim)
 ("h" 'rysco-split-left-dwim)
 ("c" 'rysco-split-up-dwim)

 ;; Frames
 ("/" 'make-frame)
 ("=" 'delete-frame)
 ("\\" 'other-frame)
 ("-" 'rysco-frame-by-name))

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Multiple-Cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Helpers for things that have a lot of muscle memory
(global-set-key (kbd "<escape>d") 'kill-word)
(global-set-key (kbd "<escape>DEL") 'backward-kill-word)

;;
(defun rysco-post-init-setup ()
  (unless (equal rysco-theme :none)
    (load-theme
     (if rysco-theme
         rysco-theme
       'molokai)))
  (server-start)
  (bluedot-resume))

(add-hook 'after-init-hook 'rysco-post-init-setup)

;;;;;;;;;;;;
(provide 'rysco-core)
