;;; -*- lexical-binding: t; -*-

(add-to-list 'straight-profiles '(rysco . "rysco.el"))

;; Disable some features for setup duration
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6
      rysco-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

;; Enable gcmh as early as possible
(require 'rysco-system)

(rysco-packages
 gcmh)

(require 'gcmh)
(gcmh-mode 1)
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars
(defvar rysco-fancy-modeline nil)
(defvar rysco-fancy-modeline-theme 'ocodo-minimal-light-smt)
(defvar rysco-theme nil)
(defvar rysco-capslock-mapped nil)
(defvar rysco-ssh-config-directories nil)
(defvar rysco-lead-key "<escape>")
(defvar rysco-font "Source Code Pro")
(defvar rysco-font-size "15.0")
(defvar rysco-writing-font "Georgia")
(defvar-local org-export-directory "org-export")
(defvar-local rysco-modeline-extras nil)

;; Setup the rysco versions file for straight
;; NOTE:  This overwrites whatever is currently in the users folder
(let ((repo-versions (expand-file-name "straight/repos/ryscomacs/versions/rysco.el" user-emacs-directory))
      (straight-versions (expand-file-name "straight/versions/" user-emacs-directory)))
  (when (file-exists-p repo-versions)
    (mkdir straight-versions t)
    (copy-file repo-versions straight-versions t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(rysco-packages
 org
 use-package
 transient
 autothemer
 csharp-mode
 csv-mode
 dash
 dired-hacks
 dired-hacks-utils
 dired+
 eimp
 async
 request
 molokai-theme
 doom-themes
 kaolin-themes
 expand-region
 f
 fill-column-indicator
 glsl-mode
 go-mode
 graphviz-dot-mode
 helm
 (helm-org :type git :host github :repo "emacs-helm/helm-org")
 helm-projectile
 helm-descbinds
 json-mode
 rjsx-mode
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
 god-mode
 flycheck
 helm-flycheck
 desktop+
 all-the-icons
 epl
 json-reformat
 json-snatcher
 pkg-info
 org-super-agenda
 org-gcal
 calfw
 calfw-ical
 calfw-org
 with-editor
 docker-compose-mode
 dockerfile-mode
 docker
 nginx-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; requires
(setq load-prefer-newer t)
(setq inhibit-compacting-font-caches t) ;; Fixes hiccups on certain unicode characters

(require 'rysco-util)

(require 'doom-themes)
(doom-themes-org-config)
(require 'kaolin-themes)

(require 'all-the-icons)
(require 'async)

(require 'god-mode)
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(add-to-list 'god-exempt-major-modes 'monky-log-edit-mode)
(add-to-list 'god-exempt-major-modes 'rcirc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-loads
(rysco-autoloads
 (helm-configuration "helm-config" "helm-configuration autoload")
 (helm-info-info "helm-info" "helm-info-info autoload")
 (helm-source "helm-source" "helm-source autoload")
 (helm-projectile "helm-projectile" "helm-projectile autoload")
 (org-goto-calendar "org")
 (screenwriter-mode "screenwriter")
 (helm-screenwriter-init "helm-screenwriter"))

(require 'multi)
(require 's)
(require 'transient)
(require 'uniquify)
(require 'windmove)
(require 'rotate)
(require 'font-lock+)

(require 'run-assoc)
(setq associated-program-alist
      '(((lambda (file)
           (if (eq system-type 'windows-nt)
               (w32-shell-execute "open" (convert-standard-filename file))
             (start-process "open" nil "open" file)))
         "\\.*$")))

(add-hook 'dired-mode-hook
          (lambda ()
            (require 'dired+)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
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
(setq helm-completing-read-handlers-alist
      (append '((find-file . ido)
                (dired . ido))
              helm-completing-read-handlers-alist))
(add-to-list 'helm-boring-file-regexp-list "\\.org_archive$")

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

(setq org-export-allow-bind-keywords t
      org-log-done 'time
      org-enforce-todo-dependencies t
      org-todo-keywords '((sequence
                           "TODO(t)"
                           "NEXT(n)"
                           "NOW(q)"
                           "WAITING(w@)"
                           "DONE(d)")
                          (type
                           "INACTIVE(i)"
                           "CANCELLED(c@)"))
      org-use-fast-todo-selection 'expert
      org-log-into-drawer t

      org-super-agenda-groups
      '((:name "NOW"
               :todo "NOW"
               :order 0)
        (:name "NEXT/WAITING"
               :todo "NEXT"
               :todo "WAITING"
               :order 0)
        (:name "Important"
               :priority "A"
               :order 0)
        (:todo "INACTIVE"
               :order 8)
        (:auto-property "ProjectId"
                        :order 7))

      org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files . (:tag . "refile_target")))

      rysco-org-refile-targets
      '((org-agenda-files . (:tag . "refile_target")))

      org-agenda-show-inherited-tags nil
      org-reverse-note-order t

      markdown-asymmetric-header t
      markdown-header-scaling t
      markdown-command "pandoc --smart -r markdown_github -w html"
      graphviz-dot-auto-indent-on-braces nil
      graphviz-dot-auto-indent-on-semi nil
      graphviz-dot-auto-indent-on-newline nil
      git-commit-style-convention-checks nil)

;; Calfw display
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓
      cfw:fstring-period-start " "
      cfw:fstring-period-end " "
      cfw:face-item-separator-color "Gray23"
      cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap
      org-gcal-auto-archive nil)

(custom-set-variables '(org-startup-folded t))

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
(put 'narrow-to-region 'disabled nil)
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
      ispell-program-name "aspell"
      kill-do-not-save-duplicate t)

;;;;
(require 'rysco-transients)

;; Local config
(define-transient-command rysco-personal-transient ()
  "Personal" [])

(defun rysco--rememoize-mode-icons ()
  (interactive)
  ;; Note:  Need to restore and re-memoize this function in order to get the changes to take effect
  (memoize-restore 'all-the-icons-icon-for-mode)
  (memoize 'all-the-icons-icon-for-mode))

(defvar rysco-personal-transients nil "A list of items to be used to build the ryscomacs personal transient during local config loading")

(defun rysco-load-local-config (&optional init)
  (interactive)

  (load (expand-file-name "ryscomacs/config" user-emacs-directory) t t)

  (when rysco-personal-transients
    (transient-replace-suffix 'rysco-personal-transient '(0)
      (vconcat
       [:description
        (lambda ()
          (concat
           (all-the-icons-faicon "registered" :face `(:inherit rysco-main-transient-title :height 0.8 :underline nil))
           (propertize " Personal" 'face 'rysco-main-transient-title)
           "\n"))]
       (cl-loop for item in rysco-personal-transients vconcat (list item)))))

  (unless init
    (rysco--rememoize-mode-icons)))

(rysco-load-local-config t)
(rysco--rememoize-mode-icons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks/setups
(eval-after-load 'dash '(dash-enable-font-lock))

(defun rysco-help-movement-hook ()
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line))

(add-hook 'help-mode-hook 'rysco-help-movement-hook)
(add-hook 'Info-mode-hook 'rysco-help-movement-hook)

(defun eshell/w32-explorer-path ()
  (s-replace "/" "\\" (eshell/pwd)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq pcomplete-cycle-completions nil)

            (eshell/alias "d" "dired-other-window $1")
            (eshell/alias "ff" "find-file-other-window $1")
            (eshell/alias "dir" "ls $*")
            (eshell/alias "exp"
                          (if (string= system-type "windows-nt")
                              "cmd /C \"start explorer ${eshell/w32-explorer-path}\""
                            "open (eshell/pwd)"))

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

;; Disables auto-fill in commit message buffers
(add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)

;; flycheck
(when (eq system-type 'windows-nt)
  (defun rysco-fix-flycheck-cmd (cmd)
    `(,(car cmd)
      ,@(cl-loop for part in (cdr cmd) collect
                 (replace-regexp-in-string "/" "\\" part t t))))

  (custom-set-variables '(flycheck-command-wrapper-function 'rysco-fix-flycheck-cmd)))

;; Font 
(let ((font (concat rysco-font "-" rysco-font-size)))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font))

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function) ;We don't want buffers opened with emacsclient to give us that warning...

;; Do not use tabs
(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-common-hook
	  '(lambda () (c-set-style "stroustrup")
             (rysco-semantic-mode t)
	     (setq tab-width 4
	           indent-tabs-mode nil)
             (local-set-key (kbd "C-c o") 'ff-find-related-file-ignore-include)
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)
             (local-set-key (kbd "M-<RET>") 'indent-new-comment-line)))

(add-hook 'csharp-mode-hook
	  '(lambda ()
	     ;; for hide/show support
	     ;; (hs-minor-mode 1)
	     ;; (setq hs-isearch-open t)
             (rysco-semantic-mode t)
	     (c-set-style "c#")
	     (setq indent-tabs-mode nil
	           tab-width 4)
             
             ;; No idea why this isn't the default, but here we are...
             (local-set-key "{" 'c-electric-brace)
             (local-set-key "}" 'c-electric-brace)

             ;; with point inside the block, use these keys to hide/show
	     (local-set-key "\C-c>"  'hs-hide-block)
	     (local-set-key "\C-c<"  'hs-show-block)
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)
             (local-set-key (kbd "M-<RET>") 'indent-new-comment-line)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'rysco-comment-dwim)
             (local-set-key (kbd "M-<RET>") 'indent-new-comment-line)))

(add-hook 'lua-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-<RET>") 'indent-new-comment-line)))

;; HACK:  There's some funky alignment decisions in lua-mode that I don't want.
;; .This fixes things most of the way.  The notable exception being anonymous function indentations when 'function' starts on the line with a function call open paren
(with-eval-after-load "lua-mode"
  (defun rysco-lua-at-most-one-indent (old-function &rest arguments)
    (let ((old-res (apply old-function arguments)))
      (min old-res tab-width)))

  (defun rysco-lua-no-left-shifting ()
    t)

  (advice-add #'lua-calculate-indentation-block-modifier :around #'rysco-lua-at-most-one-indent)
  (advice-add #'lua-point-is-after-left-shifter-p :override #'rysco-lua-no-left-shifting))

(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
                   tab-width 4
                   js-indent-level 2)
             (local-set-key (kbd "M-.") 'find-tag)
             (local-set-key "\C-c\C-c" 'rysco-comment-dwim)
             (local-set-key (kbd "M-<RET>") 'indent-new-comment-line)))

(add-hook 'html-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-c" 'rysco-comment-dwim)))

(add-hook 'json-mode-hook
	  '(lambda ()
	     (setq-default indent-tabs-mode nil)
             (setq js-indent-level 2)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-<RET>") 'indent-new-comment-line)))

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
  (local-set-key (kbd (concat rysco-lead-key " SPC")) 'helm-org-in-buffer-headings)
  (electric-indent-local-mode -1)
  (setq org-adapt-indentation nil))

;; HACK:  I don't like way that this function would call org-show-entry at the end.
(with-eval-after-load "org"
  (require 'org-refile)

  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((python . t)
             (latex . t)
             (dot . t))))

  (defun rysco-helm-org-goto-marker (marker)
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))
    (org-show-set-visibility 'canonical)
    (re-search-backward "^\\*+ " nil t))

  (advice-add 'helm-org-goto-marker :override 'rysco-helm-org-goto-marker)

  (defadvice org-export-output-file-name (before org-add-export-dir activate)
    "Modifies org-export to place exported files in a different directory"
    (when (not pub-dir)
      (setq pub-dir org-export-directory)
      (when (not (file-directory-p pub-dir))
        (make-directory pub-dir)))))

(with-eval-after-load "ox-latex"
    (add-to-list 'org-latex-logfiles-extensions "tex"))

(custom-set-variables
 '(org-src-lang-modes
   '(("C" . c)
     ("C++" . c++)
     ("asymptote" . asy)
     ("bash" . sh)
     ("beamer" . latex)
     ("calc" . fundamental)
     ("cpp" . c++)
     ("ditaa" . artist)
     ("dot" . graphviz-dot)
     ("elisp" . emacs-lisp)
     ("ocaml" . tuareg)
     ("screen" . shell-script)
     ("shell" . sh)
     ("sqlite" . sql)))
 '(org-src-window-setup 'other-window))

(add-hook 'org-mode-hook 'rysco-org-hook)

(defun rysco-org-latex-export-list-newline-fixup (output)
  "Kills the trailing latex newline for any \\item lines"
  (replace-regexp-in-string
   "\\(\\item[ [\\]+.*\\)\\\\\\\\$" "\\1"
   output))

(with-eval-after-load 'ox-latex
  (advice-add
   #'org-latex-plain-list
   :filter-return #'rysco-org-latex-export-list-newline-fixup))

(defun rysco-org-babel-core-suppress-silent-echo (fun &rest args)
  (let ((inhibit-message t))
    (apply fun args)))

(with-eval-after-load 'ob-core
  (advice-add #'org-babel-insert-result :around #'rysco-org-babel-core-suppress-silent-echo))

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(with-eval-after-load 'org-colview
  (define-key org-columns-map "n" nil)
  (define-key org-columns-map "p" nil))

(with-eval-after-load 'org-agenda
  (require 'rysco-org)
  (org-super-agenda-mode 1)

  (copy-face 'org-level-2 'org-super-agenda-header )

  (advice-add 'helm-org-in-buffer-headings :before-until
            'helm-rysco-org-agenda-buffer-items)

  (advice-add 'helm-rysco-semantic-or-imenu :before-until
              'helm-rysco-org-agenda-buffer-items)

  (define-key org-agenda-mode-map "n" 'org-agenda-next-item)
  (define-key org-agenda-mode-map "p" 'org-agenda-previous-item)
  (define-key org-agenda-mode-map ")" 'rysco-org-agenda-goto-first-section)
  (define-key org-super-agenda-header-map ")" 'rysco-org-agenda-goto-first-section))

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

(setq 
  ido-save-directory-list-file nil ;"~/.emacs.d/ido.last"
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~/src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)

  ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t ; wait for RET, even with unique completion
  confirm-nonexistent-file-or-buffer nil) ; when using ido, the confirmation is rather annoying...

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
 ("." 'god-mode-all)
 (rysco-lead-key 'rysco-main-transient)
 ("<tab>" 'rysco-personal-transient)
 ("SPC" 'helm-rysco-semantic-or-imenu)
 ("<RET>" 'helm-mini)
 ("s" 'helm-rysco-occur-or-resume)
 ("w" 'helm-rysco-project-ag)
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
 ("-" 'rysco-frame-by-name)

 ;; Desktops
 ("b" 'rysco-desktop+-create)
 ("m" 'desktop+-load))

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
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Helpers for things that have a lot of muscle memory
(global-set-key (kbd "<escape>d") 'kill-word)
(global-set-key (kbd "<escape>DEL") 'backward-kill-word)

;; Unsetting things I don't use that accidentally get hit
(global-unset-key (kbd "C-x C-b"))

;; Timestamp insertion
(add-hook
 'calendar-mode-hook
 (lambda () (define-key calendar-mode-map (kbd "RET") 'rysco-calendar-exit-and-insert-date)))

;; Magit keys
(add-hook
 'magit-mode-hook
 (lambda ()
   (define-key magit-mode-map (kbd "C-o") 'magit-diff-visit-file-other-window)))

;;
(defun rysco-post-init-setup ()
  (unless (equal rysco-theme :none)
    (load-theme (or rysco-theme 'molokai)))

  (god-mode-all)
  (run-with-idle-timer
   0.25 nil
   (lambda ()
     (server-start)
     (bluedot-resume))))

;; NOTE:  Special thanks to Doom Emacs for the startup/GC tips
;; .https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

(defun rysco-startup-hook ()
  ;; Enable temporarily disabled features again
  (setq gc-cons-threshold 16777216 ; 16mb
        gc-cons-percentage 0.1
        file-name-handler-alist rysco-file-name-handler-alist))

(add-hook 'emacs-startup-hook 'rysco-startup-hook)
(add-hook 'after-init-hook 'rysco-post-init-setup)

;; Enabling GC as per usual
(defun rysco-defer-garbage-collection ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun rysco-restore-garbage-collection ()
  ;; Defer it so that commands launched immediately after will enjoy the benefits
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'rysco-defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'rysco-restore-garbage-collection)
;;;;

;;;;;;;;;;;;
(provide 'rysco-core)
