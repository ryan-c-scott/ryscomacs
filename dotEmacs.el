(set-variable 'inhibit-splash-screen "True")
( tool-bar-mode -1 )
(setq make-backup-files nil)

(add-to-list 'load-path "~/ryscomacs/elisp" )
(add-to-list 'load-path "~/.emacs.d/elisp" )

(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function) ;We don't want buffers opened with emacsclient to give us that warning...

; Wheel mouse setup
( global-set-key (quote [mouse-4]) 'scroll-down )
( global-set-key (quote [mouse-5]) 'scroll-up )
( global-set-key (quote [wheel-up]) 'scroll-down )
( global-set-key (quote [wheel-down]) 'scroll-up )

; Font settings
( set-face-attribute 'default t :font "Consolas-10.0" )

; Random setting
( transient-mark-mode t )
( global-font-lock-mode t )
( line-number-mode t )
( normal-erase-is-backspace-mode 1 )
( show-paren-mode t )
( setq indent-tabs-mode nil )
( setq truncate-partial-width-windows nil )
( setq ring-bell-function 'ignore )
( setq eshell-prefer-lisp-functions t )

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'windmove)
(windmove-default-keybindings 'meta)

; Bind various keys
(global-set-key "\eg" 'goto-line)
(global-set-key "\C-c \r" 'mark-defun)
(global-set-key "\C-c \t" 'indent-region)
(global-set-key (kbd "<C-tab>") 'complete-tag)

(setq w32-enable-caps-lock nil)
(global-set-key (kbd "<capslock> <capslock>") 'switch-to-buffer )

(global-set-key (kbd "<capslock> <right>") 'split-window-right )
(global-set-key (kbd "<capslock> <down>") 'split-window-below )
(global-set-key (kbd "<capslock> <left>") 'delete-window )
(global-set-key (kbd "<capslock> <up>") 'delete-other-windows )

;; Windows specific setup for locales under cygwin
  (when (eq system-type 'windows-nt)
    (setenv "LANG" "C")
    )
;;

;; OSX Specific key bindings/fixes

;; RS: The below taken from http://defindit.com/readme_files/tom_emacs.txt
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; I think this allows my preferred mode map to continue working when
;; other minor modes are active. See my user-minor-mode-map define-key
;; bindings below.
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(defvar user-minor-mode-map (make-sparse-keymap) "user-minor-mode keymap.")

(define-minor-mode user-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t
  " user-keys"
  'user-minor-mode-map)


;; Turn user-minor-mode on/off 1/0 in the mini-buffer.
;; Oct 5 2009 Was 1 which was clearly a mistake. 

(defun user-minibuffer-setup-hook ()
  (user-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'user-minibuffer-setup-hook)

(user-minor-mode 1)(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [C-home] 'beginning-of-buffer)
(define-key global-map [C-end] 'end-of-buffer)

(define-key global-map [delete] 'delete-char)
(define-key global-map [kp-delete] 'delete-char)
(define-key global-map [backspace] 'delete-backward-char)

(define-key isearch-mode-map [backspace] 'isearch-delete-char) 

; now need to fix the mini buffer because of the above
(define-key minibuffer-local-map [delete] 'delete-char)
(define-key minibuffer-local-map [kp-delete] 'delete-char)
(define-key minibuffer-local-map [backspace] 'backward-delete-char)
(define-key minibuffer-local-map "\C-h" 'backward-delete-char)

(define-key user-minor-mode-map [delete] 'delete-char)
(define-key user-minor-mode-map [kp-delete] 'delete-char)
(define-key user-minor-mode-map [backspace] 'backward-delete-char)
;;;;;;;;;;;


;; Modes
(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(autoload 'cg-mode "cg-mode" "Major mode for editing CG program code." t)
(autoload 'json-mode "json-mode" "Major mode for editing json data." t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(autoload 'php-mode "php-mode" "PHP editing mode." t)
(autoload 'protobuf-mode "protobuf-mode" "Protobuf editing mode." t)
(autoload 'markdown-mode "markdown-mode" "Major mode for the Markdown format." t)

(setq auto-mode-alist
      (append
       (list
        '("\\.xsl" . xsl-mode)
        '("\\.xml$" . xml-mode)
        '("\\.rml$" . xml-mode)
        '("\\.css$" . css-mode)
        '("\\.rcss$" . css-mode)
        '("\\.cs" . csharp-mode)
        '("\\.cg" . cg-mode)
        '("\\.glsl$" . cg-mode)
        '("\\.shader" . cg-mode)
        '("\\.lua" . lua-mode)
        '("\\.particle" . lua-mode)
        '("\\.material" . lua-mode)
        '("\\.scene" . lua-mode)
        '("\\.deps" . lua-mode)
        '("\\.json" . json-mode)
        '("\\.php" . php-mode)
        '("\\.proto" . protobuf-mode)
	'("\\.markdown$" . markdown-mode)
	'("\\.md$" . markdown-mode)
        '("\\.scala$" . scala-mode)
		)
       auto-mode-alist))


(add-hook 'c-mode-common-hook
	  '(lambda () (c-set-style "stroustrup")
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil)
             ( local-set-key  (kbd "C-c o") 'ff-find-other-file )
	     ) )

(add-hook 'csharp-mode-hook
	  '(lambda ()
					; for hide/show support
	     (hs-minor-mode 1)
	     (setq hs-isearch-open t)
	     (c-set-style "c#")
	     (setq indent-tabs-mode t)
	     (setq tab-width 4)
					; with point inside the block, use these keys to hide/show
	     (local-set-key "\C-c>"  'hs-hide-block)
	     (local-set-key "\C-c<"  'hs-show-block)

	     ) )

(add-hook 'javascript-mode-hook
	  '(lambda ()
	     (setq-default indent-tabs-mode nil)
	     ) )

(add-hook 'json-mode-hook
	  '(lambda ()
	     (setq-default indent-tabs-mode nil)
             (setq js-indent-level 2)
	     ) )

(global-set-key "\C-h" 'backward-delete-char)

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/color-theme-6.6.0/" )
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)
;;;;;;;;;

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/ess-13.05/lisp/" )
(require 'ess-site)
;;;;;;;;;

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/nav/")
(require 'nav)
(nav-disable-overeager-window-splitting)
;; Optional: set up a quick key to toggle nav
(global-set-key [f8] 'nav-toggle)
;;;;;;;;;

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/helm/")
(require 'helm-config)
(global-set-key (kbd "<capslock> SPC") 'helm-mini )
(helm-mode 1)
;;;;;;;;;

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/scala-mode2/")
(require 'scala-mode2)
;;;;;;;;;

;;;;;;;;;
(require 'p4)
;;;;;;;;;

;;;;;;;;;
(add-to-list 'load-path "~/ryscomacs/elisp/expand-region/")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;;;;;;;;;

;;;;;;;;; Customizing colors used in diff mode
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :background "grey18" :foreground "green")
  (set-face-attribute
   'diff-removed nil :background "grey13" :foreground "red3")
  (set-face-attribute
   'diff-changed nil :background "grey5" :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))
;;;;;;;;;

;CTags setup
(defun tag-dir( dirpath subdirs )
  "Run ctags against the specified directories"
  (let ((default-directory dirpath))
    (shell-command
     (concat "ctags -R -e --extra=qf -o " (mapconcat 'expand-file-name subdirs " ")) )
    (visit-tags-table (expand-file-name "TAGS")) ))

(defun vltags()
  "Tag VL"
  (interactive)
  (tag-dir "~/vl" '( "TAGS" "src" "Content/scripts" )) )

(defun replace-regexp-and-return (from to)
  (save-excursion
    (while (re-search-forward from nil t)
      (replace-match to))))

(defun json-format()
  "Format Json Data"
  (interactive)
  (goto-char (point-min))
  (replace-regexp-and-return "{" "{\n")
  (replace-regexp-and-return "}" "\n}\n")
  (replace-regexp-and-return "," ",\n")

  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil) )
  )

(defun json-unformat()
  "Format Json Data"
  (interactive)
  (goto-char (point-min))
  (replace-regexp-and-return "\n" "")
  )

(defun insert-standard-date ()
  "Inserts standard date time string." 
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))


; IDO buffer switching crap
(require 'ido) 
(ido-mode 'both) ;; for buffers and files
(setq 
  ido-save-directory-list-file "~/.emacs.d/ido.last"

  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*\.meta"
     "^\*compilation" "^\*TAGS" "^session\.*" "^\*")
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
(load "localprojects" :missing-ok t)