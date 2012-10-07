(set-variable 'inhibit-splash-screen "True")

(setq make-backup-files nil)

(add-to-list 'load-path "~/ryscomacs/elisp" )

(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function) ;We don't want buffers opened with emacsclient to give us that warning...

; Wheel mouse setup
( global-set-key (quote [mouse-4]) 'scroll-down )
( global-set-key (quote [mouse-5]) 'scroll-up )
( global-set-key (quote [wheel-up]) 'scroll-down )
( global-set-key (quote [wheel-down]) 'scroll-up )

; PHP Mode
;(require 'php-mode)
;(add-hook 'php-mode-user-hook 'turn-on-font-lock)

; Random setting
( transient-mark-mode t )
( global-font-lock-mode t )
( line-number-mode t )
( normal-erase-is-backspace-mode 1 )
( show-paren-mode t )
( setq indent-tabs-mode nil )
( setq truncate-partial-width-windows nil )

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


; Bind various keys
(global-set-key "\eg" 'goto-line)
(global-set-key "\C-c \r" 'mark-defun)
(global-set-key "\C-c \t" 'indent-region)
(global-set-key (kbd "<C-tab>") 'complete-tag)

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


;; XSL Mode
(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; C# Mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

;; CG Mode
(autoload 'cg-mode "cg-mode" "Major mode for editing CG program code." t)

;; Json
(autoload 'json-mode "json-mode" "Major mode for editing CG json data." t)

;; Lua
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; PHP
(autoload 'php-mode "php-mode" "PHP editing mode." t)

;; Protobufs
(autoload 'protobuf-mode "protobuf-mode" "Protobuf editing mode." t)

;; Markdown Mode
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
        '("\\.json" . json-mode)
        '("\\.php" . php-mode)
        '("\\.proto" . protobuf-mode)
	'("\\.markdown$" . markdown-mode)
		)
       auto-mode-alist))


(add-hook 'c-mode-common-hook
	  '(lambda () (c-set-style "stroustrup")
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil)
	     ) )

(add-hook 'csharp-mode-hook
	  '(lambda ()
					; for hide/show support
	     (hs-minor-mode 1)
	     (setq hs-isearch-open t)
	     (c-set-style "c#")
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

;CTags setup
(defun create-tags (dir-name-raw)
  "Create tags file."
  (interactive "Directory: ")
  (setq dir-name (expand-file-name dir-name-raw) )
  (shell-command
   (format "find \"%s\" -iname \"*.cxx\" -or -iname \"*.hxx\" | etags -o \"%sTAGS\" -" dir-name dir-name))
  )

(defun nemo()
  "Tag Nemo"
  (interactive)
  (create-tags "~/Perforce/nemo/rscott_rcscott-pa_3364/Nemo/" )
  (visit-tags-table "~/Perforce/nemo/rscott_rcscott-pa_3364/Nemo/TAGS")
)

(defun dwarfs()
  "Tag Dwarfs"
  (interactive)
  (create-tags "~/src/Dwarfs/" )
  (visit-tags-table "~/src/Dwarfs/TAGS")
)

(defun merlin()
  "Tag Nemo"
  (interactive)
  (create-tags "~/src/merlin/" )
  (visit-tags-table "~/src/merlin/TAGS")
)

(defun muhproto()
  "Tag Muh Proto"
  (interactive)
  (create-tags "~/src/Muh-Protos/" )
  (visit-tags-table "~/src/Muh-Protos/TAGS")
)

(defun vltags()
  "Tag VL"
  (interactive)
  ;(create-tags "~/vl/src/")
  (setq dir-name (expand-file-name "~/vl/src/") )
  (shell-command
   (format "find \"%s\" -iname \"*.cpp\" -or -iname \"*.h\" | etags -o \"%sTAGS\" -" dir-name dir-name))
  (visit-tags-table "~/vl/src/TAGS")
)

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


; IDO buffer switching crap
(require 'ido) 
(ido-mode 'both) ;; for buffers and files
(setq 
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

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