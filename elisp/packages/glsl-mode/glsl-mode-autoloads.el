;;; glsl-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "glsl-mode" "glsl-mode.el" (0 0 0 0))
;;; Generated autoloads from glsl-mode.el

(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))

(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

(autoload 'glsl-mode "glsl-mode" "\
Major mode for editing GLSL shader files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "glsl-mode" '("gl-version" "glsl-")))

;;;***

(provide 'glsl-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; glsl-mode-autoloads.el ends here
