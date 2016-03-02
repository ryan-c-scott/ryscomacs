(define-derived-mode hg-graph-log-mode fundamental-mode "HG Graph"
  "Major mode for viewing Mercurial glogs
\\{hg-graph-log-mode-map}
"
  (let ((depth-faces '(font-lock-keyword-face font-lock-doc-face font-lock-warning-face))
	(locks '())
	(depth 0))

    (while depth-faces
      (let* ((this-face (car depth-faces))
	     (depth-prefix (format "^.\\{%d,%d\\}" (max (- depth 1) 0) depth)))
	(setq locks (append locks (list
				   (list (format "%s\\(o\\).*\\(changeset:.*\\)" depth-prefix) '(1 font-lock-function-name-face) `(2 ,this-face))
				   (list (format "%s\\([|\\/@]\\)" depth-prefix) `(1 ,this-face) ))))
	(setq depth (+ depth 2))
	(setq depth-faces (cdr depth-faces))))
    
    (setq font-lock-defaults
	  `( ,(append
	       locks
	       '((" .*\\: " . font-lock-function-name-face))))))
  (font-lock-mode))

(provide 'hg-graph-log)
