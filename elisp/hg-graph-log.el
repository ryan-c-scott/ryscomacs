(defgroup hg-graph-log-faces nil
  "Faces used in hg graph log Mode"
  :group 'hg-graph-log-faces
  :group 'faces)

(defface hg-graph-log-branch-1
  '((t (:inherit font-lock-variable-name-face)))
  "Face for branch depth 1."
  :group 'hg-graph-log-faces)

(defface hg-graph-log-branch-2
  '((t (:inherit font-lock-variable-name-face)))
  "Face for branch depth 2."
  :group 'hg-graph-log-faces)

(defface hg-graph-log-branch-3
  '((t (:inherit font-lock-variable-name-face)))
  "Face for branch depth 3."
  :group 'hg-graph-log-faces)

(defface hg-graph-log-branch-4
  '((t (:inherit font-lock-variable-name-face)))
  "Face for branch depth 4."
  :group 'hg-graph-log-faces)

(defface hg-graph-log-branch-5
  '((t (:inherit font-lock-variable-name-face)))
  "Face for branch depth 5."
  :group 'hg-graph-log-faces)

(defface hg-graph-log-branch-6
  '((t (:inherit font-lock-variable-name-face)))
  "Face for branch depth 6."
  :group 'hg-graph-log-faces)

(defface hg-graph-log-summary
  '((t (:inherit font-lock-variable-name-face)))
  "Face for changeset summary."
  :group 'hg-graph-log-faces)

(defvar hg-graph-log-branch-1 'hg-graph-log-branch-1)
(defvar hg-graph-log-branch-2 'hg-graph-log-branch-2)
(defvar hg-graph-log-branch-3 'hg-graph-log-branch-3)
(defvar hg-graph-log-branch-4 'hg-graph-log-branch-4)
(defvar hg-graph-log-branch-5 'hg-graph-log-branch-5)
(defvar hg-graph-log-branch-6 'hg-graph-log-branch-6)
(defvar hg-graph-log-summary 'hg-graph-log-summary)

(define-derived-mode hg-graph-log-mode fundamental-mode "HG Graph"
  "Major mode for viewing Mercurial glogs
\\{hg-graph-log-mode-map}
"
  (let ((depth-faces '(hg-graph-log-branch-1 hg-graph-log-branch-2 hg-graph-log-branch-3 hg-graph-log-branch-4 hg-graph-log-branch-5 hg-graph-log-branch-6))
	(locks '())
	(depth 0))

    (while depth-faces
      (let* ((this-face (car depth-faces))
	     (depth-prefix (format "^.\\{%d,%d\\}" (max (- depth 1) 0) depth)))
	(setq locks (append locks (list
				   (list (format "%s\\(o\\).*\\(changeset:.*\\)" depth-prefix) '(1 font-lock-function-name-face) `(2 ,this-face))
				   (list (format "%s\\([|\\/@]\\)" depth-prefix) `(1 ,this-face) )
				   (list (format "^\\(.\\)\\(%s\\)\\(o\\)" (make-string (max 0 (- depth 1)) ?-)) '(1 font-lock-function-name-face) `(2 ,this-face) '(3 font-lock-function-name-face)))))
	(setq depth (+ depth 2))
	(setq depth-faces (cdr depth-faces))))
    
    (setq font-lock-defaults
	  `( ,(append
	       locks
	       '(("summary\\: \\(.*\\)" (1 hg-graph-log-summary)))))))
  (font-lock-mode))

(provide 'hg-graph-log)
