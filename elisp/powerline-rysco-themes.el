;; NOTE:  eval-defun will handle re-evaluating a defface immediately
;;(defface powerline-rysco-backing '((t (:foreground "black" :background "firebrick4"))) "" :group 'powerline)
;;(defface powerline-rysco-backing '((t (:foreground "black" :background "IndianRed4"))) "" :group 'powerline)
;;(defface powerline-rysco-backing '((t (:foreground "black" :background "goldenrod"))) "" :group 'powerline)

(defface powerline-rysco-active1 '((t (:background "grey22" :foreground "white" :weight normal)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-rysco-backing '((t (:foreground "black" :background "dark slate grey" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-backing-modified '((t (:foreground "black" :background "firebrick4" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-backing-ro '((t (:foreground "black" :background "IndianRed4" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-buffer-id '((t (:foreground "white" :background "black" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defun powerline-rysco-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
	     (mode-line (if active 'mode-line 'mode-line-inactive))
	     (face1 (if active 'powerline-rysco-active1 'powerline-inactive1))
	     (face2 (cond ((buffer-modified-p) 'powerline-rysco-backing-modified)
			  (buffer-read-only 'powerline-rysco-backing-ro)
			  (t 'powerline-rysco-backing)))
	     (face3 'powerline-rysco-buffer-id)
	     (separator-left (intern (format "powerline-%s-%s"
					     (powerline-current-separator)
					     (car powerline-default-separator-dir))))
	     (separator-right (intern (format "powerline-%s-%s"
					      (powerline-current-separator)
					      (cdr powerline-default-separator-dir))))

	     (lhs
	      (list
	       (powerline-raw "%z" nil 'l)
	       ;(powerline-raw "%*" nil 'l)
	       ;; 	 (powerline-buffer-size nil 'l))
	       ;; 	 (powerline-raw mode-line-mule-info nil 'l))
	       (powerline-buffer-id face3 'l)
	       (when (and (boundp 'which-func-mode) which-func-mode)
		 (powerline-raw which-func-format nil 'l))
	       (powerline-raw " ")
	       (funcall separator-left mode-line face1)
	       (when (boundp 'erc-modified-channels-object)
		 (powerline-raw erc-modified-channels-object face1 'l))
	       (powerline-major-mode face1 'l)
	       (powerline-process face1)
	       (powerline-narrow face1 'l)
	       (powerline-raw " " face1)
	       (funcall separator-left face1 face2)
	       (powerline-vc face2 'r)))

	     (rhs
	      (list
	       (powerline-raw global-mode-string face2 'r)
	       (funcall separator-right face2 face1)
	       (unless window-system
	       	 (powerline-raw (char-to-string #xe0a1) face1 'l))
	       (powerline-raw "%4l " face1 'l)
	       ;; (powerline-raw ":" face1 'l)
	       ;; (powerline-raw "%c" face1 'l)
	       (funcall separator-right face1 mode-line)
	       (powerline-raw " ")
	       (powerline-raw "%6p" nil 'r)
	       ;; (when powerline-display-hud
	       ;; 	 (powerline-hud face2 face1))
	       )))

	(concat (powerline-render lhs)
		(powerline-fill face2 (powerline-width rhs))
		(powerline-render rhs)))))))

(provide 'powerline-rysco-themes)
