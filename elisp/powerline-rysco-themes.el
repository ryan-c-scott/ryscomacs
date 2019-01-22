(defface powerline-rysco-active1 '((t (:background "grey22" :foreground "white" :weight normal)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-rysco-backing '((t (:foreground "black" :background "dimgray" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-backing-modified '((t (:foreground "black" :background "lightgray" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-backing-ro '((t (:foreground "black" :background "firebrick4" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-buffer-id '((t (:foreground "white" :background "Turquoise4" :weight normal)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-bluedot '((t (:foreground "#Fafad2" :weight bold)))
	 "Powerline face 3."
	 :group 'powerline)

(defun powerline-rysco-minor-modes ()
  (s-join "•"
          (cl-loop for m in (s-split " " (format-mode-line minor-mode-alist))
                   if (not (member m '("" "Helm" "Projectile[-]" "ElDoc"))) collect
                   (s-replace "Projectile" "" m))))

(defun powerline-rysco-vc (face justification)
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (concat
     (powerline-raw
      (format "%s%s"
              (propertize
               (all-the-icons-octicon "git-commit" :face `(:inherit ,face :height 0.8))
               'display '(raise 0))

              (replace-regexp-in-string
               " \\(Hg\\|Git\\)[:-]" ""
               (substring-no-properties vc-mode)))
      face justification))))

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
	     (separator-left (intern "powerline-arrow-left"))
	     (separator-left-secondary (intern "powerline-rounded-left"))
	     (separator-right (intern "powerline-rounded-right"))
	     (separator-right-secondary (intern "powerline-bar-right"))

	     (lhs
	      (list
	       (powerline-raw "%z" face3 'l)

	       ;(powerline-raw "%*" nil 'l)
	       ;; 	 (powerline-buffer-size nil 'l))
	       ;; 	 (powerline-raw mode-line-mule-info nil 'l))
	       (powerline-buffer-id face3 'l)
	       (when (and (boundp 'which-func-mode) which-func-mode)
		 (powerline-raw which-func-format nil 'l))

               (powerline-raw " " face3)

	       (funcall separator-left face3 face1)

	       (when (boundp 'erc-modified-channels-object)
		 (powerline-raw erc-modified-channels-object face1 'l))

               (--when-let (all-the-icons-icon-for-mode major-mode :face `(:inherit ,face1 :height 0.8))
                 (unless (equal it major-mode)
                   (powerline-raw (propertize it 'display '(raise 0)) face1 'l)))
               
	       (powerline-major-mode face1 'l)
	       (powerline-process face1)
               (when (buffer-narrowed-p)
                 (--when-let (all-the-icons-faicon "compress" :face `(:inherit ,face1 :height 0.8))
	         (powerline-raw (propertize it 'display '(raise 0)) face1 'l)))
	       (powerline-raw " " face1)

	       (funcall separator-left-secondary face1 face2)
	       (powerline-raw (powerline-rysco-minor-modes) face2 'l)
	       (powerline-rysco-vc face2 'l)))

	     (rhs
	      (list
	       (powerline-raw global-mode-string face2 'r)

	       (funcall separator-right-secondary face2 face1)

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

               (when bluedot-mode
                 (powerline-raw bluedot--current-bar 'powerline-rysco-bluedot 'r)))))

	(concat (powerline-render lhs)
		(powerline-fill face2 (powerline-width rhs))
		(powerline-render rhs)))))))

(provide 'powerline-rysco-themes)
