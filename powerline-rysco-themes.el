(defface powerline-rysco-active1 '((t (:background "grey22" :foreground "white" :weight normal :box nil)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-rysco-inactive1 '((t (:background "grey22" :foreground "grey45" :weight normal :box nil)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-rysco-backing '((t (:foreground "black" :background "dimgray" :weight normal :box nil)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-backing-modified '((t (:foreground "black" :background "lightgray" :weight normal :box nil)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-backing-ro '((t (:foreground "black" :background "dimgray" :weight normal :box nil)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-buffer-id '((t (:foreground "white" :background "Turquoise4" :weight normal :slant italic :box nil)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-buffer-id-god '((t (:inherit powerline-rysco-buffer-id :foreground "Gray13" :background "goldenrod" :weight bold)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-bluedot '((t (:background "black" :height 0.95 :box nil)))
	 "Powerline face 3."
	 :group 'powerline)

(defface powerline-rysco-right '((t (:background "black" :box nil :box nil)))
  ""
  :group 'powerline)

(defun powerline-rysco-minor-modes ()
  (s-join "•"
          (cl-loop for m in (s-split " " (format-mode-line minor-mode-alist))
                   if (not (member m '("" "Helm" "Projectile[-]" "ElDoc" "God" "ARev"))) collect
                   (s-replace-regexp
                    "Projectile\\[\\(.*\\):.*\\]"
                    "[\\1]"
                    m))))

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

(defun powerline-rysco-pos (&optional face)
  (let ((start-visible (equal (window-start) (point-min)))
        (end-visible (equal (window-end) (point-max))))
    (if (or start-visible end-visible)
        (concat
         (powerline-raw
          (all-the-icons-material 
           (cond
            ((and start-visible end-visible) "all_inclusive")
            (start-visible "vertical_align_top")
            (end-visible "vertical_align_bottom"))
           :face `(:inherit ,face :height 0.8))
          face 'l)
         (powerline-raw " " face 'r))
      ;;
      (powerline-raw "%p" face 'r))))

(defun powerline-rysco-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
	     (face1 'powerline-rysco-inactive1)
	     (face2 (cond ((buffer-modified-p) 'powerline-rysco-backing-modified)
			  (buffer-read-only 'powerline-rysco-backing-ro)
			  (t 'powerline-rysco-backing)))
	     (face3 (if (and (boundp god-local-mode) god-local-mode) 'powerline-rysco-buffer-id-god 'powerline-rysco-buffer-id))
             (face-theme 'powerline-rysco-right)
             (active-text (if active 'powerline-rysco-active1 'powerline-rysco-inactive1))
	     (separator-left (intern "powerline-arrow-left"))
	     (separator-left-secondary (intern "powerline-rounded-left"))
	     (separator-right (intern "powerline-rounded-right"))
	     (separator-right-secondary (intern "powerline-bar-right"))
             (padding-hack 0)

	     (lhs
	      (list
	       (powerline-raw "%z" face3 'l)
	       (powerline-buffer-id face3 'l)
	       (when (and (boundp 'which-func-mode) which-func-mode)
		 (powerline-raw which-func-format nil 'l))

               (powerline-raw " " face3)

	       (funcall separator-left face3 face1)

	       (when (boundp 'erc-modified-channels-object)
		 (powerline-raw erc-modified-channels-object face1 'l))

               (--when-let (all-the-icons-icon-for-mode major-mode :face `(:inherit ,active-text :height 0.8))
                 (unless (equal it major-mode)
                   (powerline-raw (propertize it 'display '(raise 0)) active-text 'l)))
               
	       (powerline-major-mode active-text 'l)
	       (powerline-process active-text)
               (when (buffer-narrowed-p)
                 (--when-let (all-the-icons-faicon "compress" :face `(:inherit ,active-text :height 0.8))
	         (powerline-raw (propertize it 'display '(raise 0)) active-text 'l)))
	       (powerline-raw " " active-text)

	       (funcall separator-left-secondary face1 face2)
	       (powerline-raw (powerline-rysco-minor-modes) face2 'l)
	       (powerline-rysco-vc face2 'l)))

	     (rhs
	      (list
               (--when-let rysco-modeline-extras
                 (powerline-raw
                  (cl-loop
                   for (type form) in (cl-loop for (group . data) in it append data) concat
                   (concat " "
                           (eval
                            (cond
                             ((equal type :eval)
                              form)

                             ((equal type :icon)
                              ;; HACK:
                              (cl-incf padding-hack)
                              
                              `(propertize
                                (,(plist-get form :family)
                                 ,(plist-get form :icon)
                                 :face `(:inherit ,face :height ,(plist-get form :height)))
                                'display '(raise ,(plist-get form :raise))))

                             (t ""))

                            `((face ,face2)))))
                  face2 'r))

               (when buffer-read-only
                 ;; HACK:
                 (cl-incf padding-hack 2)
                 
                 (powerline-raw
                  (format "%s "
                          (propertize
                           (all-the-icons-material
                            "error_outline"
                            :face `(:inherit ,face2 :foreground "white" :height 0.8))
                           'display '(raise -0.175)))
                  face2 'l))

	       (funcall separator-right-secondary face2 face1)

	       (unless window-system
	       	 (powerline-raw (char-to-string #xe0a1) face1 'l))

               (powerline-raw "%4l " face1 'l)
	       ;; (powerline-raw ":" face1 'l)
	       ;; (powerline-raw "%c" face1 'l)

               (funcall separator-right face1 face-theme)
               (powerline-raw " " face-theme)

               ;; TODO:  Look at ways to handle this more cleanly.
               (powerline-raw
                (mapcar (lambda (el)
                          (propertize
                           (if (symbolp el)
                               (string-trim (eval el))
                             (string-trim el))
                           'face face-theme))
                        global-mode-string)
                face-theme 'r)

               (powerline-rysco-pos face-theme)
	       ;; (when powerline-display-hud
	       ;; 	 (powerline-hud face2 face1))

               (when bluedot-mode
                 (powerline-raw bluedot--current-bar 'powerline-rysco-bluedot 'r)))))

        ;; TODO:  Clean this up
        (let ((space (-
                      (window-width)
                      (+
                       (powerline-width lhs)
                       (powerline-width rhs)))))
	  (concat
           (if (< space 0)
               (let* ((char-width-modeline
                       (powerline-width
                        `(,(powerline-raw " " face2))))
                      (trunc (ceiling (abs (/ space char-width-modeline)))))
                 (concat
                  (substring (powerline-render lhs) 0 (* -1 trunc))
                  (powerline-render
                   `(,(powerline-raw
                       "…"
                       face2)))))
             (powerline-render lhs))

	   (powerline-fill face2 (+ (powerline-width rhs) (/ padding-hack 2)))
	   (powerline-render rhs))))))))

;;;; Misc.
(setq org-clock-string-limit 7
      bluedot-modeline-face 'powerline-rysco-bluedot)

(provide 'powerline-rysco-themes)
