(defface rysco-modeline-buffer-id
  '((default
      (:slant italic
       :weight normal))
    (((background dark))
     (:foreground "white"
      :underline "Gray45"
      :overline "black"))
    (((background light))
     (:foreground "black"
      :underline "Gray45"
      :overline "black")))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-buffer-id-god
  '((default
     (:inherit rysco-modeline-buffer-id
      :weight bold))
    (((background dark))
     (:foreground "white"
      :background "Turquoise3"
      :underline "black"))
    (((background light))
     (:foreground "Gray85"
      :background "Turquoise4"
      :underline "white")))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-mode
  '((((background dark))
      (:foreground "white"))
    (((background light))
     (:foreground "black")))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-backing
  '((t
     (:foreground "black"
      :strike-through t
      :background nil
      :underline t)))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-backing-modified
  '((t
     (:inherit rysco-modeline-backing
      :foreground "gray")))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-backing-ro
  '((t
     (:inherit rysco-modeline-backing
      :foreground "DarkSlateGray")))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-right
  '((((background dark))
      (:background "black"))
    (((background light))
      (:background "white")))
  ""
  :group 'rysco-modeline)

(defsubst rysco-modeline-vc ()
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (format "%s%s"
            (propertize
             (all-the-icons-octicon
              "git-commit"
              :face `(:inherit ,current-face :height 0.8))
             'display '(raise 0))

            (propertize
             (replace-regexp-in-string
              " \\(Hg\\|Git\\)[:-]" ""
              (substring-no-properties vc-mode))
             'face current-face))))

(defsubst rysco-modeline-minor-modes ()
  (propertize
   (s-join
    "•"
    (cl-loop
     for m in (s-split " " (format-mode-line minor-mode-alist))
     if (not (member m '("" "Helm" "Projectile[-]" "ElDoc" "God" "ARev" "GCMH"))) collect
     (s-replace-regexp
      "Projectile\\[\\(.*\\):.*\\]"
      "[\\1]"
      m)))
   'face current-face))

(defsubst rysco-modeline-pos ()
  (let ((start-visible (equal (window-start) (point-min)))
        (end-visible (equal (window-end) (point-max))))
    (if (or start-visible end-visible)
        (concat
         (propertize "  " 'face current-face)
         (all-the-icons-material
          (cond
           ((and start-visible end-visible) "all_inclusive")
           (start-visible "vertical_align_top")
           (end-visible "vertical_align_bottom"))
          :face `(:inherit ,current-face :height 0.8))
         (propertize " " 'face current-face))
      ;;
      (propertize
       (concat
        (format "%3s" (floor (* 100 (/ (float (window-end)) (point-max)))))
        "%%")
       'face current-face))))

(defsubst rysco-modeline-major-mode ()
  (let ((icon (all-the-icons-icon-for-mode
               major-mode
               :face `(:inherit ,current-face :height 0.8))))
    (concat
     (when icon
       (propertize icon 'display '(raise 0)))
     (propertize
      (concat
       (when icon " ")
       (format-mode-line mode-name))
      'face `(:inherit ,current-face)))))

(defsubst rysco-modeline-narrowed ()
  (when (buffer-narrowed-p)
    (--when-let (all-the-icons-faicon
                 "compress"
                 :face `(:inherit ,current-face :height 0.8))
      (concat
       (propertize it 'display '(raise 0))))))

(defsubst rysco-modeline-bluedot ()
  (when bluedot-mode
    (format-mode-line
     (propertize bluedot--current-bar 'face current-face))))

(defsubst rysco-modeline-read-only ()
  (when buffer-read-only
    (propertize
     (all-the-icons-material
      "error_outline"
      :face `(:inherit ,current-face :height 0.8))
     'display '(raise -0.175))))

(defun rysco-modeline-center ()
  (propertize
   " "
   'face current-face
   'display `((space
               :align-to
               (- (+ right right-fringe right-margin)
                  ,(* (let ((width (doom-modeline--font-width)))
                        (or (and (= width 1) 1)
                            (/ width (frame-char-width) 1.0)))
                      (string-width
                       (format-mode-line (cons "" rhs)))))))))

(defun rysco-modeline--section (segments)
  (loop
   for part in segments

   as handled = (pcase part
                  (`(:face ,face)
                   (setq current-face face))
                  (`(:active ,face)
                   (when is-active
                     (setq current-face face)))
                  (`(:inactive ,face)
                   (unless (or is-active is-god)
                     (setq current-face face)))
                  (`(:god ,face)
                   (when is-god
                     (setq current-face face)))
                  (`(:modified ,face)
                   (when is-modified
                     (setq current-face face)))
                  (`(:read-only ,face)
                   (when is-read-only
                     (setq current-face face))))

   unless handled collect
   (pcase part
     ((pred functionp)
      `(:eval ,((lambda ()
                  (--when-let (funcall part)
                    (concat
                     it
                     (propertize " " 'face current-face)))))))
     (_
      (format-mode-line part current-face)))))

;; HACK:  Using helpers from doom-modeline
(require 'doom-modeline-core)
;;;;

(cl-defun rysco-modeline--render (&key left center right)
  (let* (current-face
         (center-face center)
         (is-active (doom-modeline--active))
         (is-god (and (boundp god-local-mode)
                      god-local-mode))
         (is-modified (buffer-modified-p))
         (is-read-only buffer-read-only)

         (lhs (rysco-modeline--section left))
         (rhs (rysco-modeline--section right))
         (center-section (rysco-modeline--section center)))
    
    `(,lhs
      ,center-section
      ,rhs)))

(defun rysco-modeline ()
  (interactive)
  (setq-default
   mode-line-format
   `("%e"
     (:eval
      (rysco-modeline--render
       :left
       '((:active rysco-modeline-buffer-id)
         (:god rysco-modeline-buffer-id-god)
         " %z "
         (:eval (buffer-name (current-buffer))) " "
         (:face nil)
         (:active rysco-modeline-mode)
         " "
         rysco-modeline-major-mode
         (:face nil)
         rysco-modeline-narrowed
         rysco-modeline-minor-modes
         rysco-modeline-vc)

       :center
       '((:face 'rysco-modeline-backing)
         (:modified 'rysco-modeline-backing-modified)
         (:read-only 'rysco-modeline-backing-ro)
         rysco-modeline-center)

       :right
       '(rysco-modeline-read-only
         (:face rysco-modeline-right)
         (:inactive nil)
         "%4l "
         rysco-modeline-pos
         mode-line-misc-info
         " "))))))

;;;;;;;;
(provide 'rysco-modeline)
