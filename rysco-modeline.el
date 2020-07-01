(defface rysco-modeline-buffer-id '((t (:foreground "white" :background "Turquoise4" :weight normal :slant normal :box nil)))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-buffer-id-god '((t (:inherit rysco-modeline-buffer-id :foreground "Gray13" :background "goldenrod" :weight bold)))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-right '((t (:background nil :box nil)))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-bluedot '((t (:inherit rysco-modeline-right :height 0.95 :box t)))
  ""
  :group 'rysco-modeline)

(defsubst rysco-modeline-vc ()
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (format "%s%s"
            (propertize
             (all-the-icons-octicon "git-commit" :face `(:inherit ,current-face :height 0.8))
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
         (all-the-icons-material
          (cond
           ((and start-visible end-visible) "all_inclusive")
           (start-visible "vertical_align_top")
           (end-visible "vertical_align_bottom"))
          :face `(:inherit ,current-face :height 0.8))
       ;;
       (propertize "%p%% " 'face current-face))))

(defsubst rysco-modeline-major-mode ()
  (let ((icon (all-the-icons-icon-for-mode major-mode :face `(:inherit ,current-face :height 0.8))))
    (concat
     (when icon (propertize icon 'display '(raise 0)))
     (propertize
      (concat
       (when icon " ")
       mode-name)
      'face `(:inherit ,current-face)))))

(defsubst rysco-modeline-narrowed ()
  (when (buffer-narrowed-p)
    (--when-let (all-the-icons-faicon "compress" :face `(:inherit ,current-face :height 0.8))
      (concat
       (propertize it 'display '(raise 0))))))

(defsubst rysco-modeline-bluedot ()
  ;; (when bluedot-mode
  (propertize bluedot--current-bar 'face current-face)) ; 'face 'powerline-rysco-bluedot))
;; )

(defsubst rysco-modeline-read-only ()
  (when buffer-read-only
    (concat
     (propertize
      (all-the-icons-material
       "error_outline"
       :face `(:inherit ,current-face :height 0.8))
      'display '(raise -0.175))
     " ")))

(defun rysco-modeline--section (segments)
  (loop
   with active-face
   with inactive-face
   with god-face

   with is-active = (eq powerline-selected-window (selected-window))
   with is-god = (and (boundp god-local-mode)
                      god-local-mode)

   for part in segments
   as handled = (pcase part
                  (`(:active ,face)
                   (setq active-face face))
                  (`(:inactive ,face)
                   (setq inactive-face))
                  (`(:god ,face)
                   (setq god-face face)))

   as current-face = (if is-active
                         (if is-god
                             god-face
                           active-face)
                       inactive-face)

   unless handled concat
   (if (functionp part)
       (format-mode-line (funcall part))
     (format-mode-line part current-face))))

(defun rysco-modeline--render (sections)
  (loop
   for part in sections collect
   (rysco-modeline--section part)))

(defun rysco-modeline ()
  (interactive)
  (setq mode-line-format
        `("%e"
          (:eval
           (rysco-modeline--section
            '(
              (:active nil)
              (:active rysco-modeline-buffer-id)
              (:god rysco-modeline-buffer-id-god)
              " %z "
              ;; mode-line-modified
              ;; mode-line-remote
              ;; " "
              (:eval (buffer-name (current-buffer)))
              " "
              (:active nil)
              (:god nil)
              rysco-modeline-narrowed
              " "
              rysco-modeline-major-mode
              " "
              rysco-modeline-minor-modes
              " "
              rysco-modeline-vc)))

          (:eval
           (rysco-modeline--section
            '("       "
              (:active rysco-modeline-right)
              rysco-modeline-read-only
              "%4l "
              rysco-modeline-pos

              mode-line-misc-info
              " "
              (:active rysco-modeline-bluedot)
              rysco-modeline-bluedot

              (:active rysco-modeline-right)
              "  "
              mode-line-end-spaces))))))

;;;;;;;;
(provide 'rysco-modeline)
