(defface rysco-modeline-buffer-id
  '((t
     (:foreground "white"
      :background "Turquoise4"
      :weight bold
      :slant italic
      :underline "white"
      :overline "black")))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-buffer-id-god
  '((t
     (:inherit rysco-modeline-buffer-id
      :foreground "Gray13"
      :background "goldenrod"
      :underline "black")))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-backing
  '((t
     (:foreground "black"
      :strike-through t
      :background nil
      :underline t
      )))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-backing-modified
  '((t
     (:inherit rysco-modeline-backing
      :foreground "gray"
      )))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-backing-ro
  '((t
     (:inherit rysco-modeline-backing
      :foreground "DarkSlateGray"
      )))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-right
  '((t
     (:background "black"
      :box nil)))
  ""
  :group 'rysco-modeline)

(defface rysco-modeline-bluedot
  '((t
     (:inherit rysco-modeline-right
      :height 0.95
      :box nil)))
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
             (concat
              (replace-regexp-in-string
               " \\(Hg\\|Git\\)[:-]" ""
               (substring-no-properties vc-mode))
              " ")
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
  (let ((icon (all-the-icons-icon-for-mode
               major-mode
               :face `(:inherit ,current-face :height 0.8))))
    (concat
     (when icon (propertize icon 'display '(raise 0)))
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
   (make-string center-width ?\s)
   'face
   (cond
    ;; TODO:  These faces should be set in the mode spec and not hardcoded here
    (buffer-read-only 'rysco-modeline-backing-ro)
    ((buffer-modified-p) 'rysco-modeline-backing-modified)
    (t 'rysco-modeline-backing))))

(defun rysco-modeline--section (segments)
  (loop
   for part in segments

   as handled = (pcase part
                  (`(:active ,face)
                   (when is-active
                     (setq current-face face)))
                  (`(:inactive ,face)
                   (unless (or is-active is-god)
                     (setq current-face face)))
                  (`(:god ,face)
                   (when is-god
                     (setq current-face face))))

   unless handled concat
   (if (functionp part)
       (format-mode-line (funcall part))
     (format-mode-line part current-face))))

(cl-defun rysco-modeline--render (&key left center right)
  (let* ((center-face center)
         (is-active (eq powerline-selected-window (selected-window)))
         (is-god (and (boundp god-local-mode)
                      god-local-mode))

         (lhs (rysco-modeline--section left))
         (rhs (rysco-modeline--section right))
         (lhs-width (string-width lhs))
         (rhs-width (string-width rhs))
         (center-width (- (window-width)
                          (+ lhs-width rhs-width))))
    
    `((:eval ,lhs)
      ,(rysco-modeline-center)
      (:eval ,rhs))))

(defun rysco-modeline ()
  (interactive)
  (setq
   mode-line-format
   `("%e"
     (:eval
      (rysco-modeline--render
       :center 'rysco-modeline-backing
       :left
       '((:active rysco-modeline-buffer-id)
         (:god rysco-modeline-buffer-id-god)
         " %z "
         (:eval (buffer-name (current-buffer)))
         " "
         (:inactive nil)
         (:active nil)
         (:god nil)
         " "
         rysco-modeline-major-mode
         " "
         rysco-modeline-narrowed
         " "
         rysco-modeline-minor-modes
         " "
         rysco-modeline-vc)

       :right
       '(rysco-modeline-read-only
         (:active rysco-modeline-right)
         "%4l "
         rysco-modeline-pos

         " "
         (:active rysco-modeline-right)
         mode-line-misc-info
         "  "
         ;; mode-line-end-spaces
         ))))))

;;;;;;;;
(provide 'rysco-modeline)