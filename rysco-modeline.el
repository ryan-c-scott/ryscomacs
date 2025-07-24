(require 'dash)

(defvar rysco-modeline-mode-blacklist
  '("" "Helm" "Projectile[-]" "Projectile" "ElDoc" "God" "WK" "ARev" "GCMH" "Abbrev"))

(defvar rysco-modeline-show-project t)
(defvar rysco-modeline-show-mode-name t)

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

(defface rysco-modeline-vc
  '((default
     (:weight bold))
    (((background dark))
     (:foreground "dimgray"
      :underline nil))
    (((background light))
     (:foreground "dimgray"
      :underline nil)))
  ""
  :group 'rysco-modeline)

;; NOTE: Borrowed from Doom
(defvar rysco-modeline-current-window (frame-selected-window)
  "Current window.")

;; NOTE: Borrowed from Doom
(defun rysco-modeline--active ()
  "Whether is an active window."
  (unless (and (bound-and-true-p mini-frame-frame)
               (and (frame-live-p mini-frame-frame)
                    (frame-visible-p mini-frame-frame)))
    (and rysco-modeline-current-window
         (eq (frame-selected-window) rysco-modeline-current-window))))

;; NOTE: Borrowed from Doom
(defun rysco-modeline-string-pixel-width (str)
  "Return the width of STR in pixels."
  ;; HACK: Commented out the string-pixel-width usage as it was causing lag in line number updating for some reason
  ;; (if (fboundp 'string-pixel-width)
      ;; (string-pixel-width str)
    (* (string-width str) (window-font-width nil 'mode-line) 1.05)
    ;; )
  )

(defun rysco-modeline-set-selected-window ()
  (setq rysco-modeline-current-window (selected-window)))

(defun rysco-modeline-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'rysco-modeline-set-selected-window)
(add-hook 'buffer-list-update-hook 'rysco-modeline-update-all)

(defsubst rysco-modeline-vc ()
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (format "%s%s"
            (propertize
             (all-the-icons-octicon
              "git-branch"
              :face `(:inherit ,current-face :height 0.8))
             'display '(raise 0))

            (propertize
             (concat
              " "
              (replace-regexp-in-string
               " \\(Hg\\|Git\\)[:-]" ""
               (substring-no-properties vc-mode)))
             'face current-face))))

(defsubst rysco-modeline-minor-modes ()
  (propertize
   (s-join
    "•"
    (cl-loop
     for m in (s-split " " (format-mode-line minor-mode-alist))
     if (not (member m rysco-modeline-mode-blacklist)) collect
     (pcase m
       ((rx "Projectile["
            (let proj (* anything))
            ":" (* anything) "]")
        (if rysco-modeline-show-project
            (concat "[" proj "]")
          ""))
       ("tree-sitter" "ts")
       (_ m))))
   'face current-face))

(defsubst rysco-modeline-column ()
  (when column-number-mode
    (propertize
     ":%C"
     'face current-face)))

(defsubst rysco-modeline-pos ()
  (let ((start-visible (equal (window-start) (point-min)))
        (end-visible (equal (window-end) (point-max))))
    (if (or start-visible end-visible)
        (progn
          (cl-incf width-adjustment 0.65)
          (concat
           (propertize "  " 'face current-face)
           (all-the-icons-material
            (cond
             ((and start-visible end-visible) "all_inclusive")
             (start-visible "vertical_align_top")
             (end-visible "vertical_align_bottom"))
            :face `(:inherit ,current-face :height 0.8))
           (propertize " " 'face current-face)))
      ;;
      (propertize
       (concat
        (format "%3s" (floor (* 100 (/ (float (window-end)) (point-max)))))
        "%% ")
       'face current-face))))

(defsubst rysco-modeline-major-mode ()
  (let ((icon (all-the-icons-icon-for-mode
               major-mode
               :face `(:inherit ,current-face :height 0.8))))
    (concat
     (when (not (eq icon major-mode))
       (propertize icon 'display '(raise 0)))
     (when rysco-modeline-show-mode-name
       (propertize
        (concat
         (when icon " ")
         (format-mode-line mode-name))
        'face `(:inherit ,current-face))))))

(defsubst rysco-modeline-process ()
  (format-mode-line mode-line-process))

(defsubst rysco-modeline-narrowed ()
  (when (buffer-narrowed-p)
    (--when-let (all-the-icons-faicon
                 "compress"
                 :face `(:inherit ,current-face :height 0.8))
      (concat
       (propertize it 'display '(raise 0))))))

(defsubst rysco-modeline-bluedot ()
  (when bluedot-mode
    (cl-incf width-adjustment 0.1)
    (format-mode-line
     bluedot--current-bar current-face)))

(defsubst rysco-modeline-read-only ()
  (when buffer-read-only
    (cl-incf width-adjustment 0.65)
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
                  ,width-adjustment
                  ,(* (/ 1.0 (frame-char-width))
                      (rysco-modeline-string-pixel-width (format-mode-line (cons "" rhs)))))))))

(defun rysco-modeline--section (segments)
  (cl-loop
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
                     (setq current-face face)))
                  (`(:width ,amount)
                   (cl-incf width-adjustment amount)))

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

(cl-defun rysco-modeline--render (&key left center right)
  (let* (current-face
         (is-active (rysco-modeline--active))
         (is-god (and (boundp god-local-mode)
                      god-local-mode))
         (is-modified (buffer-modified-p))
         (is-read-only buffer-read-only)

         (width-adjustment 0)
         (lhs (rysco-modeline--section left))

         (width-adjustment 0)
         (rhs (rysco-modeline--section right))
         (rhs-width-adjustment width-adjustment)

         (center-section (rysco-modeline--section center))
         )

    `(
      ,lhs
      ,center-section
      ,rhs
      )))

(defun rysco-modeline ()
  (interactive)
  (setq-default
   mode-line-format
   `(;"%e"
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
         rysco-modeline-process
         rysco-modeline-narrowed
         rysco-modeline-minor-modes
         (:active rysco-modeline-vc)
         rysco-modeline-vc
         )

       :center
       '(
         (:face rysco-modeline-backing)
         (:modified rysco-modeline-backing-modified)
         (:read-only rysco-modeline-backing-ro)
         rysco-modeline-center
         )

       :right
       '(
         mode-line-misc-info
         rysco-modeline-read-only
         (:face rysco-modeline-right)
         (:inactive nil)
         "%4l"
         rysco-modeline-column
         " "
         rysco-modeline-pos
         rysco-modeline-bluedot
       )
      ))
     )))

;;;;;;;;
(provide 'rysco-modeline)
