;;; -*- lexical-binding: t; -*-

;;; bluedot.el --- Small pomodoro timer (1 char)

;; Author: Ryon C. Scott
;; URL: 
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: calendar

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; This package provides a little pomodoro timer in the mode-line.
;;
;; After importing, it shows a little red tick (✓) in the mode-line.
;; When you click on it, it starts a pomodoro timer.
;;
;; This is a heavily modified fork of redtick by F. Febles (http://github.com/ferfebles/redtick)
;;; Code:

(require 'rysco-util)
(require 'helm-source)

(defgroup bluedot nil
  "Little pomodoro timer in the mode-line."
  :group 'tools
  :prefix "bluedot-")

;; pomodoro work & rest intervals in seconds
(defcustom bluedot-work-interval (* 60 25)
  "Interval of time you will be working, in seconds."
  :type 'number)
(defcustom bluedot-rest-interval (* 60 5)
  "Interval of time you will be resting, in seconds."
  :type 'number)
(defcustom bluedot-history-file (expand-file-name "bluedot-history.txt"  user-emacs-directory)
  "File to store all the completed pomodoros."
  :type 'string)
(defcustom bluedot-popup-header '(format "Working with '%s'" (current-buffer))
  "Header used in popup."
  :type 'sexp)
(defcustom bluedot-play-sound t
  "Play sounds when true."
  :type 'boolean)
(defcustom bluedot-show-notification t
  "Sends system notifications when true."
  :type 'boolean)
(defcustom bluedot-sound-volume "0.3"
  "Sound volume as numeric string (low < 1.0 < high)."
  :type 'string)

(defcustom bluedot-modeline-face nil
  "Face to serve as the base for modeline display"
  :type 'face)

(defvar bluedot-bar-characters nil)

(require 'which-func)

;; stores bluedot timer, to be cancelled if restarted
(defvar bluedot--timer nil)
(defvar bluedot--notification nil)
(defvar bluedot--notified-done nil)

;; stores the number of completed pomodoros
(defvar bluedot--completed-pomodoros 0)

;; pomodoro start time
(defvar bluedot--pomodoro-started-at (current-time))

;; current pomodoro description
(defvar bluedot--pomodoro-description "Start your first pomodoro now!!!")

;; intervals, bars & colours
(defvar bluedot--bars)

(defun bluedot--setup ()
  "Recalculate variables"
  (setq bluedot--bars
        (cl-loop with work
               with rest
               for icon in (or bluedot-bar-characters '("⠁" "⠉" "⠋" "⠛" "⠟" "⠿" "⡿" "⣿"))
               for work-color in (color-ramp "Deepskyblue4" "white" 8)
               for rest-color in (color-ramp "#00cc66" "#ccff66" 8)
               collect (list icon work-color) into work
               collect (list icon rest-color) into rest
               finally return
               (append work rest '(("✓" "PaleGreen"))))))

(bluedot--setup)

(defun bluedot--ended-work-interval-p (time)
  "Return t when ended work interval based on BLUEDOT--CURRENT-BARS."
  (>= (bluedot--seconds-since time) bluedot-work-interval))

(defun bluedot--ding ()
  (let ((ring-bell-function nil))
    (ding t)))

(defun bluedot--notify (title msg)
  (when (eq system-type 'windows-nt)
    (w32-notification-close bluedot--notification)
    
    (--when-let (w32-notification-notify :tip title :title title :body msg)
      (setq bluedot--notification it)
      (run-at-time 2 nil #'w32-notification-close it))))

(defun bluedot--notify-work-done ()
  (message "work done")
  (when bluedot-play-sound
    (bluedot--ding))
  (when bluedot-show-notification
    (bluedot--notify "Time to Rest" bluedot--pomodoro-description)))

(defun bluedot--notify-rest-done ()
  (message "rest done")
  (when bluedot-play-sound
    (bluedot--ding))
  (when bluedot-show-notification
    (bluedot--notify "Back to Work" "Get to it")))

(defun bluedot--seconds-since (time)
  "Seconds since TIME."
  (truncate (- (float-time (current-time)) (float-time time))))

(defun bluedot--format-seconds (seconds)
  (format-time-string
   (concat
    (when (>= seconds 3600) "%H:")
    "%M:%S")
   (seconds-to-time seconds)))

(defun bluedot--popup-message (time desc)
  "TIME since start, DESC(ription) and instructions."
  (let* ((elapsed (float (bluedot--seconds-since time)))
         (working (min 1 (/ elapsed bluedot-work-interval)))
         (resting (/ (max 0 (- elapsed bluedot-work-interval))
                     bluedot-rest-interval)))

    (concat
     (cond
      ((>= resting 1)
       "[Inactive]")

      ((>= working 1)
       (format
        "[Resting] %s remaining"
        (bluedot--format-seconds
         (* (- 1 resting) bluedot-rest-interval))))

       (t
        (format
         "[Working] %s remaining [%s]"
         (bluedot--format-seconds
          (* (- 1 working) bluedot-work-interval))
         desc))))))

(defun bluedot--propertize (bar bar-color)
  "Propertize BAR with BAR-COLOR, help echo, and click action."
  (propertize bar
              'face `(:inherit ,bluedot-modeline-face :foreground ,bar-color :height 0.95)
              'help-echo '(bluedot--popup-message bluedot--pomodoro-started-at
                                                  bluedot--pomodoro-description)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map 'mouse-1 'bluedot)))

;; initializing current bar
(defvar bluedot--current-bar (apply #'bluedot--propertize (car (last bluedot--bars))))
;; setting as risky, so it's painted with colour
(put 'bluedot--current-bar 'risky-local-variable t)

;; adding to mode-line
(defun bluedot-add-to-misc-mode-line-info ()
  (add-to-list 'mode-line-misc-info
               '(:eval (when bluedot-mode
                         bluedot--current-bar))
               t))

(defun bluedot--load (file)
  "Use FILE to load DATA."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))

      (let ((buffer (current-buffer)))
        (save-excursion
          (goto-char (point-min))
          (cl-loop with entry
                   do (setq entry (ignore-errors (read buffer)))
                   while entry
                   collect entry))))))

(defun bluedot--add-history ()
  "Adding current-pomodoro info to history file."
  (--when-let bluedot-history-file
    (with-current-buffer (find-file it)
      (save-excursion
        (goto-char (point-max))
        (insert (prin1-to-string
                 (list bluedot--pomodoro-started-at
                       bluedot-work-interval
                       bluedot-rest-interval
                       bluedot--pomodoro-description))
                "\n")
        (save-buffer)
        (kill-buffer (current-buffer))))))

(defun bluedot--retrieve-history (&optional file)
  (bluedot--load (or file bluedot-history-file)))

(cl-defun bluedot--format-history (&key history format)
  (cl-loop
   for (time work rest desc) in (or history (bluedot--retrieve-history)) collect
   (list
    (format-time-string (or format "%Y-%m-%d-%H:%M") time)
    (/ work 60)
    (/ rest 60)
    desc)))

(cl-defun bluedot-org-insert-history (&optional block)
  (interactive "P")
  (save-excursion
    (if block
        (progn
          (insert "#+begin_src elisp\n(bluedot--format-history)\n#+end_src\n")
          (forward-line -1)
          (let ((org-confirm-babel-evaluate nil))
            (org-babel-execute-src-block)))
      ;;
      (insert
       (cl-loop for entry in (bluedot--format-history)
                concat "|"
                concat (s-join "|" (mapcar (lambda (el) (format "%s" el)) entry))
                concat "|\n"))
      (forward-line -1)
      (org-table-align))))

;;;###autoload
(defun bluedot-history-report (&optional days)
  (interactive "P")
  (let ((buf (get-buffer-create "*Bluedot History*")))
    (with-current-buffer buf
      (erase-buffer)
      (loop
       with history = (--group-by
                       (car it)
                       (bluedot--format-history :format "%Y-%m-%d"))

       for (day . entries) in (reverse (-take-last (or days 2) history))
       do
       (insert (format "* %s\n" day))
       (loop
        for work in (-uniq
                     (--map
                      (-last-item it)
                      entries))
        do
        (insert (format "  - %s\n" work)))
       do (insert "\n"))
      (org-mode)
      (org-show-all)
      (local-set-key "q" 'kill-this-buffer))
    (switch-to-buffer buf)))

(defun bluedot--update-current-bar (&optional bluedot--current-bars)
  "Update current bar, and program next update using BLUEDOT--CURRENT-BARS."

  (let* ((elapsed (float (bluedot--seconds-since bluedot--pomodoro-started-at)))
         (working (min 1 (/ elapsed bluedot-work-interval)))
         (resting (/ (max 0 (- elapsed bluedot-work-interval))
                     bluedot-rest-interval)))
    (setq bluedot--current-bar
          (apply #'bluedot--propertize
                 (--if-let
                     (nth
                      (truncate (+ (* working 8)
                                   (* resting 8)))
                      bluedot--bars)
                     it
                   (nth 16 bluedot--bars))))

    (when (and (>= working 1)
               (not bluedot--notified-done))
      (run-hooks 'bluedot-after-work-hook
                 'bluedot-before-rest-hook)
      (setq bluedot--notified-done t))

    (if (< resting 1)
        (setq bluedot--timer
              (run-at-time
               (/ (if (< working 1) bluedot-work-interval bluedot-rest-interval) 16.0)
               nil #'bluedot--update-current-bar))
      ;;
      (run-hooks 'bluedot-after-rest-hook)
      (setq bluedot--completed-pomodoros
            (1+ bluedot--completed-pomodoros)))
    
    (force-mode-line-update t)))

;; Hooks
(add-hook 'bluedot-after-work-hook #'bluedot--notify-work-done)
(add-hook 'bluedot-after-rest-hook #'bluedot--notify-rest-done)
(add-hook 'bluedot-before-work-hook #'bluedot--add-history)

;;;###autoload
(define-minor-mode bluedot-mode
  "Little pomodoro timer in the mode-line."
  :global t)

(defun bluedot--default-desc ()
  "Default pomodoro description: Working with 'current-buffer'..."
  (concat (eval bluedot-popup-header)
          (cond ((which-function)
                 (format ":'%s'" (which-function))))))

(defun bluedot-get-heading-from-agenda ()
  (interactive)
  (when (derived-mode-p 'org-agenda-mode)
    (--when-let (org-get-at-bol 'org-marker)
      (let* ((marker it)
	     (buffer (marker-buffer marker))
	     (pos (marker-position marker)))
        (with-current-buffer buffer
          ;; TODO: Save the narrowing somehow
          (widen)
          (push-mark)
          (goto-char pos)
          (org-get-heading t t t t))))))

;;;###autoload
(defun bluedot ()
  "Ask for DESCRIPTION, enable minor-mode, and start the pomodoro."
  (interactive)

  (--when-let (helm
               :sources
               `(,(when (or (derived-mode-p 'org-mode)
                            (derived-mode-p 'org-agenda-mode))
                    (helm-build-sync-source "Org Heading"
                      :candidates
                      (-non-nil
                       `(,(when (derived-mode-p 'org-mode)
                            (org-get-heading t t t t))
                         ,(bluedot-get-heading-from-agenda)))))
                 ,(helm-build-sync-source "Previous Labels"
                    :candidates
                    (lambda ()
                      (seq-take
                       (delete-dups
                        (reverse
                         (cl-loop for (time work rest label) in (bluedot--retrieve-history)
                                  collect label)))
                       20)))
                 ,(helm-build-dummy-source "New Label")))

    (bluedot-mode t)
    (if bluedot--timer (cancel-timer bluedot--timer))
    (setq bluedot--pomodoro-started-at (current-time)
          bluedot--pomodoro-description it
          bluedot--notified-done nil)
    (run-hooks 'bluedot-before-work-hook)
    (bluedot--update-current-bar bluedot--bars)))

;;;###autoload
(defun bluedot-resume ()
  "Detects and resumes any currently active timer in the history"
  (interactive)
  (--when-let (bluedot--retrieve-history)
    (let* ((entry (car (last it)))
           (time (current-time))
           (last-time (car entry))
           (description (car (last entry))))
      (when (< (- (time-to-seconds time) (time-to-seconds last-time))
               (+ (nth 1 entry) (nth 2 entry)))

        (bluedot-mode t)

        (when bluedot--timer
          (cancel-timer bluedot--timer))

        (setq bluedot--pomodoro-started-at last-time
              bluedot--pomodoro-description description
              bluedot--notified-done nil)
        
        (bluedot--update-current-bar bluedot--bars)))))

;;;;;;;;
(provide 'bluedot)

