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
(defcustom bluedot-history-file "~/.emacs.d/bluedot-history.txt"
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

(add-hook 'bluedot-after-work-hook #'bluedot--notify-work-done)
(add-hook 'bluedot-after-rest-hook #'bluedot--notify-rest-done)

(defun bluedot--seconds-since (time)
  "Seconds since TIME."
  (truncate (- (float-time (current-time)) (float-time time))))

(defun bluedot--popup-message (time desc)
  "TIME since start, DESC(ription) and instructions."
  (let* ((seconds (bluedot--seconds-since time))
         (minutes (truncate seconds 60)))
    (concat (format "%s completed pomodoro(s) in this session\n"
                    bluedot--completed-pomodoros)
            (format "%s, %s\n" (format-time-string "%T" time) desc)
            (cond
             ((= 0 minutes) (format "%s seconds" seconds))
             ((= 1 minutes) "1 minute")
             (t (format "%s minutes" minutes)))
            " elapsed, click to (re)start")))

(defun bluedot--propertize (bar bar-color)
  "Propertize BAR with BAR-COLOR, help echo, and click action."
  (propertize bar
              'face `(:foreground ,bar-color)
              'help-echo '(bluedot--popup-message bluedot--pomodoro-started-at
                                                  bluedot--pomodoro-description)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map 'mouse-1 'bluedot)))

;; initializing current bar
(defvar bluedot--current-bar (apply #'bluedot--propertize (car (last bluedot--bars))))
;; setting as risky, so it's painted with colour
(put 'bluedot--current-bar 'risky-local-variable t)

;; storing selected window to use from mode-line
(defvar bluedot--selected-window (selected-window))

;; function that updates selected window variable
(defun bluedot--update-selected-window (windows)
  "WINDOWS parameter avoids error when called before 'pre-redisplay-function'."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq bluedot--selected-window (selected-window))))

(add-function :before pre-redisplay-function #'bluedot--update-selected-window)

(defun bluedot--selected-window-p ()
  "Check if current window is the selected one."
  (eq bluedot--selected-window (get-buffer-window)))

;; adding to mode-line
(add-to-list 'mode-line-misc-info
             '(:eval (if (and bluedot-mode (bluedot--selected-window-p))
                         bluedot--current-bar))
             t)

(defun bluedot--save (file data)
  "Use FILE to save DATA."
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t))  ; Allow circular data
      (prin1 data))))

(defun bluedot--load (file)
  "Use FILE to load DATA."
  (ignore-errors (with-temp-buffer
                   (insert-file-contents file)
                   (read (current-buffer)))))

(defun bluedot--save-history ()
  "Adding current-pomodoro info to history file."
  (when bluedot-history-file
    (let ((history (bluedot--load bluedot-history-file)))
      (bluedot--save bluedot-history-file
                     (add-to-list 'history
                                  (list bluedot--pomodoro-started-at
                                        bluedot-work-interval
                                        bluedot-rest-interval
                                        bluedot--pomodoro-description)
                                  t)))))

(add-hook 'bluedot-after-rest-hook #'bluedot--save-history)

(defun bluedot--retrieve-history (&optional file)
  (when bluedot-history-file
    (with-current-buffer (find-file-existing (or file bluedot-history-file))
      (prog1
          (read (buffer-string))
        (kill-current-buffer)))))

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

;;;###autoload
(define-minor-mode bluedot-mode
  "Little pomodoro timer in the mode-line."
  :global t)

(defun bluedot--default-desc ()
  "Default pomodoro description: Working with 'current-buffer'..."
  (concat (eval bluedot-popup-header)
          (cond ((which-function)
                 (format ":'%s'" (which-function))))))

;;;###autoload
(defun bluedot (&optional description)
  "Ask for DESCRIPTION, enable minor-mode, and start the pomodoro."
  (interactive (list (read-string (format "Description (%s): "
                                          (bluedot--default-desc))
                                  nil nil (bluedot--default-desc))))
  (unless description
    (setq description (bluedot--default-desc)))
  (bluedot-mode t)
  (if bluedot--timer (cancel-timer bluedot--timer))
  (run-hooks 'bluedot-before-work-hook)
  (setq bluedot--pomodoro-started-at (current-time)
        bluedot--pomodoro-description description
        bluedot--notified-done nil)
  (bluedot--update-current-bar bluedot--bars))

;;;;;;;;
(provide 'bluedot)

