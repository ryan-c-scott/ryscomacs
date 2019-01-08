;;; redtick.el --- Smallest pomodoro timer (1 char)

;; Author: F. Febles
;; URL: http://github.com/ferfebles/redtick
;; Version: 00.01.03
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
;;; Code:

(require 'rysco-util)

(defgroup redtick nil
  "Little pomodoro timer in the mode-line."
  :group 'tools
  :prefix "redtick-")

;; pomodoro work & rest intervals in seconds
(defcustom redtick-work-interval (* 60 25)
  "Interval of time you will be working, in seconds."
  :type 'number)
(defcustom redtick-rest-interval (* 60 5)
  "Interval of time you will be resting, in seconds."
  :type 'number)
(defcustom redtick-history-file "~/.emacs.d/redtick-history.txt"
  "File to store all the completed pomodoros."
  :type 'string)
(defcustom redtick-popup-header '(format "Working with '%s'" (current-buffer))
  "Header used in popup."
  :type 'sexp)
(defcustom redtick-play-sound nil
  "Play sounds when true."
  :type 'boolean)
(defcustom redtick-sound-volume "0.3"
  "Sound volume as numeric string (low < 1.0 < high)."
  :type 'string)
(defcustom redtick-work-sound
  (expand-file-name "./resources/work.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Sound file to loop during the work period."
  :type 'string)
(defcustom redtick-rest-sound
  (expand-file-name "./resources/rest.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Sound file to loop during the rest period."
  :type 'string)

(require 'which-func)

;; stores redtick timer, to be cancelled if restarted
(defvar redtick--timer nil)

;; stores the number of completed pomodoros
(defvar redtick--completed-pomodoros 0)

;; pomodoro start time
(defvar redtick--pomodoro-started-at (current-time))

;; current pomodoro description
(defvar redtick--pomodoro-description "Start your first pomodoro now!!!")

;; redtick intervals for every bar
(defvar redtick--workbar-interval (/ redtick-work-interval 8.0))
(defvar redtick--restbar-interval (/ redtick-rest-interval 8.0))

;; intervals, bars & colours
(defvar redtick--bars)

(defun redtick--setup ()
  "Recalculate variables"
  (setq redtick--workbar-interval (/ redtick-work-interval 8.0)
        redtick--restbar-interval (/ redtick-rest-interval 8.0))
  
  (setq redtick--bars
        (cl-loop with work
               with rest
               for icon in '("⠁" "⠉" "⠋" "⠛" "⠟" "⠿" "⡿" "⣿")
               for work-color in (color-ramp "Deepskyblue4" "white" 8)
               for rest-color in (color-ramp "#00cc66" "#ccff66" 8)
               collect (list redtick--workbar-interval icon work-color) into work
               collect (list redtick--restbar-interval icon rest-color) into rest
               finally return
               (append work rest '((nil "✓" "PaleGreen"))))))

(redtick--setup)

(defun redtick--ended-work-interval-p (redtick--current-bars)
  "Return t when ended work interval based on REDTICK--CURRENT-BARS."
  (equal `(,redtick--restbar-interval "⣿")
       (butlast (car redtick--current-bars))))

(defun redtick--ding ()
  (let ((ring-bell-function nil))
    (ding t)))

(add-hook 'redtick-after-work-hook #'redtick--ding)
(add-hook 'redtick-after-rest-hook #'redtick--ding)

(defun redtick--seconds-since (time)
  "Seconds since TIME."
  (truncate (- (float-time (current-time)) (float-time time))))

(defun redtick--popup-message (time desc)
  "TIME since start, DESC(ription) and instructions."
  (let* ((seconds (redtick--seconds-since time))
         (minutes (truncate seconds 60)))
    (concat (format "%s completed pomodoro(s) in this session\n"
                    redtick--completed-pomodoros)
            (format "%s, %s\n" (format-time-string "%T" time) desc)
            (cond
             ((= 0 minutes) (format "%s seconds" seconds))
             ((= 1 minutes) "1 minute")
             (t (format "%s minutes" minutes)))
            " elapsed, click to (re)start")))

(defun redtick--propertize (bar bar-color)
  "Propertize BAR with BAR-COLOR, help echo, and click action."
  (propertize bar
              'face `(:foreground ,bar-color)
              'help-echo '(redtick--popup-message redtick--pomodoro-started-at
                                                  redtick--pomodoro-description)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map 'mouse-1 'redtick)))

;; initializing current bar
(defvar redtick--current-bar (apply #'redtick--propertize
                                    (cdar (last redtick--bars))))
;; setting as risky, so it's painted with colour
(put 'redtick--current-bar 'risky-local-variable t)

;; storing selected window to use from mode-line
(defvar redtick--selected-window (selected-window))

;; function that updates selected window variable
(defun redtick--update-selected-window (windows)
  "WINDOWS parameter avoids error when called before 'pre-redisplay-function'."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq redtick--selected-window (selected-window))))

(add-function :before pre-redisplay-function #'redtick--update-selected-window)

(defun redtick--selected-window-p ()
  "Check if current window is the selected one."
  (eq redtick--selected-window (get-buffer-window)))

;; adding to mode-line
(add-to-list 'mode-line-misc-info
             '(:eval (if (and redtick-mode (redtick--selected-window-p))
                         redtick--current-bar))
             t)

(defun redtick--save (file data)
  "Use FILE to save DATA."
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t))  ; Allow circular data
      (prin1 data))))

(defun redtick--load (file)
  "Use FILE to load DATA."
  (ignore-errors (with-temp-buffer
                   (insert-file-contents file)
                   (read (current-buffer)))))

(defun redtick--save-history ()
  "Adding current-pomodoro info to history file."
  (when redtick-history-file
    (let ((history (redtick--load redtick-history-file)))
      (redtick--save redtick-history-file
                     (add-to-list 'history
                                  (list redtick--pomodoro-started-at
                                        redtick-work-interval
                                        redtick-rest-interval
                                        redtick--pomodoro-description)
                                  t)))))

(add-hook 'redtick-after-rest-hook #'redtick--save-history)

(defun redtick--update-current-bar (redtick--current-bars)
  "Update current bar, and program next update using REDTICK--CURRENT-BARS."
  (setq redtick--current-bar (apply #'redtick--propertize
                                    (cdar redtick--current-bars)))
  (when (redtick--ended-work-interval-p redtick--current-bars)
    (run-hooks 'redtick-after-work-hook
               'redtick-before-rest-hook))
  (if (caar redtick--current-bars)
      (setq redtick--timer
            (run-at-time (caar redtick--current-bars)
                         nil
                         #'redtick--update-current-bar
                         (cdr redtick--current-bars)))
    (run-hooks 'redtick-after-rest-hook)
    (setq redtick--completed-pomodoros
          (1+ redtick--completed-pomodoros)))
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode redtick-mode
  "Little pomodoro timer in the mode-line."
  :global t)

(defun redtick--default-desc ()
  "Default pomodoro description: Working with 'current-buffer'..."
  (concat (eval redtick-popup-header)
          (cond ((which-function)
                 (format ":'%s'" (which-function))))))

;;;###autoload
(defun redtick ()
  "Enable minor-mode, and start the pomodoro."
  (interactive)
  (redtick-with-description (redtick--default-desc)))

;;;###autoload
(defun redtick-with-description (description)
  "Ask for DESCRIPTION, enable minor-mode, and start the pomodoro."
  (interactive (list (read-string (format "Description (%s): "
                                          (redtick--default-desc))
                                  nil nil (redtick--default-desc))))
  (redtick-mode t)
  (if redtick--timer (cancel-timer redtick--timer))
  (run-hooks 'redtick-before-work-hook)
  (setq redtick--pomodoro-started-at (current-time)
        redtick--pomodoro-description description)
  (redtick--update-current-bar redtick--bars))

(provide 'redtick)
;;; redtick.el ends here
