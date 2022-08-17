;;; -*- lexical-binding: t; -*-

;;; bluedot.el --- Small pomodoro timer (1 char)

;; Author: Ryon C. Scott
;; URL: 
;; Version: 1.0
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

;; This package provides a little pomodoro timer in the mode-line based on the currently clocked in org task.
;;
;; This is a heavily modified fork of redtick by F. Febles (http://github.com/ferfebles/redtick)
;;; Code:

(require 'cl)
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

(defcustom bluedot-popup-header '(format "Working with '%s'" (current-buffer))
  "Header used in popup."
  :type 'sexp)

(defcustom bluedot-play-sound t
  "Play sounds when true."
  :type 'boolean)

(defcustom bluedot-show-notification t
  "Sends system notifications when true."
  :type 'boolean)

(defcustom bluedot-modeline-face nil
  "Face to serve as the base for modeline display"
  :type 'face)

(defcustom bluedot-clocked-org-todo-states '("NOW")
  "States which will not cause the active clock to be stopped for an entry"
  :type 'sexp)

(defvar bluedot-bar-characters nil)

(require 'which-func)

;; stores bluedot timer, to be cancelled if restarted
(defvar bluedot--timer nil)
(defvar bluedot--notified-done nil)

;; stores the number of completed pomodoros
(defvar bluedot--completed-pomodoros 0)

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

(defun bluedot--notify (title msg &optional sound)
  (when (eq system-type 'windows-nt)
    (alert-toast-notify `(:title ,title :message ,msg :data (:audio ,(or sound 'default))))))

(defun bluedot--notify-work-done ()
  (message "work done")
  (when bluedot-play-sound
    (bluedot--ding))
  (when bluedot-show-notification
    (bluedot--notify "Time to Rest" org-clock-current-task 'im)))

(defun bluedot--notify-rest-done ()
  (message "rest done")
  (when bluedot-play-sound
    (bluedot--ding))
  (when bluedot-show-notification
    (bluedot--notify "Back to Work" "Get to it" 'reminder)))

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
              'help-echo '(bluedot--popup-message org-clock-start-time
                                                  org-clock-current-task)
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

(defun bluedot--cancel-timer ()
  (when bluedot--timer
    (cancel-timer bluedot--timer))
  (setq bluedot--timer nil))

;;;###autoload
(defun bluedot--update-current-bar (&optional bluedot--current-bars)
  "Update current bar, and program next update using BLUEDOT--CURRENT-BARS."

  (let* ((elapsed (float (bluedot--seconds-since org-clock-start-time)))
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

    (when (< working 1)
      (setq bluedot--notified-done nil))

    (when (and (>= working 1)
               (not bluedot--notified-done))
      (run-hooks 'bluedot-after-work-hook
                 'bluedot-before-rest-hook)
      (setq bluedot--notified-done t))

    (if (< resting 1)
        (setq bluedot--timer
              (progn
                (bluedot--cancel-timer)
                (run-at-time
                 (/ (if (< working 1) bluedot-work-interval bluedot-rest-interval) 16.0)
                 nil #'bluedot--update-current-bar)))
      ;;
      (run-hooks 'bluedot-after-rest-hook)
      (setq bluedot--completed-pomodoros
            (1+ bluedot--completed-pomodoros)))

    (force-mode-line-update t)))

;; Hooks
(add-hook 'bluedot-after-work-hook #'bluedot--notify-work-done)
(add-hook 'bluedot-after-rest-hook #'bluedot--notify-rest-done)

;;;###autoload
(define-minor-mode bluedot-mode
  "Little pomodoro timer in the mode-line."
  :global t)

(defun bluedot-org-clock-in ()
  (bluedot-mode 1))

(defun bluedot-org-clock-out ()
  (org-clock-out nil t))

(defun bluedot-org-clock-cancel ()
  (bluedot--cancel-timer)
  (bluedot-mode 0))

(defun bluedot-org-jump-to-clock ()
  (interactive)
  (when (org-clocking-p)
    (org-clock-jump-to-current-clock)))

(defun bluedot-org-clock-in-last ()
  (interactive)
  (unless (org-clocking-p)
    (org-clock-in-last 1)))

(defun bluedot-org-clock-out-if-current ()
  "Detects whether current entry is the running clock and clocks out if so.

Based on `org-clock-out-if-current', but ignores `org-clock-out-when-done'"
  (when (and (org-clocking-p)
	     org-clock-out-when-done
	     (marker-buffer org-clock-marker)
	     (equal (or (buffer-base-buffer (org-clocking-buffer))
			(org-clocking-buffer))
		    (or (buffer-base-buffer (current-buffer))
			(current-buffer)))
	     (< (point) org-clock-marker)
	     (> (org-with-wide-buffer (org-entry-end-position))
		org-clock-marker))
    ;; Clock out, but don't accept a logging message for this.
    (let ((org-log-note-clock-out nil)
	  (org-clock-out-switch-to-state nil))
      (org-clock-out))))

(defun bluedot-org-todo-change ()
  (when (org-clocking-p)
    (unless (member org-state bluedot-clocked-org-todo-states)
      (bluedot-org-clock-out-if-current))))

;;;###autoload
(defun bluedot-enable (enable)
  (if enable
      (progn
        (add-hook 'org-clock-in-hook 'bluedot-org-clock-in)
        (add-hook 'org-clock-out-hook 'bluedot-org-clock-cancel)
        (add-hook 'org-clock-cancel-hook 'bluedot-org-clock-cancel)
        (add-hook 'org-after-todo-state-change-hook 'bluedot-org-todo-change)
        (add-hook 'bluedot-after-rest-hook 'bluedot-org-clock-out)
        (advice-add 'org-clock-update-mode-line :after 'bluedot--update-current-bar))

    (remove-hook 'org-clock-in-hook 'bluedot-org-clock-in)
    (remove-hook 'org-clock-out-hook 'bluedot-org-clock-cancel)
    (remove-hook 'org-clock-cancel-hook 'bluedot-org-clock-cancel)
    (remove-hook 'org-after-todo-state-change-hook 'bluedot-org-todo-change)
    (remove-hook 'bluedot-after-rest-hook 'bluedot-org-clock-out)
    (advice-remove 'org-clock-update-mode-line 'bluedot--update-current-bar)))

(defun bluedot--default-desc ()
  "Default pomodoro description: Working with 'current-buffer'..."
  (concat (eval bluedot-popup-header)
          (cond ((which-function)
                 (format ":'%s'" (which-function))))))

;;;;;;;;
(provide 'bluedot)

