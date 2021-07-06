(require 'calfw)
(require 'calfw-ical)
(require 'calfw-org)
(require 'org-gcal)
(require 'request-deferred)

(defvar rysco-gcal-calendars nil)
(defvar rysco-calfw-upcoming-threshold 600)
(defvar rysco-calendar-preferred-meeting-links '())

;;;###autoload
(defun rysco-calendar-open ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (cl-loop
    for (id file color) in rysco-gcal-calendars collect
    (funcall 'cfw:org-create-file-source id file color))))

;;;###autoload
(cl-defun rysco-calendar-gcal-fetch (&optional update-only)
  (interactive "P")

  (unless update-only
    (message "Clearing gcal calendar data")
    (rysco-calendar-gcal-clear-files))

  (deferred:watch (org-gcal-sync t t)
    (lambda ()
      (message "Gcal fetched")
      (rysco-calendar-gcal-save)
      (if (derived-mode-p 'cfw:calendar-mode)
          (cfw:refresh-calendar-buffer)
        (rysco-calendar-open))
      (message "Gcal update completed"))))

(cl-defun rysco-calendar-gcal-save ()
  (interactive)
  (cl-loop
   for (_ . file) in org-gcal-file-alist do
   (--when-let (get-buffer (f-filename file))
     (with-current-buffer it
       (setq buffer-file-coding-system 'utf-8-unix)
       (save-buffer)
       (kill-buffer)))))

(defun rysco-calendar-gcal-refresh-token ()
  (interactive)
  (org-gcal--refresh-token))

(cl-defun rysco-calendar-gcal-clear-files ()
  (interactive)
  (org-gcal-sync-tokens-clear)
  (rysco-calendar-gcal-save)
  (cl-loop
   for (_ . file) in org-gcal-file-alist do
   (when (f-exists? file)
     (f-move file (concat file ".bak")))))

(defun rysco-calendar-calfw-org-format-title (file h-obj t-obj h-beg loc)
  (propertize
   (concat
    (org-element-property :title h-obj))
   'keymap cfw:org-text-keymap
   'display nil
   'cfw:org-file file
   'cfw:org-h-beg h-beg
   'cfw:org-loc loc))

(advice-add 'cfw:org-format-title :override 'rysco-calendar-calfw-org-format-title)

(defun rysco-calendar-hash-cfw:event (evt)
  (secure-hash
   'md5
   (format
    "%s|%s|%s"
    (cfw:event-start-date evt)
    (cfw:event-end-date evt)
    (substring-no-properties
     (cfw:event-title evt)))))

(defun rysco-calendar-calfw-unique-events (results)
  (cl-loop
   with hashes = (make-hash-table :test 'equal)

   for (key . data) in results
   if (equal key 'periods) collect
   (cons
    'periods
    (cl-loop
     for (beg end evt) in data
     as evthash = (rysco-calendar-hash-cfw:event evt)
     unless (gethash evthash hashes) collect
     (let ((start-time (cfw:event-start-time evt))
           (end-time (cfw:event-end-time evt)))
       (puthash evthash t hashes)
       `(,beg
         ,(if (or (and (not start-time) (not end-time))
                  (= (car start-time) (cadr start-time)
                     (car end-time) (cadr end-time)
                     0))
              ;; Day boundary, so cheat the end back to the previous day 23:59
              (progn
                (setf (cfw:event-end-time evt) '(23 59))
                (cfw:date-before end 1))

            ;; Otherwise use the existing end time
            end)
         ,evt))))

   else collect
   (cons
    key
    (cl-loop
     for evt in data
     as evthash = (rysco-calendar-hash-cfw:event evt)
     unless (gethash evthash hashes) collect
     (progn
       (puthash evthash t hashes)
       evt)))))

(advice-add 'cfw:contents-merge :filter-return 'rysco-calendar-calfw-unique-events)

(defun rysco-calendar-convert-same-day-periods (event-list)
  ;; Note:  Assumes (('periods ...) ...) structure
  (loop
   with events = (cdr event-list)
   with periods

   for ev in (cadar event-list)
   if (equal (cfw:event-start-date ev) (cfw:event-end-date ev))
   collect ev into events
   else
   collect ev into periods
   finally return `((periods ,periods) ,@events)))

(advice-add 'cfw:org-convert-org-to-calfw :filter-return 'rysco-calendar-convert-same-day-periods)

(defun rysco-calendar--preferred-event-link (urls)
  (--first
   (let ((host (url-host (url-generic-parse-url it))))
     (--first (s-matches? it host) rysco-calendar-preferred-meeting-links))
   urls))

(defun rysco-calendar-event-location-detection (event)
  (unless (cfw:event-location event)
    (let* ((urls (rysco-calfw--extract-urls (cfw:event-description event)))
           (new-loc
            (or
             (rysco-calendar--preferred-event-link urls)
             (car urls))))
      (setf (cfw:event-title event) (cfw:tp (cfw:event-title event) 'cfw:org-loc new-loc)
            (cfw:event-location event) new-loc)))
  event)

(advice-add 'cfw:org-convert-event :filter-return 'rysco-calendar-event-location-detection)

(defface rysco-calfw-past-date
  '((default
      (:foreground "gray")))
  "Face for past events in Calfw"
  :group 'calfw)

(defface rysco-calfw-upcoming-date
  '((default
      (:slant italic
       :weight bold
       :underline t
       :foreground "white"
       :background "cyan4")))
  "Face for upcoming events in Calfw"
  :group 'calfw)

(defface rysco-calfw-current-date
  '((default
      (:slant italic
       :weight bold
       :underline t)))
  "Face for currently occurring events in Calfw"
  :group 'calfw)

(defun rysco-calfw--convert-date-and-time (date time)
  (time-add (cfw:calendar-to-emacs date)
            (if time
                (+ (* (car time) 3600)
                   (* (cadr time) 60))
              0)))

(defun rysco-calfw-dimmed-past (fun event format-string)
  (let* ((output (funcall fun event format-string))
         (now (current-time))
         (event-start
          (rysco-calfw--convert-date-and-time
           (cfw:event-start-date event)
           (cfw:event-start-time event)))
         (event-end
          (rysco-calfw--convert-date-and-time
           (or (cfw:event-end-date event)
               (cfw:event-start-date event))
           (or (cfw:event-end-time event)
               (cfw:event-start-time event))))
         (started (time-less-p event-start now))
         (ended (time-less-p event-end now))
         (starting-soon (time-less-p
                         (time-subtract
                          event-start
                          (seconds-to-time rysco-calfw-upcoming-threshold))
                         now)))
    (cond
     ((and started ended)
      (propertize output 'face 'rysco-calfw-past-date 'cfw:old t))
     (started
      (propertize output 'face 'rysco-calfw-current-date))
     (starting-soon
      (propertize output 'face 'rysco-calfw-upcoming-date))
     (t
      output))))

(advice-add 'cfw:event-format :around 'rysco-calfw-dimmed-past)

(defun rysco-calfw-today-overlay (dest)
  nil)
(advice-add 'cfw:dest-ol-today-set :override 'rysco-calfw-today-overlay)

(defun rysco-calfw-render-truncate (org limit-width &optional ellipsis)
  (let ((new (substring org 0 (s-index-of "\n" org)))
        (ellipsis (when ellipsis "…")))
    (if (< limit-width (string-width org))
        (let ((str (truncate-string-to-width
                    (substring new 0) limit-width 0 nil ellipsis)))
          (cfw:tp str 'mouse-face 'highlight)
          (unless (get-text-property 0 'help-echo str)
            (cfw:tp str 'help-echo org))
          str)
      new)))

(defun rysco-calfw-render-padding-change (args)
  (-let [(width string padding) args]
    `(,width
      ,string
      ,(--when-let padding
         (if (equal padding ?-)
             ?\s
           padding)))))

(advice-add 'cfw:render-truncate :override 'rysco-calfw-render-truncate)
(advice-add 'cfw:render-left :filter-args 'rysco-calfw-render-padding-change)
(advice-add 'cfw:render-right :filter-args 'rysco-calfw-render-padding-change)
(advice-add 'cfw:render-center :filter-args 'rysco-calfw-render-padding-change)

(defun rysco-calfw--extract-urls (s)
  (mapcar
   'car
   (s-match-strings-all
    (rx
     word-start (one-or-more word) "://"
     (+ (not (any blank control "\"" "," ""))))
    (substring-no-properties s))))

(defun rysco-calfw-goto-loc-at-point ()
  (interactive)
  (--when-let
      (car
       (rysco-calfw--extract-urls
        (get-text-property (point) 'cfw:org-loc)))
    (browse-url it)))

(defun rysco-calfw--get-gcal-viewing-link (file-path)
  (cl-loop
   with link
   until link
   for (id path _ viewing-link) in rysco-gcal-calendars do
   (when (equal path file-path)
     (setq link viewing-link))
   finally return link))

(defun rysco-calfw-goto-gcal-at-point ()
  (interactive)
  (--when-let (rysco-calfw--get-gcal-viewing-link (get-text-property (point) 'cfw:org-file))
    (browse-url it)))

(defun rysco-calfw-navi-forward-to-title (&rest _)
  (interactive)
  (when (eq (cfw:cp-get-view (cfw:cp-get-component)) 'day)
    (--if-let (get-text-property (point) 'cfw:row-count)
        (progn
          (text-property-search-backward 'cfw:row-count it t t)
          (pcase (char-after)
            (?\s ;; If the line starts with a space assume it's a period entry
             (forward-word 6)
             (forward-char 3))
            (_
             (forward-word 4)
             (forward-char 1))))
      (forward-line 1)
      (if (get-text-property (point) 'cfw:row-count)
          (progn
            (forward-char 1)
            (rysco-calfw-navi-forward-to-title))
        (forward-line -1)
        (forward-char 1)))))

(defun rysco-calfw-goto-current-entry (&rest _)
  (unless (get-text-property (point) 'cfw:row-count)
    (cfw:navi-next-item-command))

  (loop
   with last-row
   as row = (get-text-property (point) 'cfw:row-count)
   as old = (get-text-property (point) 'cfw:old)
   while (and row old)
   while (or (not last-row) (> row last-row)) do
   (progn
     (setq last-row row)
     (cfw:navi-next-item-command))))

(advice-add 'cfw:refresh-calendar-buffer :after 'rysco-calfw-goto-current-entry)
(advice-add 'cfw:navi-goto-today-command :after 'rysco-calfw-goto-current-entry)
(advice-add 'cfw:navi-goto-date-command :after 'rysco-calfw-goto-current-entry)
(advice-add 'cfw:change-view-month :after 'rysco-calfw-goto-current-entry)
(advice-add 'cfw:change-view-week :after 'rysco-calfw-goto-current-entry)
(advice-add 'cfw:change-view-two-weeks :after 'rysco-calfw-goto-current-entry)
(advice-add 'cfw:change-view-day :after 'rysco-calfw-goto-current-entry)

(advice-add 'cfw:navi-next-item-command :after 'rysco-calfw-navi-forward-to-title)
(advice-add 'cfw:navi-prev-item-command :after 'rysco-calfw-navi-forward-to-title)

(add-to-list 'god-exempt-major-modes 'cfw:calendar-mode)

(define-key cfw:calendar-mode-map (kbd "C-<return>") 'rysco-calfw-goto-loc-at-point)
(define-key cfw:calendar-mode-map (kbd "M-<return>") 'rysco-calfw-goto-gcal-at-point)
(define-key cfw:calendar-mode-map (kbd "M-p") 'cfw:navi-previous-week-command)
(define-key cfw:calendar-mode-map (kbd "M-n") 'cfw:navi-next-week-command)
(define-key cfw:calendar-mode-map (kbd "C-M-p") 'cfw:navi-previous-month-command)
(define-key cfw:calendar-mode-map (kbd "C-M-n") 'cfw:navi-next-month-command)
(define-key cfw:calendar-mode-map (kbd "p") 'cfw:navi-prev-item-command)
(define-key cfw:calendar-mode-map (kbd "n") 'cfw:navi-next-item-command)

;;;;
(provide 'rysco-calendar)
