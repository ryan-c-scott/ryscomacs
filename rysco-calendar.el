(require 'calfw)
(require 'calfw-ical)
(require 'calfw-org)
(require 'org-gcal)

;;;###autoload
(defun rysco-calendar-open ()
  (interactive)

  (cfw:open-calendar-buffer
   :contents-sources
   (cl-loop
    for (id file color) in rysco-gcal-calendars collect
    (funcall 'cfw:org-create-file-source id file color))))

;;;###autoload
(cl-defun rysco-calendar-gcal-save ()
  (interactive)
  (cl-loop
   for (_ . file) in org-gcal-file-alist do
   (with-current-buffer (find-file-noselect file)
     (save-buffer)
     (kill-buffer))))

(defun rysco-calendar-hash-cfw:event (evt)
  (secure-hash
   'md5
   (format
    "%s|%s|%s"
    (cfw:event-start-date evt)
    (cfw:event-end-date evt)
    (cfw:event-title evt))))

(defun rysco-calendar-calfw-unique-events (results)
  (cl-loop
   with hashes = (make-hash-table :test 'equal)

   for (key . data) in results collect
   (cond
    ((equal key 'periods)
     (cons
      'periods
      (cl-loop
       for (beg end evt) in data
       as evthash = (rysco-calendar-hash-cfw:event evt)
       unless (gethash evthash hashes) collect
       (progn
         (puthash evthash t hashes)
         `(,beg ,end ,evt)))))

    (t
     (let ((evthash (rysco-calendar-hash-cfw:event (car data))))
       (unless (gethash evthash hashes)
         (puthash evthash t hashes)
         (cons key data)))))))

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

(defun rysco-calfw--extract-urls (s)
  (s-match-strings-all
   (rx
    word-start (one-or-more word) "://"
    (+ (not (any blank ","))))
   s))

(defun rysco-calfw-goto-loc-at-point ()
  (interactive)
  (--when-let
      (caar
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

(advice-add 'cfw:contents-merge :filter-return 'rysco-calendar-calfw-unique-events)
(advice-add 'cfw:org-convert-org-to-calfw :filter-return 'rysco-calendar-convert-same-day-periods)
(advice-add 'cfw:render-truncate :override 'rysco-calfw-render-truncate)
(advice-add 'cfw:render-left :filter-args 'rysco-calfw-render-padding-change)
(advice-add 'cfw:render-right :filter-args 'rysco-calfw-render-padding-change)
(advice-add 'cfw:render-center :filter-args 'rysco-calfw-render-padding-change)

(add-to-list 'god-exempt-major-modes 'cfw:calendar-mode)

(define-key cfw:calendar-mode-map (kbd "C-<return>") 'rysco-calfw-goto-loc-at-point)
(define-key cfw:calendar-mode-map (kbd "M-<return>") 'rysco-calfw-goto-gcal-at-point)

;;;;
(provide 'rysco-calendar)
