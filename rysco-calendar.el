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

(advice-add 'cfw:contents-merge :filter-return 'rysco-calendar-calfw-unique-events)
(add-to-list 'god-exempt-major-modes 'cfw:calendar-mode)

;;;;
(provide 'rysco-calendar)
