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
   (with-current-buffer (find-file file)
     (save-buffer)
     (kill-buffer))))

(add-to-list 'god-exempt-major-modes 'cfw:calendar-mode)

;;;;
(provide 'rysco-calendar)
