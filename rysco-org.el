(require 'org-agenda)
(require 'helm)

(defun helm-rysco-org-agenda-buffer-items (&optional arg)
  (interactive "P")
  (--when-let
      (and (derived-mode-p 'org-agenda-mode)
           (save-excursion
             (goto-char (point-min))

             (cl-loop
              with inhibit-message = t
              with last = (point)

              do (org-agenda-next-item 1)
              while (not (= (point) last))
              ;; TODO:  Detect super agenda headings
              as entry = (s-replace "\n" "" (thing-at-point 'line))
              collect (cons entry (point))
              do (setq last (point)))))

    (or
     (helm
      :sources
      (helm-build-sync-source "Org Agenda Items"
        :candidates it
        :action `(("Go to item" . (lambda (pos) (goto-char pos)))
                  ("Go to heading" . (lambda (pos)
                                       (goto-char pos)
                                       (org-agenda-goto))))))
     t)))

;;
(provide 'rysco-org)
