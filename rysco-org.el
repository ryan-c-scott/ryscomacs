(require 'org-agenda)
(require 'helm)

(defvar rysco-org-refile-targets nil)

(defun helm-rysco-org-agenda-buffer-items (&optional arg)
  (interactive "P")
  (--when-let
      (and (derived-mode-p 'org-agenda-mode)
           (save-excursion
             (goto-char (point-min))
             (cl-loop
              with action = `(("Go to item" . (lambda (pos) (goto-char pos)))
                              ("Go to heading" . (lambda (pos)
                                                   (goto-char pos)
                                                   (org-agenda-goto))))

              do (org-agenda-forward-block)
              as heading = (s-replace "\n" "" (thing-at-point 'line))
              do (forward-line 1)
              while (< (point) (point-max))
              as end = (save-excursion
                         (org-agenda-forward-block)
                         (if (= (point) (point-max))
                             (point-max)
                           (forward-line -1)
                           (point)))

              collect
              (helm-build-sync-source heading
                :candidates
                (save-excursion
                  (cl-loop
                   while (< (point) end)
                   collect
                   (cons
                    (s-replace "\n" "" (thing-at-point 'line))
                    (point))
                   do (forward-line 1)))

                :action action))))

    (or
     (helm :sources it)
     t)))

(defun rysco-org-agenda-goto-first-section ()
  (interactive)
  (goto-char (point-min))
  (org-agenda-next-item 1))

(defun rysco-agenda-refile-wrapper (old &rest args)
  (let ((org-refile-targets (or rysco-org-refile-targets org-refile-targets)))
    (apply old args)))

(advice-add 'org-agenda-refile :around 'rysco-agenda-refile-wrapper)

;;
(provide 'rysco-org)
