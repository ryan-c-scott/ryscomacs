(defun helm-monky--do-staging (el)
  (let ((file-list (helm-marked-candidates :all-sources t)))
    (cl-loop for file in file-list do
             (cond
              ((member file monky-all-untracked-files)
               (monky-hg-clear-cache)
               (monky-run-hg "add" file)
               (monky-stage-file file))

              ((member file monky-staged-files)
               (monky-unstage-file file))

              ((member file monky-all-changed-files)
               (monky-stage-file file)))))
  (monky-refresh-buffer))

(defun helm-monky-stage ()
  (interactive)
  (helm :sources `(,(helm-build-sync-source "Unstaged"
                      :candidates
                      (cl-loop for file in monky-all-changed-files
                               when (not (member file monky-staged-files))
                               collect file)
                      :action #'helm-monky--do-staging)
                   
                   ,(helm-build-sync-source "Staged"
                      :candidates monky-staged-files
                      :action #'helm-monky--do-staging)

                   ,(helm-build-sync-source "Untracked"
                      :candidates monky-all-untracked-files
                      :action #'helm-monky--do-staging))))

;;;;;;;;
(provide 'helm-monky)
