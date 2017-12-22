(defvar dvc-diff-backups-regex "\\.orig$")

(defun dvc-diff-mark-file-region (start end &optional unmark)
  (interactive "rregion: ")
    (goto-char start)
    (while (< (point) end)
      (let ((before (point)))
	(if unmark
	    (dvc-diff-unmark-file)
	  (dvc-diff-mark-file))
	(when (= (point) before)
	  (forward-line 1)))))

(defun dvc-diff-unmark-file-region (start end)
  (interactive "rregion: ")
  (dvc-diff-mark-file-region start end t))

(defun dvc-diff-mark-file-dwim ()
  (interactive)
  (if (use-region-p)
      (dvc-diff-mark-file-region (region-beginning) (region-end))
    (dvc-diff-mark-file)))

(defun dvc-diff-unmark-file-dwim ()
  (interactive)
  (if (use-region-p)
      (dvc-diff-unmark-file-region (region-beginning) (region-end))
    (dvc-diff-unmark-file)))

(defun dvc-diff-mark-files-matching (&optional regex unmark)
  (save-excursion
    (let ((path "") (line "") (end 0))
      (when (not regex)
	(setq path (substring-no-properties (thing-at-point 'line t) 4 (current-column))))

      (goto-char (point-min))
      (forward-line 1)
      (forward-paragraph)
      (setq end (point))
      
      (goto-char (point-min))
      (forward-line 1)
      (setq line (thing-at-point 'line t))
      
      (while (< (point) end)
	(let ((before (point)))
	  (when (if regex (string-match regex line)
		  (s-contains? path line))
	    (if unmark
		(dvc-diff-unmark-file)
	      (dvc-diff-mark-file)))

	  (when (= (point) before)
	    (forward-line 1)))
      	(setq line (thing-at-point 'line t))))))

(defun dvc-diff-mark-files-subdirectory ()
  (interactive)
  (dvc-diff-mark-files-matching))

(defun dvc-diff-unmark-files-subdirectory ()
  (interactive)
  (dvc-diff-mark-files-matching nil t))

(defun dvc-diff-mark-files-regex (regex)
  (interactive "sRegex: ")
  (dvc-diff-mark-files-matching regex))

(defun dvc-diff-unmark-files-regex (regex)
  (interactive "sRegex: ")
  (dvc-diff-mark-files-matching regex t))
  
(defun dvc-diff-mark-backups ()
  (interactive)
  (dvc-diff-mark-files-matching dvc-diff-backups-regex))

(defun dvc-diff-print-list ()
  (interactive)
  (message "%s" (dvc-current-file-list)))

(defun dvc-diff-delete-files-marked ()
  (interactive)
  (when (yes-or-no-p (format "Really delete these files? %s" (dvc-current-file-list)))
    (let ((root (dvc-tree-root)) path)
      (dolist (filename (dvc-current-file-list))
	(setq path (format "%s%s" root filename))
	(delete-file path)
	(message "Deleted: %s" path)))
    (dvc-status)))

(defun dvc-diff-delete-backups ()
  (interactive)
  (dvc-status)
  (dvc-diff-mark-backups)
  (when (string-match dvc-diff-backups-regex (car (dvc-current-file-list)))
    (dvc-diff-delete-files-marked)))

;;;;;;;; Fixes for missing functionality
(defun xhg-dvc-file-diff (&optional path baseref localref noswitch)
  (interactive)
  (dvc-run-dvc-sync 'xhg `("diff" ,path)
		    :finished
		    (dvc-capturing-lambda (output error status arguments)
		      (progn
			(let* ((diff-buffer (get-buffer-create "*xhg-file-diff*"))
			       (inhibit-read-only t))
			  (with-current-buffer diff-buffer
			    (erase-buffer)
			    (insert-buffer-substring output)
			    (diff-mode)
			    (read-only-mode)
			    (dvc-switch-to-buffer diff-buffer)))))))

;;;;;
(provide 'dvc-helpers)
