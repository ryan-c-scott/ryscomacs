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

(defun dvc-diff-mark-files-subdirectory (&optional unmark)
  (interactive)
  (save-excursion
    (let ((path "") (line "") (end 0))
      (setq path (substring-no-properties (thing-at-point 'line t) 4 (current-column)))

      (goto-char (point-min))
      (forward-line 1)
      (forward-paragraph)
      (setq end (point))
      
      (goto-char (point-min))
      (forward-line 1)
      (setq line (thing-at-point 'line t))
      
      (while (< (point) end)
	(let ((before (point)))
	  (when (s-contains? path line)
	    (if unmark
		(dvc-diff-unmark-file)
	      (dvc-diff-mark-file)))

	  (when (= (point) before)
	    (forward-line 1)))
      	(setq line (thing-at-point 'line t))))))

(defun dvc-diff-unmark-files-subdirectory ()
  (interactive)
  (dvc-diff-mark-files-subdirectory t))

;;;;;
(provide 'dvc-helpers)
