;;;;;;;;;
(defun rysco-split-dwim (dir &optional switch)
  ""
  (let* ((can-horizontal (not (or (windmove-find-other-window 'left) (windmove-find-other-window 'right))))
         (window-below (windmove-find-other-window 'down))
         (can-vertical (not (or (windmove-find-other-window 'up) (and window-below (not (window-minibuffer-p window-below)))))))

    (cond ((and (or (eq dir 'left) (eq dir 'right)) can-horizontal) (split-window-right))
          ((and (or (eq dir 'up) (eq dir 'down)) can-vertical) (split-window-below)))

         (windmove-do-window-select dir)))

(defun rysco-split-up-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'up t))

(defun rysco-split-down-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'down t))

(defun rysco-split-right-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'right t))

(defun rysco-split-left-dwim ()
    ""
  (interactive)
  (rysco-split-dwim 'left t))

(defun rysco-comment-dwim ()
  ""
  (interactive)
  (if mark-active
      (comment-dwim nil)
    (comment-line 1)
    (forward-line -1)))

(defun rysco-clear-undo ()
  ""
  (interactive)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun replace-regexp-and-return (from to)
  (save-excursion
    (while (re-search-forward from nil t)
      (replace-match to))))

(defun insert-standard-date ()
  "Inserts standard date time string." 
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun ryscomacs-compile()
  "Byte compiles all of ~/ryscomacs/elisp."
  (interactive)
  (byte-recompile-directory "~/ryscomacs/elisp" 0))

(defun vertical-windows-with-related()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (ff-find-related-file t t))

(defun ff-find-related-file-ignore-include()
  (interactive)
  (ff-find-related-file nil t))

(defun kill-all-matching (criteria)
  "Kills all buffers that the criteria function returns non-nil on"
  (mapc (lambda (buffer)
	  (when (funcall criteria buffer)
	    (kill-buffer buffer)))
	(buffer-list)))

(defun kill-all-matching-prefix (buffer-prefixes)
  (interactive)
  (kill-all-matching (lambda (buffer)
                       (let ((names buffer-prefixes)
                             (bufname (buffer-name buffer))
                             (found))
                         (while names
                           (if (string-match (car names) bufname)
                               (progn
                                 (setq found t)
                                 (setq names nil))
                             (setq names (cdr names))))
                         found))))

(defun killall()
  "Kill all non-system buffers"
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (not (string-match "^*" (buffer-name buffer)))))
  (dvc-kill-all-buffers)
  (switch-to-buffer "*scratch*")
  (delete-other-windows))

(defun kill-dired-buffers ()
  (interactive)
  (kill-all-matching (lambda (buffer)
		       (eq 'dired-mode (buffer-local-value 'major-mode buffer)))))

;; (defun kill-p4-buffers ()
;;   "Kill all *P4 buffers"
;;   (interactive)
;;   (kill-all-matching (lambda (buffer)
;; 		       (string-match "^*P4 " (buffer-name buffer)))))

(defun markdown-toc ()
  (interactive)
  (let (res depth title)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\(#*\\) \\(.*\\)$" nil t)
	(add-to-list 'res `(,(match-string 1) ,(match-string 2)))))

    (dolist (elt (reverse res))
      (setq depth (replace-regexp-in-string (regexp-quote "#") (car elt) "\t" nil 'literal))
      (setq title (cadr elt))

      (insert (format "%s1. [%s]" depth title))
      (setq title (replace-regexp-in-string "\s+" "-" title))
      (setq title (replace-regexp-in-string "[:*]" "" title))
      (insert (format "(#%s)\n" (downcase title))))))

(defun markdown-insert-youtube (id &optional label)
  (interactive "sID: \nsLabel: ")
  (insert (format "[![%s](http://img.youtube.com/vi/%s/0.jpg)](http://www.youtube.com/watch?v=%s)" label id id)))

(defun org-archive-all-done-item ()
  "Archive all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ DONE" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ DONE" nil t)
            (org-advertized-archive-subtree))
          (org-display-all-todo-item)
          (message "Archive finished"))
      (org-display-all-todo-item)
      (message "No need to archive"))))

(defun edit-local-config()
  "Open ~/.emacs.d/elisp/localconfig.el"
  (interactive)
  (find-file "~/.emacs.d/elisp/localconfig.el"))

;;
(provide 'rysco-util)