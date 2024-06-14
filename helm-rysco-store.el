(require 'rysco-store)

;;;###autoload
(defun helm-rysco-store-query ()
  (interactive)
  (funcall-interactively
   'helm-rysco-store-ql
   :name "Knowledge Store Query"
   :actions `(,@helm-org-ql-actions
              ("Insert as link" . helm-rysco-store--insert-candidates)
              ("Save as buffer" . helm-rysco-store--save-search-buffer))))

;;;###autoload
(defun helm-rysco-store-query-for-freshness ()
  (interactive)
  (let ((rysco-store-initial-query (format "planning:to=%s "
                                           (format-time-string
                                            "%F"
                                            (org-read-date nil t rysco-store-freshness-threshold)))))
    (funcall-interactively
     'helm-rysco-store-ql
     :name "Knowledge Store Query"
     :actions `(,@helm-org-ql-actions
                ("Insert as link" . helm-rysco-store--insert-candidates)
                ("Save as buffer" . helm-rysco-store--save-search-buffer)))))

(cl-defun helm-rysco-store-ql (&key buffers-files (boolean 'and) (name "helm-org-ql") sources actions)
  "See: `helm-org-ql'."
  (interactive)
  (let ((boolean (if current-prefix-arg 'or boolean))
        (helm-input-idle-delay helm-org-ql-input-idle-delay)
        (helm-org-ql-actions actions)
        (buffers-files (or buffers-files (org-ql-search-directories-files :directories (rysco-store-existing-directories rysco-store-directories)))))

    (helm :prompt (format "Query (boolean %s): " (-> boolean symbol-name upcase))
          :input rysco-store-initial-query
          :sources `(,@sources
                     ,(helm-org-ql-source buffers-files :name name)))))

(defun helm-rysco-store--insert-candidates (&optional _)
  (rysco-store--insert-links (helm-marked-candidates :all-sources t)))

(defun helm-rysco-store--heading (window-width)
  ""
  (font-lock-ensure (point-at-bol) (point-at-eol))
  (let* ((heading (org-get-heading t))
         (path (-> (org-get-outline-path)
                   (org-format-outline-path window-width nil "")
                 (org-split-string "")))
         (path (if helm-org-ql-reverse-paths
                   (concat heading "\\" (s-join "\\" (nreverse path)))
                 (concat (s-join "/" path) "/" heading))))
    (cons path (point-marker))))

(eval-after-load 'helm-org-ql
  (advice-add 'helm-org-ql--heading :override 'helm-rysco-store--heading))

(defun helm-rysco-store-kindle-import (location)
  (interactive "D")
  (helm :prompt "Kindle Import "
        :sources
        (helm-build-sync-source "Kindle Books"
          :candidates (lambda ()
                        (--map `(,(format "%s:\t%s" (caddr it) (cadr it)) . ,it)
                               (rysco-store-kindle-get-books location)))
          :action `(("Insert" . (lambda (&rest _)
                                  (rysco-store-insert-books-kindle (helm-marked-candidates))))))))

(defun helm-rysco-store--save-search-buffer (&optional _)
  (let* ((buf "*squery*")
         new-buf
         (pattern (with-helm-buffer helm-input-local))
         (src (helm-get-current-source))
         (src-name (assoc-default 'name src)))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t)
            (map (make-sparse-keymap)))
        (erase-buffer)
        (insert "-*- mode: helm-rysco-store -*-\n\n"
                (format "%s Results for `%s':\n\n" src-name pattern))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max)))))
        (save-excursion
          (while (not (eobp))
            ;; (add-text-properties (pos-bol) (pos-eol)
            ;;                      `(keymap ,map
            ;;                               help-echo ,(concat
            ;;                                           (get-text-property
            ;;                                            (point) 'helm-grep-fname)
            ;;                                           "\nmouse-1: set point\nmouse-2: jump to selection")
            ;;                               mouse-face highlight))
            ;; (define-key map [mouse-1] 'mouse-set-point)
            ;; (define-key map [mouse-2] 'helm-grep-mode-mouse-jump)
            ;; (define-key map [mouse-3] 'ignore)
            (forward-line 1))))
      (helm-rysco-store-mode))
    (pop-to-buffer buf)
    (message "Query %s Results saved in `%s' buffer" src-name buf)))

;;;;
(defvar helm-rysco-store-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'helm-rysco-store-mode-jump)
    (define-key map (kbd "C-o") 'helm-rysco-store-mode-jump-other-window)
    (define-key map (kbd "<C-down>") 'helm-rysco-store-mode-jump-other-window-forward)
    (define-key map (kbd "<C-up>") 'helm-rysco-store-mode-jump-other-window-backward)
    (define-key map (kbd "M-n") 'helm-rysco-store-mode-jump-other-window-forward)
    (define-key map (kbd "M-p") 'helm-rysco-store-mode-jump-other-window-backward)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map))

(define-derived-mode helm-rysco-store-mode
    special-mode "helm-rysco-store"
    "Major mode to provide actions in helm rysco-store saved buffer.

Special commands:
\\{helm-rysco-store-mode-map}")
(put 'helm-rysco-store-mode 'helm-only t)

(defun helm-rysco-store-mode-jump ()
  (interactive)
  (helm-rysco-store-mode-jump-internal))

(defun helm-rysco-store-mode-jump-other-window ()
  (interactive)
  (helm-rysco-store-mode-jump-internal 'other))

(defun helm-rysco-store-mode-jump-other-window-forward ()
  (interactive)
  (helm-rysco-store-mode-jump-internal 'other-forward))

(defun helm-rysco-store-mode-jump-other-window-backward ()
  (interactive)
  (helm-rysco-store-mode-jump-internal 'other-backward))

(defun helm-rysco-store-mode-jump-other-window-stay (marker)
  (save-selected-window
    (with-current-buffer (switch-to-buffer-other-window (marker-buffer marker))
      (goto-char marker))))

(defun helm-rysco-store-mode-jump-internal (&optional method)
  (when-let* ((query-buffer (current-buffer))
              (marker (get-text-property (point) 'helm-realvalue))
              (buffer (marker-buffer marker)))

    (pcase method
      ('other
       (with-current-buffer (switch-to-buffer-other-window buffer)
         (goto-char marker)))

      ('other-forward
       (forward-line)
       (helm-rysco-store-mode-jump-other-window-stay marker))

      ('other-backward
       (forward-line -1)
       (helm-rysco-store-mode-jump-other-window-stay marker))

      (_
       (with-current-buffer (pop-to-buffer buffer)
         (goto-char marker))))))

;;
(provide 'helm-rysco-store)
