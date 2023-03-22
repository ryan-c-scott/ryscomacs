(defface mongoer-results-fringe-default-face
  '((((background dark))
     (:background "cyan4"
      :foreground "white"
      :weight bold))
    (((background light))
     (:background "gray80"
      :foreground "cyan4"
      :weight bold)))
  ""
  :group 'mongoer)

(defvar-local mongoer-results-fringe-face
  'mongoer-results-fringe-default-face)

(defvar mongoer-directory-converter 'identity)

(defun mongoer--preoutput-filter (text)
  (unless (string-empty-p text)
    (pcase (aref text 0)
      ((or ?\{ ?\[)
       (rysco-json-pretty-string
        (s-replace-regexp "ObjectId(\\(.*?\\))" "\\1" text)))
      (_ (propertize text 'font-lock-face 'font-lock-keyword-face)))))

(defun mongoer--output-filter (text)
  (unless (string-empty-p text)
    (when (and (not (string-empty-p text))
               (pcase (aref text 0) ((or ?\{ ?\[) t)))
      (let ((ov (make-overlay
                 comint-last-output-start
                 (point-max)
                 (current-buffer)
                 t))
            (last-input (buffer-substring comint-last-input-start comint-last-input-end)))
        (overlay-put
         ov 'line-prefix
         (concat
          (propertize " " 'font-lock-face mongoer-results-fringe-face)
          " "))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'help-echo (format "Mongo Input: %s" last-input))
        (overlay-put ov 'last-input last-input)))))

(defun mongoer-init ()
  (setq comint-scroll-to-bottom-on-input 'this)
  (add-hook
   'comint-preoutput-filter-functions
   'mongoer--preoutput-filter
   0 t)

  (add-hook
   'comint-output-filter-functions
   'mongoer--output-filter
   0 t))

(defvar mongoer-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    map)
  "Basic mode map for `mongoer-mode'")

;;;###autoload
(define-derived-mode mongoer-mode comint-mode "Mongoer"
  "Major mode for `mongoer-connect'

\\<mongoer-mode-map>"
  )

;;;###autoload
(cl-defun mongoer-connect (&key prog args title dir fringe)
  (interactive)
  (let* ((default-directory (if dir
                                (funcall mongoer-directory-converter dir)
                              default-directory))
         (prog (or prog "mongo"))
         (args (when args (rysco-parse-command-string args)))
         (title (concat "*" (or title "MONGOER") "*"))
         (buffer (comint-check-proc title)))

    ;; pop to the  buffer if the process is dead, the buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'mongoer-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create title)
       (current-buffer)))

    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Mongoer" title prog nil args)
      (mongoer-mode))

    (when fringe
      (setq mongoer-results-fringe-face
            (if (stringp fringe)
                `(:background ,fringe)
              fringe)))))

(add-hook 'mongoer-mode-hook 'mongoer-init)

(defun mongoer-output-last-input-to-kill ()
  (interactive)
  (cl-loop
   for ov being the overlays from (point) to (point)
   as last-input = (overlay-get ov 'last-input)
   when last-input do
   (kill-new last-input t)))

(defun mongoer-last-input-to-kill ()
  (interactive)
  (kill-new
   (buffer-substring comint-last-input-start comint-last-input-end)))

(provide 'mongoer)
