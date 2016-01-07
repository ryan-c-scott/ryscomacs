;;; kodi-mode.el --- major mode for interacting with a Kodi instance

;;; TODO:  Experiment with triggering helm sources as a response handler for show/episode listings
;;; .This has the side effect of requiring that kodi be connected to before hand, but in practice this hasn't seem like much of a problem


(require 'multi)
(require 'json)

(defvar kodi-host)

(defvar kodi-mode-connection nil)

(defvar kodi-mode-hook nil
  "")

(defvar kodi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'kodi-nav-left)
    (define-key map (kbd "<right>") 'kodi-nav-right)
    (define-key map (kbd "<up>") 'kodi-nav-up)
    (define-key map (kbd "<down>") 'kodi-nav-down)
    (define-key map (kbd "<backspace>") 'kodi-nav-back)
    (define-key map (kbd "RET") 'kodi-nav-select)
    (define-key map (kbd "SPC") 'kodi-play-pause)
    (define-key map (kbd "C-SPC") 'kodi-stop)
    (define-key map (kbd "C-s") 'helm-kodi-shows)
    (define-key map (kbd "C-f") 'helm-kodi-movies)
    map)
  "Keymap for Kodi major mode.")

(define-derived-mode kodi-mode nil "Kodi"
  "Major mode for interacting with Kodi
\\{kodi-mode-map}
"
  (kodi-draw-title "Connected"))

(defvar kodi-mode-connection-input "")

(defun kodi-get (keys alist)
"Helper function for retrieving values from nested alists"
(if (not keys)
    alist
  (progn
    (kodi-get (cdr keys) (cdr (assoc (car keys) alist))))))

(defun kodi-create-packet (method &optional params other)
  ""
  (let ((packet `(("jsonrpc" . "2.0")("method" . ,method))))
    (when params (setq packet (append packet `(("params" . ,params)))))
    (when other (setq packet (append packet other)))
    (json-encode packet)))

(defun kodi-input-filter (proc content)
  (with-current-buffer "*kodi-client*"
    (setq kodi-mode-connection-input (concat kodi-mode-connection-input content))
    (let* ((result (json-read-from-string kodi-mode-connection-input))
	   (method (cdr (assoc 'method result))))
      (setq kodi-mode-connection-input "")
      (kodi-response-handler method result))))

(defmulti kodi-response-handler (x &rest _)
  ""
  x)

(defmulti-method kodi-response-handler "Player.OnPlay" (_ data)
  (message "OnPlay received")

  (let* ((item (kodi-get '(params data item) data))
	 (type (kodi-get '(type) item))
	 (id (kodi-get '(id) item)))

    ;; TODO:  switch on type to get the correct details
    ;; TESTING:  Triggering a media information retrieval for this item
    (process-send-string kodi-mode-connection (kodi-create-packet "VideoLibrary.GetEpisodeDetails" `(("episodeid" . ,id) ("properties" . ("plot"))) '(("id" . "libTvShows"))))))

(defmulti-method kodi-response-handler "Player.OnPause" (_ data)
  (message "OnPause received"))

(defmulti-method kodi-response-handler "Player.OnStop" (_ data)
  (message "OnStop received")
  (kodi-draw-currently-playing))

(defmulti-method kodi-response-handler "GUI.OnScreensaverActivated" (_ data)
  (message "Kodi screensaver on."))

(defmulti-method kodi-response-handler "GUI.OnScreensaverDeactivated" (_ data)
  (message "Kodi screensaver off."))

(defmulti-method kodi-response-handler nil (_ data)
  (let ((result (kodi-get '(result) data)))
    (cond ((kodi-get '(episodedetails) result) (kodi-data-handler 'episodedetails result))
	  (t (message "Unhandled response data: %s" (json-encode data))))))

(defmulti-method-fallback kodi-response-handler (&rest data)
  (message "Unhandled method response: %s" (json-encode data)))

(defmulti kodi-data-handler (x &rest _)
  ""
  x)

(defmulti-method kodi-data-handler 'episodedetails (_ data)
  (message "Handling episode details")
  (let* ((details (kodi-get '(episodedetails) data))
	 (label (kodi-get '(label) details))
	 (plot (kodi-get '(plot) details)))
    (kodi-draw-currently-playing (format " %s" label) plot)))

(defun kodi-connect ()
  (interactive)
  ""
  (kodi-disconnect)
  (let ((stream (open-network-stream "kodi-client" "*kodi-client*" kodi-host 9090)))
    (setq kodi-mode-connection stream)
    (setq kodi-mode-connection-input "")
    (with-current-buffer "*kodi-client*" (erase-buffer))
    (kodi-draw-setup)
    (set-process-filter stream 'kodi-input-filter))
  (switch-to-buffer "*kodi-client*")
  (kodi-mode))

(defun kodi-disconnect ()
  (interactive)
  ""
  (when kodi-mode-connection (delete-process kodi-mode-connection) (setq kodi-mode-connection nil)))

(defun kodi-play-pause ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.PlayPause" '(("playerid" . 1)))))

(defun kodi-stop ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.Stop" '(("playerid" . 1)))))

(defun kodi-nav (cmd)
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet (format "Input.%s" cmd))))

(defun kodi-nav-left ()
  (interactive)
  ""
  (kodi-nav "Left")
  (kodi-seek-small-backward))

(defun kodi-nav-right ()
  (interactive)
  ""
  (kodi-nav "Right")
  (kodi-seek-small-forward))

(defun kodi-nav-up ()
  (interactive)
  ""
  (kodi-nav "Up")
  (kodi-seek-big-forward))

(defun kodi-nav-down ()
  (interactive)
  ""
  (kodi-nav "Down")
  (kodi-seek-big-backward))

(defun kodi-nav-back ()
  (interactive)
  ""
  (kodi-nav "Back"))

(defun kodi-nav-select ()
  (interactive)
  ""
  (kodi-nav "Select"))

(defun kodi-nav-home ()
  (interactive)
  ""
  (kodi-nav "Home"))

(defun kodi-seek (cmd)
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.Seek" `(("playerid" . 1)("value" . ,cmd)))))

(defun kodi-seek-small-forward ()
  (interactive)
  ""
  (kodi-seek "smallforward"))

(defun kodi-seek-small-backward ()
  (interactive)
  ""
  (kodi-seek "smallbackward"))

(defun kodi-seek-big-forward ()
  (interactive)
  ""
  (kodi-seek "bigforward"))

(defun kodi-seek-big-backward ()
  (interactive)
  ""
  (kodi-seek "bigbackward"))

(defun kodi-disable-subtitles ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.SetSubtitle" '(("playerid" . 1)("subtitle" . "off")))))

;;; Interface
(defun kodi-draw-setup ()
  (interactive)
  ""
  (with-current-buffer "*kodi-client*" (insert "KODI: .
Playing: .
Position: .
Plot: .")))

(defun kodi-draw (label value)
  ""
  (with-current-buffer "*kodi-client*"
    (goto-char (point-min))
    (search-forward label)
    (kill-line)
    (insert " " value)))

(defun kodi-draw-title (&optional status)
  ""
  (let ((title " It's what's on your TV a lot... "))
    (when status (setq title (concat title status)))
    (kodi-draw "KODI:" title)))

(defun kodi-draw-currently-playing (&optional item plot)
  ""
  (kodi-draw "Playing:" (if item item " ."))
  (kodi-draw "Plot:" (if plot plot " .")))


(provide 'kodi)
