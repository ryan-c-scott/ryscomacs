;;; kodi-mode.el --- major mode for interacting with a Kodi instance

;;; TODO:  Experiment with triggering helm sources as a response handler for show/episode listings
;;; .This has the side effect of requiring that kodi be connected to before hand, but in practice this hasn't seem like much of a problem


(require 'multi)

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
    (define-key map (kbd "C-m") 'helm-kodi-movies)
    map)
  "Keymap for Kodi major mode.")

(define-derived-mode kodi-mode nil "Kodi"
  "Major mode for interacting with Kodi
\\{kodi-mode-map}
")

(defvar kodi-mode-connection-input "")

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

      (message method)
      (setq kodi-mode-connection-input "")
      (kodi-response-handler method result))))

(defmulti kodi-response-handler (x &rest _)
  ""
  x)

(defmulti-method kodi-response-handler "Player.OnPlay" (_ data)
  (message "OnPlay received"))

(defmulti-method kodi-response-handler "Player.OnPause" (_ data)
  (message "OnPause received"))

(defmulti-method-fallback kodi-response-handler (&rest data)
  (message "Unhandled data: %s" data))

(defun kodi-connect ()
  (interactive)
  ""
  (kodi-disconnect)
  (let ((stream (open-network-stream "kodi-client" "*kodi-client*" kodi-host 9090)))
    (setq kodi-mode-connection stream)
    (with-current-buffer "*kodi-client*" (erase-buffer))
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

(provide 'kodi)
