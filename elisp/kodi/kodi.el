;;; kodi-mode.el --- major mode for interacting with a Kodi instance

(require 'multi)
(require 'json)

;;; Settings
(defvar kodi-default-sorting '("sort" . (("order" . "ascending") ("method" . "label") ("ignorearticle" . t))))
(defvar kodi-show-properties '("properties" . ("season" "episode" "title" "playcount" "plot")))

(defvar kodi-host)
(defvar kodi-mode-connection nil)
(defvar kodi-mode-connection-input "")
(defvar kodi-mode-hook nil "")

(defvar kodi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'kodi-nav-left)
    (define-key map (kbd "<right>") 'kodi-nav-right)
    (define-key map (kbd "<up>") 'kodi-nav-up)
    (define-key map (kbd "<down>") 'kodi-nav-down)
    (define-key map (kbd "<backspace>") 'kodi-nav-back)
    (define-key map (kbd "C-<up>") 'kodi-audio-stream-next)
    (define-key map (kbd "C-<down>") 'kodi-audio-stream-previous)
    (define-key map (kbd "RET") 'kodi-nav-select)
    (define-key map (kbd "SPC") 'kodi-play-pause)
    (define-key map (kbd "C-SPC") 'kodi-stop)
    (define-key map (kbd "C-s") 'kodi-shows)
    (define-key map (kbd "C-f") 'kodi-movies)
    map)
  "Keymap for Kodi major mode.")


;;; Mode and helpers
(define-derived-mode kodi-mode nil "Kodi"
  "Major mode for interacting with Kodi
\\{kodi-mode-map}
"
  (auto-fill-mode)
  (set-fill-column 80)

  (setq font-lock-defaults
	`(((,(format "^%s:" (regexp-opt '("KODI" "Playing" "Position" "Streams" "Plot") 'words)) . font-lock-function-name-face)
	   ("^KODI:.*(\\(.*\\))$" . (1 font-lock-keyword-face)))))

  (font-lock-mode)
  (kodi-draw-title "Connected"))

(defun kodi-get (keys alist)
  "Helper function for retrieving values from nested alists"
  (while keys
      (setq alist (cdr (assoc (pop keys) alist))))
    alist)

(defun kodi-process-time (time)
  (let* ((hours (kodi-get '(hours) time))
	 (minutes (kodi-get '(minutes) time))
	 (seconds (kodi-get '(seconds) time)))
    (format-time-string "%H:%M:%S" (encode-time seconds minutes hours 0 0 0))))

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

(defun kodi-process-sentinel (proc state)
  (message "KODI-SENTINEL: %s" state))

;;; Response handlers
(defmulti kodi-response-handler (x &rest _)
  ""
  x)

(defmulti-method kodi-response-handler "Player.OnPlay" (_ data)
  (let* ((item (kodi-get '(params data item) data))
	 (type (kodi-get '(type) item))
	 (id (kodi-get '(id) item)))

    (cond ((equal type "episode")
	   (process-send-string kodi-mode-connection (kodi-create-packet "VideoLibrary.GetEpisodeDetails" `(("episodeid" . ,id) ("properties" . ("plot" "streamdetails" "season" "episode"))) '(("id" . "libTvShows")))))
	  ((equal type "movie")
	   (process-send-string kodi-mode-connection (kodi-create-packet "VideoLibrary.GetMovieDetails" `(("movieid" . ,id) ("properties" . ("plot" "streamdetails"))) '(("id" . "libMovies"))))))))

(defmulti-method kodi-response-handler "Player.OnPause" (_ data))

(defmulti-method kodi-response-handler "Player.OnStop" (_ data)
  (kodi-draw-currently-playing)
  (kodi-draw-position))

(defmulti-method kodi-response-handler "Player.OnSeek" (_ data)
  (kodi-draw-position (kodi-process-time (kodi-get '(params data player time) data))))

(defmulti-method kodi-response-handler "GUI.OnScreensaverActivated" (_ data)
  (kodi-draw-title "Asleep"))

(defmulti-method kodi-response-handler "GUI.OnScreensaverDeactivated" (_ data)
  (kodi-draw-title "Connected"))

(defmulti-method kodi-response-handler "VideoLibrary.OnScanStarted" (_ data)
  (kodi-draw-title "Scanning..."))

(defmulti-method kodi-response-handler "VideoLibrary.OnScanFinished" (_ data)
  (kodi-draw-title "Connected"))

(defmulti-method kodi-response-handler nil (_ data)
  (let ((result (kodi-get '(result) data)))
    (cond ((kodi-get '(episodedetails) result) (kodi-data-handler 'episodedetails result))
	  ((kodi-get '(moviedetails) result) (kodi-data-handler 'moviedetails result))
	  ((kodi-get '(movies) result) (kodi-data-handler 'movies result))
	  ((kodi-get '(tvshows) result) (kodi-data-handler 'tvshows result))
	  ((kodi-get '(episodes) result) (kodi-data-handler 'episodes result))
	  ((kodi-get '(time) result) (kodi-data-handler 'time result))
	  (t (message "Unhandled response data: %s" (json-encode data))))))

(defmulti-method-fallback kodi-response-handler (&rest data)
  (message "Unhandled method response: %s" (json-encode data)))


;;; Data handlers
(defmulti kodi-data-handler (x &rest _)
  ""
  x)

(defmulti-method kodi-data-handler 'episodedetails (_ data)
  (let* ((details (kodi-get '(episodedetails) data))
	 (label (format "s%de%d:  %s"
			(kodi-get '(season) details)
			(kodi-get '(episode) details)
			(kodi-get '(label) details)))
	 (plot (kodi-get '(plot) details))
	 (audio (kodi-get '(streamdetails audio) details))
	 (audio-list (mapcar (lambda (elt)
			       (format "%s-%s-%s"
				       (kodi-get '(language) elt)
				       (kodi-get '(codec) elt)
				       (kodi-get '(channels) elt)))
			     audio)))

    (kodi-draw-currently-playing (format "%s" label) plot audio-list)))

(defmulti-method kodi-data-handler 'moviedetails (_ data)
  (let* ((details (kodi-get '(moviedetails) data))
	 (label (kodi-get '(label) details))
	 (plot (kodi-get '(plot) details))
	 (audio (kodi-get '(streamdetails audio) details))
	 (audio-list (mapcar (lambda (elt)
			       (format "%s-%s-%s"
				       (kodi-get '(language) elt)
				       (kodi-get '(codec) elt)
				       (kodi-get '(channels) elt)))
			     audio)))

    (kodi-draw-currently-playing (format " %s" label) plot audio-list)))

(defmulti-method kodi-data-handler 'movies (_ data)
  (let ((movies (mapcar (lambda (elt) `( ,(kodi-get '(label) elt) . ,(kodi-get '(movieid) elt))) (kodi-get '(movies) data))))
    (helm :sources '((name . "KODI: Movies")
                     (candidates . movies)
                     (action . (lambda (candidate) (kodi-play-item `(("movieid" . ,candidate)))))))))

(defmulti-method kodi-data-handler 'tvshows (_ data)
  (let ((shows (mapcar (lambda (elt) `( ,(kodi-get '(label) elt) . ,(kodi-get '(tvshowid) elt))) (kodi-get '(tvshows) data))))
    (helm :sources '((name . "KODI: Shows")
                     (candidates . shows)
                     (action . (lambda (candidate) (kodi-episodes candidate)))))))

(defmulti-method kodi-data-handler 'episodes (_ data)
  (let ((episodes (mapcar (lambda (elt) `( ,(format "%s s%de%d. %s:  %s"
						    (if (> (kodi-get '(playcount) elt) 0) "*" " ")
                                                    (kodi-get '(season) elt)
                                                    (kodi-get '(episode) elt)
						    (kodi-get '(title) elt)
						    (kodi-get '(plot) elt)) . ,(kodi-get '(episodeid) elt))) (kodi-get '(episodes) data))))
    (helm :sources '((name . "KODI: Episodes")
                     (candidates . episodes)
                     (action . (lambda (candidate) (kodi-play-item `(("episodeid" . ,candidate)))))))))

(defmulti-method kodi-data-handler 'time (_ data)
  (kodi-draw-position
   (kodi-process-time (kodi-get '(time) data))
   (kodi-process-time (kodi-get '(totaltime) data))))


;;; High-level commands
(defun kodi-connect ()
  (interactive)
  ""
  (kodi-disconnect)
  (let ((stream (open-network-stream "kodi-client" "*kodi-client*" kodi-host 9090)))
    (setq kodi-mode-connection stream)
    (setq kodi-mode-connection-input "")
    (with-current-buffer "*kodi-client*" (erase-buffer))
    (kodi-draw-setup)
    (set-process-filter stream 'kodi-input-filter)
    (set-process-sentinel stream 'kodi-process-sentinel))
  (switch-to-buffer "*kodi-client*")
  (kodi-mode))

(defun kodi-disconnect ()
  (interactive)
  ""
  (when kodi-mode-connection (delete-process kodi-mode-connection) (setq kodi-mode-connection nil)))

(defun kodi-update-video-library ()
  (interactive)
  "Updates the video library"
  (process-send-string kodi-mode-connection (kodi-create-packet "VideoLibrary.Scan")))

(defun kodi-movies ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "VideoLibrary.GetMovies" `(,kodi-default-sorting) '(("id" . "libTvShows")))))

(defun kodi-shows ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "VideoLibrary.GetTVShows" `(,kodi-default-sorting) '(("id" . "libTvShows")))))

(defun kodi-episodes (show-id)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "VideoLibrary.GetEpisodes" `(("tvshowid" . ,show-id) ,kodi-show-properties ,kodi-default-sorting) '(("id" . "libTvShows")))))

(defun kodi-play-item (item)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.Open" `(("item" . ,item)))))

(defun kodi-update-time ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.GetProperties" '(("playerid" . 1)("properties" . ("time" "totaltime" "percentage"))) '(("id" . 1)))))

;;; Basic commands
(defun kodi-play-pause ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.PlayPause" '(("playerid" . 1)))))

(defun kodi-stop ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.Stop" '(("playerid" . 1)))))


;;; Basic navigation
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

;;; Player option interaction
(defun kodi-disable-subtitles ()
  (interactive)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.SetSubtitle" '(("playerid" . 1)("subtitle" . "off")))))

(defun kodi-audio-stream (id)
  ""
  (process-send-string kodi-mode-connection (kodi-create-packet "Player.SetAudioStream" `(("playerid" . 1)("stream" . ,id)))))

(defun kodi-audio-stream-next ()
  (interactive)
  ""
  (kodi-audio-stream "next"))

(defun kodi-audio-stream-previous ()
  (interactive)
  ""
  (kodi-audio-stream "previous"))
  

;;; Interface
(defun kodi-draw-setup ()
  (interactive)
  ""
  (with-current-buffer "*kodi-client*" (insert "KODI: .

Playing: .
Position: .
Streams: .

Plot: .")))

(defun kodi-draw (label value &optional kill-to-end)
  ""
  (with-current-buffer "*kodi-client*"
    (goto-char (point-min))
    (search-forward label)

    (if kill-to-end (delete-region (point) (point-max))
      (kill-line))
    
    (let ((current (point)))
      (insert "\t" value)
      (fill-region current (point)))))

(defun kodi-draw-title (&optional status)
  ""
  (let ((title "\tIt's what's on your TV a lot. "))
    (when status (setq title (concat title (format "\t\t(%s: %s)" kodi-host status))))
    (kodi-draw "KODI:" title)))

(defun kodi-draw-currently-playing (&optional item plot audio-list)
  ""
  (kodi-draw "Playing:" (if item item "."))
  (kodi-draw "Plot:" (if plot (format "\n\t%s" plot) ".") t)
  (kodi-draw "Streams:" (if audio-list (mapconcat 'identity audio-list " | ") ".")))

(defun kodi-draw-position (&optional time totaltime)
  ""
  (let* ((current (if time time "."))
	 (totalString (if totaltime (format " / %s" totaltime) "")))
  (kodi-draw "Position:" (concat current totalString))))

(provide 'kodi)
