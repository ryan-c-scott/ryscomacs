(require 'json)
(require 'kodi)

(defvar helm-kodi-default-sorting '("sort" . (("order" . "ascending") ("method" . "label") ("ignorearticle" . t))))

(defvar helm-kodi-show-properties '("properties" . ("season" "episode" "title" "playcount" "plot")))


(defun helm-kodi-input-filter (proc content)
  "Process filter that continually attempts to process the collected response as json.
On success the function pointed to by 'helm-kodi-results-callback' is called with the json object."
  (with-current-buffer "*kodi-connection*"
    ;(insert content)
    (setq helm-kodi-temp-results (concat helm-kodi-temp-results content))
    (funcall helm-kodi-results-callback (json-read-from-string helm-kodi-temp-results))
    (setq helm-kodi-temp-results ""))
  (delete-process proc))

(defun helm-kodi-handle-shows (results)
  "Internal input filter callback function that processes show listing results and engages helm with that as a resource."
  (let ((shows (mapcar (lambda (elt)`( ,(cdr (assoc 'label elt)) . ,(cdr (assoc 'tvshowid elt))))
                       (cdr (assoc 'tvshows (cdar results))))))
    (helm :sources '((name . "KODI: Shows")
                     (candidates . shows)
                     (action . (lambda (candidate) (helm-kodi-episodes candidate)))))))

(defun helm-kodi-handle-episodes (results)
  "Internal input filter callback function that processes episode listing results and engages helm with that as a resource."
  (let ((episodes (mapcar (lambda (elt)`( ,(concat
                                            (format "%s s%de%d. %s:  "
						    (if (> (cdr (assoc 'playcount elt)) 0) "*" " ")
                                                    (cdr (assoc 'season elt))
                                                    (cdr (assoc 'episode elt))
                                                    (cdr (assoc 'title elt)))
                                            (cdr (assoc 'plot elt))) . ,(cdr (assoc 'episodeid elt))))
                          (cdr (assoc 'episodes (cdar results))))))
    (helm :sources '((name . "KODI: Episodes")
                     (candidates . episodes)
                     (action . (lambda (candidate) (helm-kodi-play-file `(("episodeid" . ,candidate)))))))))

(defun helm-kodi-handle-movies (results)
  ""
  (let ((movies (mapcar (lambda (elt)`( ,(cdr (assoc 'label elt)) . ,(cdr (assoc 'movieid elt))))
                       (cdr (assoc 'movies (cdar results))))))
    (helm :sources '((name . "KODI: Movies")
                     (candidates . movies)
                     (action . (lambda (candidate) (helm-kodi-play-file `(("movieid" . ,candidate)))))))))


(defun helm-kodi-shows ()
  (interactive)
  "Queries Kodi for a list of shows"
    (let ((stream (open-network-stream "kodi-connection" "*kodi-connection*" kodi-host 9090)))
      (with-current-buffer (get-buffer "*kodi-connection*") (erase-buffer))
      (setq helm-kodi-temp-results "")
      
      (setq helm-kodi-results-callback 'helm-kodi-handle-shows)
      (set-process-filter stream 'helm-kodi-input-filter)
      (process-send-string stream (kodi-create-packet "VideoLibrary.GetTVShows" `(,helm-kodi-default-sorting) '(("id" . "libTvShows"))))))

(defun helm-kodi-episodes (show-id)
  "Queries Kodi for a list of show episodes"
    (let ((stream (open-network-stream "kodi-connection" "*kodi-connection*" kodi-host 9090)))
      (with-current-buffer (get-buffer "*kodi-connection*") (erase-buffer))
      (setq helm-kodi-temp-results "")
      
      (setq helm-kodi-results-callback 'helm-kodi-handle-episodes)
      (set-process-filter stream 'helm-kodi-input-filter)
      (process-send-string stream (kodi-create-packet "VideoLibrary.GetEpisodes" `(("tvshowid" . ,show-id) ,helm-kodi-show-properties ,helm-kodi-default-sorting) '(("id" . "libTvShows"))))))

(defun helm-kodi-movies ()
  (interactive)
  "Queries Kodi for a list of movies"
    (let ((stream (open-network-stream "kodi-connection" "*kodi-connection*" kodi-host 9090)))
      (with-current-buffer (get-buffer "*kodi-connection*") (erase-buffer))
      (setq helm-kodi-temp-results "")
      
      (setq helm-kodi-results-callback 'helm-kodi-handle-movies)
      (set-process-filter stream 'helm-kodi-input-filter)
      (process-send-string stream (kodi-create-packet "VideoLibrary.GetMovies" `(,helm-kodi-default-sorting) '(("id" . "libTvShows"))))))

(defun helm-kodi-play-file (id)
  "Calls Player.Open RPC.  'id' should be contents of the RPC's 'item' parameter.  e.g. \"movied\":3   or   \"episodeid\":300"
  (let ((stream (open-network-stream "kodi-connection" "*kodi-connection*" kodi-host 9090)))
    (set-process-filter stream nil)
    (process-send-string stream (kodi-create-packet "Player.Open" `(("item" . ,id))))
    (delete-process stream)))

