(defun hugo-shortcode-ref (ref &optional title anchor)
  (interactive "sRef: \nsTitle: \nsAnchor: ")
  (let ((text title)
        (link (if (string= "" anchor) ref (format "" ref anchor))))

    (when (string= "" title)
      ;; TODO:  maybe read from the actual link and extract the title?
      (setq text (capitalize (file-name-sans-extension ref))))
    
    (insert (format "[%s]({{< relref \"%s\" >}})" text link))))

(defun hugo-shortcode-figure (src title &optional caption attr)
  (interactive "sSrc: \nsTitle: \nsCaption: \nsAttribution: ")
  (insert (format "{{< figure src=\"%s\" title=\"%s\"" src title))
  (when (not (string= "" caption)) (insert (format " caption=\"%s\"" caption)))
  (when (not (string= "" attr)) (insert (format " attr=\"%s\"" attr)))
  (insert ">}}\n"))

(defun hugo-shortcode-youtube (id)
  (interactive "sID: ")
  (insert (format "{{< youtube %s >}}\n" id)))

(defun hugo-shortcode-instagram (id)
  (interactive "sID: ")
  (insert (format "{{< instagram %s >}}\n" id)))

(defun hugo-shortcode-tweet (id)
  (interactive "sID: ")
  (insert (format "{{< tweet %s >}}\n" id)))

(defun hugo-shortcode-vimeo (id)
  (interactive "sID: ")
  (insert (format "{{< vimeo %s >}}\n" id)))

;;;;;;;;;;;;;;;;;;;;;
(provide 'hugo)

