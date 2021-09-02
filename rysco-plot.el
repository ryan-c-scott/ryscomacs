;;; -*- lexical-binding: t; -*-

(cl-defun rysco-plot--guess-filename (&optional ext)
  (when (boundp 'out)
    out)
  (when (equal major-mode 'org-mode)
    (concat
     (replace-regexp-in-string
      "[/\\:]" "-"
      (s-join "-" (org-get-outline-path t)))
     (or ext ".svg"))))

(cl-defun rysco-plot--render-functions (form)
  (pcase form
    (`(,func . ,params)
     (pcase func
       ((or '* '/ '+ '-)
        (loop
         with last
         with op = (format "%s" func)
         for chunk in params
         if last concat op
         concat (rysco-plot--render-functions chunk)
         do (setq last chunk)))

       (_
        (format "%s%s" func params))))

    ((pred stringp)
     (format "\"%s\"" form))

    (_ (format "%s" form))))

(cl-defun rysco-plot--key-p (el)
  (= (aref
      (format "%s" el)
      0)
     ?\:))

(cl-defun rysco-plot--key-to-string (key)
  (let ((s (format "%s" key)))
    (if (= (aref s 0) ?\:)
        (substring s 1)
      s)))

(cl-defun rysco-plot--render-range (range)
  (insert
   "["
   (s-join
    " : "
    (mapcar
     (lambda (it)
       (format
        "%s"
        (rysco-plot--render-functions it)))
     (append range nil)))
   "]"))

(cl-defun rysco-plot--render-element (el)
   (pcase el
     ((pred symbolp)
      (insert (rysco-plot--key-to-string el)))

     ((pred listp)
      (insert
       (s-join
        ", "
        (mapcar
         (lambda (it)
           (format
            "%s"
            (rysco-plot--render-functions it)))
         el))))

     ((pred stringp)
      (insert (format "\"%s\"" el)))

     ((pred vectorp)
      (rysco-plot--render-range el))

     (_
      (insert (format "%s" el)))))

(cl-defun rysco-plot--render-generic-entry (entry)
  (loop
   for el in entry
   do
   (rysco-plot--render-element el)
   do (insert " ")))

(cl-defun rysco-plot-gnuplot-command (path)
  ;; TODO: Variable for gnuplot binary path
  (format "gnuplot \"%s\"" path)
  )

(cl-defun rysco-plot--render-data (name data)
  (insert (format "$%s << EOD\n" name))
  (loop
   for entry in data do
   (loop
    for el in entry do
    (insert (format "%s " el)))
   do (insert "\n"))
  (insert "EOD\n"))

(cl-defun rysco-plot--render-plot (data)
  (insert "plot ")
  (loop
   for plot in data
   as skip-sep = nil
   do

   (pcase plot
     ((pred vectorp)
      (setq skip-sep t)
      (rysco-plot--render-range plot))

     ((and `(,type . ,plot-data) (guard (rysco-plot--key-p type)))
      (loop
       for (k v) on plot-data by 'cddr do
       (pcase k
         (:data
          (insert (format "$%s " v)))

         (:using
          (insert
           "using "
           (s-join
            ":"
            (mapcar
             (lambda (it)
               (format
                "%s"
                (rysco-plot--render-functions it)))
             (append v nil)))
           " "))

         (:fun
          (insert
           (format
            "%s"
            (rysco-plot--render-functions v))))

         (_
          (insert
           (rysco-plot--key-to-string k)
           " ")
          (rysco-plot--render-element v))))
      (insert (format " with %s" (rysco-plot--key-to-string type)))))

   do (insert (concat (unless skip-sep ",") " "))

   ))

(cl-defun rysco-plot--render (form &key filename as-code)
  "Generate gnuplot file from `FORM' and render image from it."
  (let ((path (f-full filename)))
    (with-temp-buffer
      (insert (format "set output \"%s\"\n" path))

      (loop
       for entry in form do
       (pcase entry
         (`(:env ,name ,value)
          (insert
           (format "%s = %s" name value)))

         (`(:data ,name . ,data)
          (if as-code
              (insert "<<DATA OMITTED>>")
            (rysco-plot--render-data name data)))

         (`(:plot . ,data)
          (rysco-plot--render-plot data))

         (_
          (rysco-plot--render-generic-entry entry)))

       do (insert "\n"))
      (cond
       (as-code
        (buffer-string))
       (t
        (let* ((temp-path (make-temp-file "rysco" nil ".plot")))
          (write-file temp-path)
          ;; TODO: Evaluate output for errors.
          (shell-command-to-string
           (rysco-plot-gnuplot-command temp-path))

          (delete-file temp-path))
        filename)))))

(cl-defun rysco-plot--process-date-log (&key title data col map start end)
  `((:set :title ,title)
    (:set :xdata time)
    (:set :timefmt "%Y-%m-%d")
    (:set :format x "%m/%y")
    (:set :xrange [,(or start '*) *])
    (:set :yrange [* 1.1])

    (:plot
     ,@(loop
        for period-title in map
        for i from (or col 3)
        collect
        `(:lines :data ,data :using [1 ,i] :title ,period-title)))))

(cl-defun rysco-plot--process (form &key type)
  "Expand all special commands and inject default forms for the conversion to gnuplot."
  (append `((:set :terminal
                  ,(pcase type
                     ('png "pngcairo")
                     ('nil "svg")
                     (_ type))
                  enhanced
                  font "helvetica,12" fontscale 1.0
                  size (800 700)
                  background rgb "#ffffff00"))
          (loop
           for el in form append
           (pcase el
             (`(:plot-date-log . ,rest)
              (apply 'rysco-plot--process-date-log rest))
             (_ `(,el))))))

;;;###autoload
(cl-defun rysco-plot (form &key filename as-code type)
  (rysco-plot--render
   (rysco-plot--process form :type type)
   :filename (or filename (rysco-plot--guess-filename type))
   :as-code as-code))

;;
(provide 'rysco-plot)
