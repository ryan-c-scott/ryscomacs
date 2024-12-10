(require 'org-agenda)
(require 'helm)

(defvar rysco-org-effective-time-override nil)

(defvar rysco-org-refile-targets nil)
(defvar rysco-org-agenda-status-overlay nil)

(defvar rysco-org-agenda-columns 3)
(defvar rysco-org-agenda-margin-col 2)
(defvar rysco-org-agenda-margin-left 2)
(defvar rysco-org-agenda-excess-threshold 4)

(defface rysco-org-agenda-status-title
  '((t :underline "grey20"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-project
  '((t :inherit 'rysco-org-agenda-status-title
       :foreground "grey40"
       :slant italic))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-base
  '((t :inherit 'rysco-org-agenda-status-title
       :slant italic
       :height 0.75
       :foreground "#54ff9f"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-active
  '((t :inherit 'rysco-org-agenda-status-base
       :foreground "gray80"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-active-underutilized
  '((t :inherit 'rysco-org-agenda-status-base
       :foreground "DarkTurquoise"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-excess
  '((t :inherit 'rysco-org-agenda-status-base
       :foreground "yellow"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-blocked
  '((t :inherit 'rysco-org-agenda-status-base
       :foreground  "#F92672"))
  ""
  :group 'rysco-org-agenda-faces)

(defface rysco-org-agenda-status-stalled
  '((t :inherit 'rysco-org-agenda-status-base
       :weight bold
       :foreground "salmon"))
  ""
  :group 'rysco-org-agenda-faces)

(set-face-attribute 'org-agenda-clocking nil :box "cyan4")

(defun helm-rysco-org-agenda-buffer-items (&optional arg)
  (interactive "P")
  (--when-let
      (and (derived-mode-p 'org-agenda-mode)
           (save-excursion
             (goto-char (point-min))
             (cl-loop
              with action = `(("Go to item" . (lambda (pos) (goto-char pos)))
                              ("Go to heading" . (lambda (pos)
                                                   (goto-char pos)
                                                   (org-agenda-goto)))
                              ("TODO" . (lambda (pos)
                                          (save-excursion
                                            (goto-char pos)
                                            (org-agenda-todo))))
                              ("Refile" . (lambda (pos)
                                            (save-excursion
                                              (goto-char pos)
                                              (org-agenda-refile)))))

              do (org-agenda-forward-block)
              as heading = (s-replace "\n" "" (thing-at-point 'line))
              do (forward-line 1)
              while (< (point) (point-max))
              as end = (save-excursion
                         (org-agenda-forward-block)
                         (if (= (point) (point-max))
                             (point-max)
                           (forward-line -1)
                           (point)))

              collect
              (helm-build-sync-source heading
                :candidates
                (save-excursion
                  (cl-loop
                   while (< (point) end)
                   collect
                   (cons
                    (s-replace "\n" "" (thing-at-point 'line))
                    (point))
                   do (forward-line 1)))

                :action action))))

    (or
     (helm :sources it)
     t)))

(defun rysco-org-agenda ()
  (interactive)
  (with-try-switch-existing-buffer "*Org Agenda*"
    (org-agenda)))

(defun rysco-org-agenda-get-projects ()
  "Return status of all projects, as specified by the org property `projectid' listed in the buffer"
  (interactive)
  (--when-let
      (and (derived-mode-p 'org-agenda-mode)
           (save-excursion
             (goto-char (point-min))
             (org-agenda-forward-block)
             (forward-line 1)

             (cl-loop
              with status = (make-hash-table :test 'equal)
              with next-count = (make-hash-table :test 'equal)

              until (eobp)
              as marker = (org-get-at-bol 'org-marker)
              when marker do
              (let* ((project (org-entry-get marker "PROJECTID" t))
                     (todo (substring-no-properties
                            (org-get-at-bol 'todo-state)))
                     (state (gethash project status))
                     (count (or (gethash project next-count) 0)))


                (when (or (equal todo "NOW")
                          (equal todo "NEXT"))
                  (cl-incf count)
                  (puthash project count next-count))

                (unless (or (equal state 'ACTIVE)
                            (equal state 'EXCESS))
                  (setq state
                        (puthash
                         project
                         (pcase todo
                           ((or "NOW" "NEXT") 'ACTIVE)
                           ("WAITING" 'BLOCKED))
                         status)))

                (when (and (equal state 'ACTIVE)
                           (or (equal todo "NOW")
                               (equal todo "NEXT")))

                  (when (> count rysco-org-agenda-excess-threshold)
                    (puthash project 'EXCESS status))))

              do (forward-line 1)
              finally return
              (cl-loop
               for k being the hash-keys of status
               collect
               `(,k ,(gethash k status) ,(or (gethash k next-count) 0))))))
    it))

(defun rysco-org-agenda-goto-first-section ()
  (interactive)
  (goto-char (point-min))
  (org-agenda-next-item 1))

(defun rysco-org-agenda-find-first-todo (state)
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     with found
     until (or found (eobp))
     as todo-state = (org-get-at-bol 'todo-state)
     do (if (equal todo-state state )
            (setq found (point))
          (forward-line))
     finally return found)))

(defun rysco-org-agenda--status-face (status count)
  (pcase status
    ('ACTIVE
     (if (< count rysco-org-agenda-excess-threshold)
         'rysco-org-agenda-status-active-underutilized
       'rysco-org-agenda-status-active))
    ('BLOCKED 'rysco-org-agenda-status-blocked)
    ('EXCESS 'rysco-org-agenda-status-excess)
    (_ 'rysco-org-agenda-status-stalled)))

(defun rysco-org-agenda--status-string (status count)
  (or status "STALLED"))

(defun rysco-org-agenda--status-entry (project status count)
  (concat
   (propertize (format "%-10s"
                       (rysco-org-agenda--status-string status count))
               'face (rysco-org-agenda--status-face status count))
   (propertize (format "%-12s" project)
               'face 'rysco-org-agenda-status-project)))

(defun rysco-org-agenda-insert-status (&rest _)
  (interactive)
  (save-excursion
    (when rysco-org-agenda-status-overlay
      (setq rysco-org-agenda-status-overlay
            (delete-overlay rysco-org-agenda-status-overlay)))

    (rysco-org-agenda-goto-first-section)
    (forward-line -2)

    (unless org-agenda-current-span
      (setq rysco-org-agenda-status-overlay
            (make-overlay (point) (+ (point) 2)))

      (let* ((last-args-prop (get-char-property 1 'org-last-args))
             (show-status (and (numberp last-args-prop)
                               (= (or last-args-prop 0) 0)))
             (status (and show-status (rysco-org-agenda-get-projects)))
             (buffer-read-only nil)
             (status-overlay rysco-org-agenda-status-overlay)
             (col-count rysco-org-agenda-columns)
             (margin-col rysco-org-agenda-margin-col)
             (margin-col-str (make-string margin-col ?\s))
             (margin-left rysco-org-agenda-margin-left)
             (margin-left-str (concat "\n" (make-string margin-left ?\s)))
             (margin-right-str ""))

        (overlay-put rysco-org-agenda-status-overlay 'invisible t)
        (overlay-put rysco-org-agenda-status-overlay 'display
                     'rysco-org-agenda-status-title)

        (overlay-put
         rysco-org-agenda-status-overlay 'before-string
         (concat
          (when show-status
            (cl-loop
             with i = 0
             for (k state count) in status
             as col = (% i col-count)

             when k do (cl-incf i)

             when k concat
             (if (= col 0)
                 margin-left-str
               margin-col-str)

             when k concat
             (rysco-org-agenda--status-entry k state count)

             when (= col (1- col-count)) concat margin-right-str))

          "\n\n"))))))

;;;###autoload
(defun rysco-agenda-entry-header (str)
  (-if-let* ((marker (get-text-property 0 'org-marker str))
             (has-note (rysco-org-agenda-entry-has-note marker)))
      (apply 'propertize
             (concat "⌄" (substring str 1))
             (text-properties-at 0 str))
    str))

;;;###autoload
(defun rysco-agenda-entry-header-auto-grouped (data)
  `(:name
    ,(plist-get data :name)
    :items
    ,(-map 'rysco-agenda-entry-header (plist-get data :items))))

;;;###autoload
(defun rysco-agenda-project-header (str)
  (-if-let* ((marker (get-text-property 0 'org-marker str))
             (face-name (org-entry-get marker "projectface" t))
             (face (intern face-name))
             (str (rysco-agenda-entry-header str))
             (header-width (-if-let (header-text (car (s-match "^.*:\s+" str)))
                               (1- (length header-text))
                             (+ 3 (length (format "%s" (get-text-property 0 'org-category str)))))))
      (prog1 str
        (add-text-properties 0 header-width `(face ,face) str))
    str))

(defun rysco-agenda-refile-wrapper (old &rest args)
  (let ((org-refile-targets (or rysco-org-refile-targets org-refile-targets)))
    (apply old args)))

(defun rysco-org-recapture ()
  "Narrow to node content, excluding headline and properties, and mark the region
for use with `%i' in org capture templates (see `org-capture-templates')"
  (interactive)
  (-when-let* ((element (org-element-at-point)))
    (save-restriction
      (narrow-to-region (org-element-contents-begin element)
                        (org-element-contents-end element))

      (let ((data (org-element-parse-buffer))
            begin)

        (org-element-map data org-element-all-elements
          (lambda (el)
            (pcase (org-element-type el)
              ((or 'drawer 'property-drawer 'section) nil)
              (_ (setq begin (org-element-begin el)))))
          nil t
          '(drawer property-drawer))

        (set-mark begin)
        (goto-char (point-max))
        (funcall-interactively 'org-capture)))))

(defun rysco-agenda-recapture ()
  "Copy node content, excluding headline and properties, into a temp buffer and mark the region
for use with `%i' in org capture templates (see `org-capture-templates')"
  (interactive)
  (-when-let* ((marker (get-text-property (point) 'org-marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (funcall-interactively 'rysco-org-recapture)))))

(defun rysco-org-agenda-goto-last-refile ()
  (interactive)
  (--when-let
      (save-window-excursion
        (org-refile-goto-last-stored)
        (org-element-property :title (org-element-at-point)))
    (re-search-forward it nil t)
    (beginning-of-line)))

(cl-defmacro rysco-org-element-get-ancestor (element &rest forms)
  (when element
    `(cl-loop
      with it = (org-element-property :parent ,element)
      for depth from 1
      until (progn ,@forms) do
      (progn
        (setq it (org-element-property :parent it)))
      finally return it)))

(defun rysco-org-get-path-string ()
  (s-replace
   "\n" ""
   (base64-encode-string
    (org-format-outline-path (org-get-outline-path t t) nil nil "-"))))

(defun rysco-org-insert-path-string ()
  (interactive)
  (insert (rysco-org-get-path-string)))

(defun rysco-org-element-value-to-kill-ring ()
  (interactive)
  (--when-let (and (derived-mode-p 'org-mode)
                   (org-element-property :value (org-element-at-point)))
    (kill-new it)))

(defun rysco-org-src-execute (&rest _)
  (interactive)
  (let ((block-point org-src--beg-marker))
    (with-current-buffer (org-src-source-buffer)
      (save-excursion
        (goto-char block-point)
        (call-interactively 'org-ctrl-c-ctrl-c)))))

(defmacro with-rysco-org-result-src-block (&rest forms)
  `(save-excursion
     (when-let* ((current (point))
                 (src-start (let ((case-fold-search t))
                                  (save-excursion
                                    (re-search-backward
                                     (rx (seq
                                          line-start
                                          (* whitespace)
                                          "#+"
                                          (or "BEGIN_SRC" "CALL:")))
                                     nil t))))
                 (_ (goto-char src-start))
                 (result-start (org-babel-where-is-src-block-result))
                 (_ (goto-char result-start))
                 (_ (forward-line))
                 (result-end (org-babel-result-end)))
       (when (and (>= current result-start)
                  (<= current result-end))
         (goto-char src-start)
         ,@forms))))

;;;###autoload
(defun rysco-org-result-execute-src ()
  (interactive)
  (let ((current (point)))
    (when (with-rysco-org-result-src-block
           (let* ((context
	           (org-element-lineage
	            (org-element-context)
	            '(babel-call inline-babel-call inline-src-block)
	            t))
                  (type (org-element-type context)))
             (pcase type
               ((or `babel-call `inline-babel-call)
                (let ((info (org-babel-lob-get-info context)))
	          (when info (org-babel-execute-src-block nil info nil type))))
               (_
                (org-babel-execute-src-block)))))
      (goto-char current))))

;;;###autoload
(defun rysco-org-result-edit-src ()
  (interactive)
  (with-rysco-org-result-src-block
   (org-edit-special)))

(add-hook 'org-ctrl-c-ctrl-c-final-hook 'rysco-org-result-execute-src)

;;;###autoload
(defun rysco-org-table-to-kill (&optional sep)
  "Adds the content of the table under the point to the kill ring as a line per
row.
Each cell is separated by `SEP', which defaults to \"\t\""
  (interactive)

  (let* ((sep (or sep "\t"))
         (beg (org-table-begin))
         (end (org-table-end))
         (parsed-elements (org-element-parse-buffer)))
    (kill-new
     (s-join
      "\n"
      (org-element-map parsed-elements 'table-row
        (lambda (row)
          (when (and (>= (org-element-property :begin row) beg)
                     (<= (org-element-property :end row) end))
            (s-join
             sep
             (org-element-map row 'table-cell
               '(buffer-substring
                 (org-element-property :contents-begin node)
                 (org-element-property :contents-end node)))))))))))


;;;###autoload
(cl-defun rysco-org-process-date-log (data windows &key value-column degrade)
  "DATA is expected to be a table sorted by the first column in ascending order.
VALUE-COLUMN can be specified to use a different column of data for processing
(it must also be sorted in ascending order)"
  (when data
    (cl-loop
     with rolling = (--map (rysco-rolling-average it) windows)

     with dates = (--map (cons (format-time-string
                                "%F" (org-read-date nil t (car it))) (cdr it)) data)
     with today-stamp = (format-time-string "%F")
     with last-stamp = (if (string= today-stamp (car (-last-item dates)))
                           today-stamp
                         (format-time-string
                          "%F"
                          (org-read-date nil t "--1" nil)))
     with first = (org-read-date nil t (caar dates))
     with last = (org-read-date nil t "++1" nil
                                (org-read-date
                                 nil t
                                 last-stamp))

     with carried = 0

     for i from 0
     as this-date = (org-read-date nil t (format "++%s" i) nil first)
     as this-date-string = (format-time-string "%F" this-date)

     while (time-less-p this-date last)
     as entry-data = (cl-assoc this-date-string dates :test 'string=)
     as this-value = (or (if value-column
                             (nth value-column entry-data)
                           (when entry-data 1.0))
                         carried)

     collect
     `(,this-date-string
       ,this-value
       ,@(--map (funcall it this-value) rolling))
     do (when degrade
          (setf carried (max 0 (* this-value (- 1 degrade))))))))

(cl-defun rysco-org-process-habit-log (periods &optional marker)
  (with-current-buffer (if marker
                           (marker-buffer marker)
                         (current-buffer))
    (when marker
      (goto-char (marker-position marker)))

    (goto-char (org-log-beginning))
    (cons
     (org-get-heading t t t t)
     (rysco-org-process-date-log
      (let* ((logbook (org-element-at-point))
             (structure (org-element-property :structure logbook nil t)))
        (--sort
         (string< (car it) (car other))
         (cl-loop
          for (beg _ _ _ _ _ end) in structure
          as entry = (buffer-substring-no-properties beg end)
          as completion = (pcase entry
                            ((rx (seq "- State \"DONE\""
                                      (*? any)
                                      "from \"" (*? any) "\""
                                      (*? any)
                                      "[" (let date (*? any)) "]"))
                             date))
          when completion collect `(,completion))))
      periods))))

(cl-defun rysco-org-clocking-data (&optional start-date)
  (--sort
   (string< (car it) (car other))
   (let (output total)
     (with-rysco-files
      (org-agenda-files t t)
      (org-element-map (org-element-parse-buffer) 'clock
        (lambda (clock)
          (let* ((entry (rysco-org-element-get-ancestor
                         clock
                         (eq (org-element-type it) 'headline)))
                 (timestamp-obj (org-element-property :value clock))
                 (timestamp (format "%s-%02d-%02d"
                                    (org-element-property :year-end timestamp-obj)
                                    (org-element-property :month-end timestamp-obj)
                                    (org-element-property :day-end timestamp-obj)))
                 (period (org-read-date t nil (or start-date "-3m"))))

            (when (string> timestamp period)
              (push
               (list
                timestamp
                (org-element-property :duration clock))
               output))))))

     (cl-loop
      for (proj . data) in (--group-by (car it) output)
      as proj-sum = (cl-loop
                     for (_ duration) in data
                     when duration sum
                     (rysco-org-duration-string-to-minutes duration))
      do (setq total (+ (or total 0) proj-sum))
      collect `(,proj ,proj-sum)))))

(cl-defun rysco-org-plot-clocking (&key title start periods max-width options)
  (let* ((periods (or periods '(7 30 60 90)))
         (period-titles (mapcar (lambda (p) (format "%s-day" p)) periods))
         (title (or title "Clocking")))
    (apply
     `(rysco-plot
       (
        ;; TODO: Create style helpers
        ,@(cl-loop
           for id in '(1 2 3 4) collect
           `(:set :linetype ,id :linewidth 1.5))

        (:set :key fixed top horizontal Right noreverse enhanced autotitle box lt black linewidth 2.000 dashtype solid opaque)
        (:set :grid)
        (:set :border lc "white")
        (:set :title font ",20" textcolor lt -1 norotate tc "white")

        (:data data ,@(rysco-org-process-date-log (rysco-org-clocking-data start) periods :value-column 1))

        (:plot-date-log :title "Clocking"
                        :data data
                        :start *
                        :col 3
                        :miny 0
                        :maxy 180
                        :map ,period-titles))

        ,@(append
          `(:dimensions
            (,(min (or max-width 1500) (window-width nil t))
             400))
          options)))))

(cl-defun rysco-org-plot-habit (&key title start periods markers columns max-width section-height options)
  (let* ((periods (or periods '(7 30 60 90)))
         (period-titles (mapcar (lambda (p) (format "%s-day" p)) periods))
         (markers (or markers `(,(point-marker))))
         (title (or title (org-get-heading t t t t)))
         (marker-count (length markers))
         (columns (or columns 1))
         (rows (ceiling marker-count columns))
         (multi (> marker-count 1)))
    (apply
     `(rysco-plot
       (
        ;; TODO: Create style helpers
        ,@(cl-loop
           for id in '(1 2 3 4) collect
           `(:set :linetype ,id :linewidth 1.5))

        (:set :key fixed top horizontal Right noreverse enhanced autotitle box lt black linewidth 2.000 dashtype solid opaque)
        (:set :grid)
        (:set :border lc "white")
        (:set :title font ",20" textcolor lt -1 norotate tc "white")

        ,(when multi
           `(:set :multiplot
                  :layout (,rows ,columns)
                  :scale (1 1)))

        ,@(cl-loop
           for m in markers
           as scheduled = (org-entry-get m "SCHEDULED")
           as repeat = (pcase scheduled
                         ((rx
                           (seq
                            "<" (*? any)
                            (let num (* digit))
                            (let unit (any alpha))
                            ">"))
                          `(,num . ,unit)))

           as target-interval = (/ 1.0
                                   (* (cl-parse-integer (car repeat))
                                      (pcase (cdr repeat)
                                        ("d" 1)
                                        ("w" 7)
                                        ("m" 30)
                                        (_ 1))))

           as data = (rysco-org-process-habit-log periods m)
           as name = (car data)
           as id = (string-replace " " "" name)
           as log = (cdr data)

           collect
           `(:data ,id ,@log)

           if (< target-interval 1.0) append
           `((:set style arrow 1 nohead lw 2 lc "#030303")
             (:set arrow 1 from graph (0 ,target-interval) to graph (1 ,target-interval) as 1)
             (:show arrow 1))
           else collect
           `(:unset arrow 1)

           collect
           `(:plot-date-log :title ,(format "%s (%s%s)" name (car repeat) (cdr repeat))
                            :data ,id
                            :start ,(or start '*)
                            :col 3
                            :miny 0
                            :maxy 1.0
                            :map ,period-titles)
           collect `(:unset :key)))

       ,@(append
          `(:dimensions
            (,(min (or max-width 1500) (window-width nil t))
             ,(* (or section-height 300) rows)))
          options)))))

(defun rysco-org-markers-from-links (links)
  "Map Org node links to markers"
  (save-window-excursion
    (save-mark-and-excursion
      (cl-loop
       for link in links collect
       (progn
         (org-link-open-from-string link)
         (point-marker))))))

(defun rysco-org-agenda-post-clock-in (&optional _)
  (org-agenda-redo-all)
  (--if-let (rysco-org-agenda-find-first-todo "NOW")
      (goto-char it)
    (rysco-org-agenda-goto-first-section)))

(defun rysco-org-clock-out-switch-state (status)
  "Callback for `org-clock-out-switch-to-state' which sets to TODO if the last clock entry has the tag 'category'"
  (or
   (--when-let (car org-clock-history)
     (with-current-buffer (marker-buffer it)
       (when (member "category" (org-get-tags (marker-position it) t))
         "TODO")))
   "NEXT"))

(advice-add #'org-agenda-redo-all :after 'rysco-org-agenda-insert-status)
(advice-add #'org-agenda-redo :after 'rysco-org-agenda-insert-status)
(advice-add #'org-agenda-todo :after 'org-agenda-redo-all)
(advice-add #'org-agenda-clock-in :after 'rysco-org-agenda-post-clock-in)
(advice-add #'org-agenda-clock-out :after 'org-agenda-redo-all)
(advice-add #'org-todo-list :after 'rysco-org-agenda-insert-status)

(advice-add 'org-agenda-refile :around 'rysco-agenda-refile-wrapper)

(advice-add 'org-edit-src-save :after 'rysco-org-src-execute)

(defun rysco-org-clock-heading ()
  (or
   (org-entry-get (point) "PROJECTID" t)
   ""))

(setq org-clock-heading-function 'rysco-org-clock-heading)

(defun rysco-org-insert-into-drawer (txt &optional drawer-name)
  ""
  (save-excursion
    (save-restriction

      (unless (org-at-heading-p)
        (org-back-to-heading t))

      (--when-let (org-element-at-point)
        (narrow-to-region (org-element-property :begin it)
                          (org-element-property :end it))
        (let ((drawer
               (or
                (org-element-map (org-element-parse-buffer) 'drawer
                  (lambda (drawer)
                    (when (string= (or drawer-name "LOGBOOK") (org-element-property :drawer-name drawer))
                      drawer)) nil t)
                (progn
                  (forward-line)
                  (org-insert-drawer nil (or drawer-name "LOGBOOK"))
                  (forward-line -1)
                  (org-element-at-point)))))
          (when drawer
            (goto-char (org-element-property :contents-begin drawer))
            (insert txt "\n")))))))

(defun rysco-org-insert-clock-entry (&optional minutes)
  ""
  (interactive)
   (let* ((seconds (* 60 (or minutes 30)))
          (now (org-read-date t t "now"))
          (then (time-subtract now seconds)))
     (rysco-org-insert-into-drawer
      (concat
       "CLOCK: "
       (format-time-string "[%F %a %R]" then)
       "--"
       (format-time-string "[%F %a %R]" now)
       " =>  "
       (format-time-string "%R" seconds t))
      "LOGBOOK")))

(defun rysco-org-agenda-insert-clock-entry (&optional minutes)
  ""
  (interactive)
  (--when-let (org-get-at-bol 'org-hd-marker)
    (with-current-buffer (marker-buffer it)
      (goto-char it)
      (rysco-org-insert-clock-entry minutes))))

(defun rysco-org-agenda-entry-has-note (&optional marker)
  (interactive)
  (-when-let* ((marker (or marker (get-text-property (point) 'org-marker))))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (when-let* ((element (org-element-at-point))
                    (begin (org-element-contents-begin element))
                    (end (org-element-contents-end element)))
          (save-restriction
            (narrow-to-region begin end)

            (let* ((data (org-element-parse-buffer))
                   (parent (org-element-map data '(drawer)
                             (lambda (el)
                               (when (string= (org-element-property :drawer-name el) "LOGBOOK")
                                 el))
                             nil t '(headline))))
              (when parent
                (org-element-map parent org-element-all-elements
                  (lambda (el)
                    (not (member (org-element-type el) '(drawer clock))))
                  nil t)))))))))

(defun rysco-org-agenda-entry-text-show-here ()
  "Add logbook from the entry as context to the current line."
  (let (m txt o)
    (setq m (org-get-at-bol 'org-hd-marker))
    (unless (marker-buffer m)
      (error "No marker points to an entry here"))
    (setq txt
          (concat
           "\n"
           (save-excursion
             (with-current-buffer (marker-buffer m)
               (org-with-wide-buffer
                (goto-char m)
                (-when-let* ((el (org-element-at-point))
                             (begin (org-element-property :contents-begin el))
                             (end (org-element-property :contents-end el))
                             (content (buffer-substring begin end))
                             (log (with-temp-buffer
                                    (insert content)
                                    (org-element-map (org-element-parse-buffer) 'drawer
                                      (lambda (drawer)
                                        (when (string= "LOGBOOK" (org-element-property :drawer-name drawer))
                                          (let ((line-prefix "\t")
                                                entries)
                                            (org-element-map drawer org-element-all-elements
                                              (lambda (el)
                                                (--when-let
                                                    (pcase (car el)
                                                      ('clock)
                                                      ('plain-list
                                                       (let* ((text-begin (org-element-contents-begin el))
                                                              (text-end (org-element-contents-end el))
                                                              (text-entry (buffer-substring
                                                                           text-begin
                                                                           text-end)))

                                                         (unless (s-starts-with? "- State \"DONE\"" text-entry)
                                                           (concat
                                                            line-prefix
                                                            (s-replace
                                                             "\n" (concat "\n" line-prefix)
                                                             text-entry))))))
                                                  (push it entries))))
                                            (apply 'concat (reverse entries)))))
                                      nil t))))
                  log))))))

    (when (string-match "\\S-" txt)
      (setq o (make-overlay (point-at-bol) (point-at-eol)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'org-overlay-type 'agenda-entry-content)
      (overlay-put o 'after-string txt))))

(advice-add 'org-agenda-entry-text-show-here :override 'rysco-org-agenda-entry-text-show-here)

(defun rysco-org-current-effective-time-advice (func &rest rest)
  (if rysco-org-effective-time-override
      rysco-org-effective-time-override
    (apply func rest)))

(advice-add 'org-current-effective-time :around 'rysco-org-current-effective-time-advice)

(defun rysco-org-insert-link (old &rest rest)
  "Invoke `org-download-clipboard', or, failing that, `org-insert-link'"
  (let ((last-file org-download-path-last-file))
    (call-interactively 'org-download-clipboard)

    (when (fboundp 'gui-set-selection)
      (gui-set-selection 'CLIPBOARD ""))

    (when (string= last-file org-download-path-last-file)
      (call-interactively old rest))))

(advice-add 'org-insert-link :around 'rysco-org-insert-link)

(defun rysco-org-duration-string-to-minutes (s)
  (cl-loop
   for mult in (list 1 60 (* 24 60))
   for part in (reverse (s-split ":" s))
   sum (* (string-to-number part) mult)))

(defun rysco-org-todo-on-date (arg)
  (interactive "P")
  (let ((rysco-org-effective-time-override (org-read-date nil t)))
    (pcase major-mode
      ('org-mode (org-todo arg))
      ('org-agenda-mode (org-agenda-todo arg)))))

(defun rysco-org-todo-yesterday--fix-scheduled-date ()
  "Decrement by delta from today to day of completion (which, because it's yesterday, means 1)"
  (let* ((scheduled (org-entry-get (point) "SCHEDULED"))
         (scheduled-time (org-get-scheduled-time (point)))
         (repeat (org-get-repeat scheduled))
         (yesterday (org-read-date nil nil "--1" nil scheduled-time)))
    (org-schedule nil (concat yesterday " " repeat))))

(defun rysco-org-todo-yesterday (arg)
  (interactive "P")
  (let ((rysco-org-effective-time-override (org-read-date nil t "-1")))
    (pcase major-mode
      ('org-mode
       (org-todo arg)
       (rysco-org-todo-yesterday--fix-scheduled-date))
      ('org-agenda-mode
       (let* ((marker (or (org-get-at-bol 'org-marker)
		          (org-agenda-error)))
              (buffer (marker-buffer marker))
	      (pos (marker-position marker)))
         (org-agenda-todo arg)
         (with-current-buffer buffer
           (goto-char pos)
           (rysco-org-todo-yesterday--fix-scheduled-date)))))))

;; Embedded images in HTML export as new backend
;; NOTE: From https://niklasfasching.de/posts/org-html-export-inline-images/
(defun rysco-org-html-export-to-embedded-html (&optional async subtree visible body)
  (cl-letf (((symbol-function 'org-html--format-image) 'rysco-format-image-inline))
    (org-html-export-to-html nil subtree visible body)))

(defun rysco-format-image-inline (source attributes info)
  (let* ((ext (file-name-extension source))
         (prefix (if (string= "svg" ext)
                     "data:image/svg+xml;base64,"
                   "data:;base64,"))
         (data (with-temp-buffer
                 (url-insert-file-contents source)
                 (buffer-string)))
         (data-url (concat prefix (base64-encode-string data)))
         (attributes (org-combine-plists `(:src ,data-url) attributes)))
    (org-html-close-tag "img" (org-html--make-attribute-string attributes) info)))

(with-eval-after-load 'ox
  (org-export-define-derived-backend 'html-inline-images 'html
    :menu-entry
    '(?h "Export to HTML"
         ((?e "As MHTML file" rysco-org-html-export-to-embedded-html)
          (?E "As MHTML file and open"
	    (lambda (a s v b)
	      (if a (rysco-org-html-export-to-embedded-html t s v b)
		(org-open-file (rysco-org-html-export-to-embedded-html nil s v b)))))))))

(defun rysco-org-add-triage-tag ()
  (interactive)
  (org-toggle-tag "triage"))

(defun rysco-org-agenda-add-triage-tag ()
  (interactive)
    (--when-let (org-get-at-bol 'org-marker)
      (with-current-buffer (marker-buffer it)
        (goto-char (marker-position it))
        (rysco-org-add-triage-tag))))

;;
(provide 'rysco-org)
