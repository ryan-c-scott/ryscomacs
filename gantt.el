;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl)

(defvar gantt-max-days 260)
(defvar gantt-max-sprints 26)

(cl-defstruct gantt-project
  ""
  id
  name
  work
  confidence
  dependencies
  blockers
  tags

  ;; Simulation data
  work-remaining
  start-blocker
  started
  ended
  resources
  resource-log)

(cl-defstruct gantt-simulation
  ""
  projects
  resources
  externals
  start-date
  simulation-start
  work-log
  dev-form)

;;;###autoload
(cl-defun gantt-create-palette (size)
  (cl-loop
   for i upfrom 0 to size
   as rotations = (/ i 1.0)

   collect
   (apply
    'color-rgb-to-hex
    `(,@(color-hsl-to-rgb
         (mod
          (+
           (* i 0.3) ;; Primary rotation
           (* rotations 0.01) ;; Increased offset for each full rotation
           )
          1.0)
         (min 0.9 (+ 0.5 (* rotations 0.2)))
         0.6)
      2))))

(cl-defun gantt-date-to-day (start-date date)
  "Convert from real date to day number based on 5 working days per week."
  (let* ((start (org-read-date nil t start-date))
         (date (org-read-date nil t date))
         (days (/ (time-subtract date start) 60 60 24))
         (weeks (/ days 7)))
    (- days (* weeks 2))))

(cl-defun gantt-day-to-date (start-date day)
  "Convert from working day to real date, assuming 5 working days per week."
  (let* ((week (/ day 5))
         (start (org-read-date nil t start-date))
         (irl-offset (* week 2)))

    (org-read-date nil t (format "++%s" (+ day irl-offset)) nil start)))

;;;###autoload
(cl-defun gantt-calculate-sprint-dates (start-date sprints &optional format-string)
  (when start-date
    (cl-loop
     for dev-day from 0 to (* sprints 10) by 10
     collect
     `(,(format-time-string
         (or format-string "%m/%d")
         (gantt-day-to-date start-date dev-day))
       ,dev-day))))

(cl-defun gantt-parse-work-log (path start-date)
  (let ((path-data (pcase path
                     ((rx (* any) (seq ?/ (let dev (+ alpha))) (seq ?/ (let date (+ (any alnum ?-))) ".log"))
                      `(,dev . ,date)))))
    (when path-data
      (with-temp-buffer
        (insert-file-contents-literally path)
        (cl-loop
         with dev = (car path-data)
         with log-date = (cdr path-data)
         with start-day = (gantt-date-to-day start-date log-date)

         for line in (s-split "\n" (buffer-string) t)
         as entry = (car (read-from-string (concat "(" line ")")))
         as day-range = (pcase entry
                          (`(* ,proj)
                           `(0 5 ,proj)))
         append
         (cl-loop
          with range-start = (+ start-day (car day-range))
          with range-end = (+ start-day (cadr day-range))
          with proj = (caddr day-range)

          for day from range-start to range-end collect
          `(,(format "%s" proj) ,dev ,day 1.0)))))))

(cl-defun gantt-work-log-from-directory (dir start-date)
  (when (f-exists? dir)
    (cl-loop
     for log-path in (f-entries dir nil t)
     as data = (gantt-parse-work-log log-path start-date)
     when data append data)))

(cl-defun gantt-generate-resource-log (simulation)
  (let* ((projects (gantt-simulation-projects simulation))
         (combined
          (cl-loop
           for proj in projects
           as id = (gantt-project-id proj)
           append
           (--map
            (cons id it)
            (gantt-project-resource-log proj)))))
    (cl-loop
     for (dev . log) in (-group-by 'cadr combined) collect
     (cons
      dev
      (-group-by 'car log)))))

(cl-defun gantt-transform-effort (effort-data)
  (cl-loop
   with conditional-effort
   with effort = 1.0

   for exp in effort-data
   if (numberp exp) do
   (setq effort exp)

   else collect
   (pcase exp
     (`(after ,date ,val)
      `(when (string> simulation-date ,(format "%s" date))
         (setq effort ,val)))

     (`(before ,date ,val)
      `(progn
         (when (string< simulation-date ,(format "%s" date))
         (setq effort ,val))))

     (`(between ,begin-date ,end-date ,val)
      `(when (and (string> simulation-date ,(format "%s" begin-date))
                  (string< simulation-date ,(format "%s" end-date)))
         (setq effort ,val)))

     (_ 'ERROR))
   into conditional-effort

   finally return
   `(lambda (simulation-date)
      (let ((effort ,effort))
        ,@conditional-effort
        effort))))

(cl-defun gantt-transform-project-dependencies (start-date depencies)
  `(lambda (day project-lookup)
     (and
      ,@(cl-loop
         for dep in depencies collect
         (pcase dep
           ((or (pred symbolp) (pred stringp))
            `(let ((proj (gethash ,(format "%s" dep) project-lookup)))
               (and proj (gantt-project-ended proj))))

           (`(:external ,date ,description)
            `(> day ,(gantt-date-to-day start-date date))))))))

(cl-defun gantt-transform-projects (start-date data)
  (cl-loop
   with projects = (make-hash-table :test 'equal)

   for (id . proj) in data
   as id = (format "%s" id)

   do
   (puthash
    id
    (make-gantt-project
     :id id
     :name (or (plist-get proj :name) id)
     :work (plist-get proj :days)
     :confidence (plist-get proj :confidence)
     :tags (--map (format "%s" it) (plist-get proj :tags))
     ;; :adjustment adjustment
     ;; :dependencies dependencies
     ;; :resources resources
     ;; :blockers blockers
     ;; :actual-started actual-started
     ;; :actual-ended actual-ended
     ;; :user-data rest
     :dependencies (gantt-transform-project-dependencies start-date (plist-get proj :deps))

     :work-remaining (plist-get proj :days)
     :resource-log nil)
    projects)
   finally return projects))

(cl-defun gantt-transform-dev-project (proj)
  (pcase proj
    ((pred symbolp)
     `(,(format "%s" proj)
       :effort (lambda (simulation-date) 1.0)))
    (`(,id . ,proj-data)
     `(,(format "%s" id)
       :effort ,(gantt-transform-effort (plist-get proj-data :effort))))))

(cl-defun gantt-get-tagged-projects (projects query)
  (cl-loop
   for proj in projects
   as tags = (gantt-project-tags proj)
   when (--all? (cl-member it tags :test 'string=) query) collect proj))

(cl-defun gantt-transform-devs (data projects)
  (cl-loop
   for (id . dev) in data
   collect
   `(,(format "%s" id)
     :effort ,(gantt-transform-effort (plist-get dev :effort))

     :dev-work
     ,(cl-loop
       for entry in (plist-get dev :projects) append
       (cl-loop
        with selected-projects = (pcase entry
                                   (`(:tags . ,tagset)
                                    (--map
                                     (gantt-project-id it)
                                     (gantt-get-tagged-projects
                                      (hash-table-values projects)
                                      tagset)))

                                   ((pred symbolp)
                                    `(,(format "%s" entry)))

                                   ;; TODO: This should instead destructure from plist
                                   (`(,id . ,_)
                                    ;; TODO: Other data
                                    `(,(format "%s" id))))

        for id in selected-projects
        collect
        `(lambda (day project-lookup)
           (let ((proj (gethash ,id project-lookup)))
             (and proj
                  (> (gantt-project-work-remaining proj) 0)
                  (funcall (gantt-project-dependencies proj) day project-lookup)
                  proj))))))))

;;;###autoload
(cl-defmacro gantt-derive-dev-form (&key projects devs start-date simulation-date work-log)
  (let* ((transformed-projects (gantt-transform-projects start-date projects)))
    `(let ((start-date ,start-date)
           (simulation-start-day ,(pcase simulation-date
                                    ((pred stringp)
                                     (gantt-date-to-day start-date simulation-date))
                                    (_ 0)))
           (projects ,transformed-projects)
           (devs ',(gantt-transform-devs devs transformed-projects)))

       '(:errors
         ,@(-uniq
            (cl-loop
             for dev in devs append
             (cl-loop
              for proj in (plist-get (cdr dev) :projects)
              as id = (if (listp proj) (car proj) proj)
              unless (assoc id projects) collect
              (format "No project '%s' defined in :PROJECTS section (referenced in dev '%s')" id (car dev))))))

       ,(when work-log
          `(cl-loop
            with max-day
            with min-day

            for (proj-id dev day effort) in ,work-log
            as proj = (gethash proj-id projects)

            ,@(when (eq simulation-date 'latest)
                `(maximize day into max-day
                  minimize day into min-day))

            unless proj do
            (setq proj
                  (puthash
                   proj-id
                   (make-gantt-project
                    :id proj-id
                    :name proj-id
                    :work 0
                    :work-remaining 0)
                   projects))

            do
            (let* ((remaining (gantt-project-work-remaining proj))
                   (new-remaining (max 0 (- remaining effort)))
                   (started (gantt-project-started proj))
                   (ended (gantt-project-ended proj))
                   (resources (gantt-project-resources proj))
                   (resource-log (gantt-project-resource-log proj)))

              (setf (gantt-project-work-remaining proj) new-remaining)
              (setf (gantt-project-resources proj)
                    (-uniq (append resources (list dev))))

              (setf (gantt-project-resource-log proj)
                    (append
                     resource-log
                     (list
                      (list dev day effort))))

              (setf (gantt-project-started proj)
                    (if started
                        (min day started)
                      day))

              (when (= new-remaining 0)
                (setf (gantt-project-ended proj)
                      (if ended
                          (max day ended)
                        day))))

            ,@(when (eq simulation-date 'latest)
                `(finally do (setq simulation-start-day (or max-day 0))))))

       (cl-loop
        for day from simulation-start-day to ,gantt-max-days
        as simulation-date = (format-time-string "%F" (gantt-day-to-date ,start-date day))

        do
        (cl-loop
         for (dev . data) in devs do
         (cl-loop
          ;; TODO: Change this lambda to use day like others
          with effort = (funcall (plist-get data :effort) simulation-date)

          for selector in (plist-get data :dev-work)
          while (> effort 0)

          as proj = (funcall selector day projects)
          when proj do
          (let* (
                 ;; TODO: Project specific effort (min of dev and project forms)
                 (remaining (gantt-project-work-remaining proj))
                 (new-remaining (max 0 (- remaining effort)))
                 (work-done (- remaining new-remaining))
                 (started (gantt-project-started proj))
                 (ended (gantt-project-ended proj))
                 (resources (gantt-project-resources proj))
                 (resource-log (gantt-project-resource-log proj)))

            (when (> work-done 0)
              (setq effort (- effort work-done))
              (setf (gantt-project-work-remaining proj) new-remaining)
              (setf (gantt-project-resources proj)
                    (-uniq (append resources (list dev))))

              (setf (gantt-project-resource-log proj)
                    (append
                     resource-log
                     (list
                      (list dev day work-done))))

              (when (not started)
                (setf (gantt-project-started proj) day))

              (when (and (= new-remaining 0)
                         (not ended))
                (setf (gantt-project-ended proj) day)
                ;; TESTING: No mid-day project change after completion
                (setq effort 0)))))))

       (make-gantt-simulation
        :start-date ,start-date
        :simulation-start simulation-start-day
        :projects
        (--sort
         (let ((it-start (or (gantt-project-started it) 0))
               (it-end (or (gantt-project-ended it) 1000))
               (other-start (or (gantt-project-started other) 0))
               (other-end (or (gantt-project-ended other) 1000)))
           (if (= it-start other-start)
               (< it-end other-end)
             (< it-start other-start)))
         (hash-table-values projects))
        ))))

;;;###autoload
(cl-defun gantt-simulation-to-table (simulation)
  (cl-loop
   for proj in (gantt-simulation-projects simulation)
   as start = (floor (or (gantt-project-started proj) 0))
   as end = (ceiling (or (gantt-project-ended proj) start))
   as resources = (gantt-project-resources proj)

   collect
   `(,(gantt-project-name proj)
     ,(s-join " " resources)
     ,(concat
       (make-string start ?\_)
       (make-string (- end start) ?#)))))

;;;###autoload
(cl-defun gantt-simulation-to-plot (simulation &rest options)
  (let* ((simulation-start (gantt-simulation-simulation-start simulation))
         (projects (gantt-simulation-projects simulation))
         (height (1+ (length projects)))
         (scale (or (plist-get options :fontscale) 1.0))
         blockers
         fails)
    (apply
     'rysco-plot
     `((:unset key)
       (:data gantt ,@(cl-loop
                       for i upfrom 1
                       for proj in projects
                       as entry = (pcase-let (((cl-struct gantt-project id name started ended resources start-blocker) proj))
                                    (when start-blocker
                                      (push `(0 ,i ,(cdr start-blocker) 0 ,(format "[{/:Bold %s}]" (car start-blocker))) blockers))
                                    (unless (or started ended)
                                      (push `(1 ,i ,name) fails))
                                    `(,started ,i ,(- (or ended gantt-max-days) (or started 0)) 0 ,id ,(format "%s: %s" name resources)))
                       when entry collect entry))

       (:data blockers ,@blockers)
       (:data fails ,@fails)

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw ,(* scale 3) lc "#Eedd82") ;Period boundaries
       (:set style arrow 2 nohead lw ,(* scale 20) lc "#8deeee") ;Projects
       (:set style arrow 3 nohead lw ,(* scale 6) lc "#8b008b") ;Simulation boundary

       (:set arrow 1 from (60 0) to (60 ,height) as 1)
       (:set arrow 2 from (,simulation-start 0) to (,simulation-start ,height) as 3)

       (:set yrange [,height 0])
       (:set grid x y)

       (:set ytics
             :out
             :offset (0.25 0.25)
             :font ",28"
             :textcolor "white")

       (:set lmargin ,(*
                       1.8 ;; HACK: Magic number to create space for the larger ytics
                       scale
                       (cl-loop
                        for proj in projects maximize
                        (pcase-let (((cl-struct gantt-project id name resources) proj))
                          (length (format "%s: %s" name resources))))))

       (:tics x
              :options (:out
                        :font ",25"
                        :rotate by 45 right
                        :textcolor "white")
              :data
              ,(gantt-calculate-sprint-dates (gantt-simulation-start-date simulation) gantt-max-sprints))

       (:set rmargin ,(* 5 scale))
       (:set bmargin ,(* 5 scale))

       (:plot [0 *]
              (:vectors :data gantt :using [1 2 3 4 (ytic 6)] :options (:arrowstyle 2))
              (:vectors :data blockers :using [1 2 3 4] :options (:arrowstyle 3))
              ;; (:labels :data blockers :using [1 2 5] :options (:left :font ",25" :tc "#Cfcfcf" :front))
              (:labels :data fails :using [1 2 3] :options (:left :offset (0.25 0.25) :font ",25" :tc "#Cf0000" :front))))
     options)))

;;;###autoload
(cl-defun gantt-simulation-to-resource-log-plot (simulation &rest options)
  (let* ((data (gantt-generate-resource-log simulation))
         (simulation-start (gantt-simulation-simulation-start simulation))
         (projects (gantt-simulation-projects simulation))
         (height (1+ (length data)))
         (scale (or (plist-get options :fontscale) 1.0))
         labels
         project-count)
    (apply
     'rysco-plot
     `((:unset key)
       (:data
        worklog
        ,@(cl-loop
           with project-colors = (make-hash-table :test 'equal)
           with next-style-id = 3

           for i upfrom 1
           for (dev . proj-log) in data append
           (cl-loop
            for (proj . entries) in proj-log

            as style-id = (gethash proj project-colors)
            unless style-id do
            (setq style-id (puthash proj next-style-id project-colors)
                  next-style-id (1+ next-style-id))

            append
            (cl-loop
             for j upfrom 0
             for (_ _ day effort) in (--sort
                                      (< (nth 2 it)
                                         (nth 2 other))
                                      entries) collect
             (progn
               (when (= j 0)
                 (push `(,day ,i ,proj) labels))
               `(,proj ,dev ,day ,i ,(or 1 effort) 0 ,style-id)))

            finally do (setq project-count (hash-table-count project-colors)))))

       (:data labels ,@labels)

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw ,(* 3 scale) lc "#Eedd82") ;Period boundaries
       (:set style arrow 2 nohead lw ,(* 6 scale) lc "#8b008b") ;Simulation boundary

       ,@(cl-loop
          for i upfrom 0
          for color in (gantt-create-palette project-count)
          collect
          `(:set style arrow ,(+ i 3) nohead lw ,(* 30 scale) lc ,color))

       (:set arrow 1 from (60 0) to (60 ,height) as 1)
       (:set arrow 2 from (,simulation-start 0) to (,simulation-start ,height) as 2)

       (:set yrange [,height 0])
       (:set grid x y)

       (:set ytics
             :out
             :font ",28"
             :textcolor "white")

       (:tics x
              :options (:out
                        :font ",25"
                        :rotate by 45 right
                        :textcolor "white")
              :data
              ,(gantt-calculate-sprint-dates (gantt-simulation-start-date simulation) gantt-max-sprints))

       (:set lmargin ,(*
                       scale
                       (cl-loop
                        for proj in projects maximize
                        (pcase-let (((cl-struct gantt-project id name resources) proj))
                          (length (format "%s: %s" name resources))))))
       (:set rmargin ,(* 5 scale))
       (:set bmargin ,(* 5 scale))

       (:plot [0 *]
              (:vectors :data worklog :using [3 4 5 6 7 (ytic 2)] :options (:arrowstyle variable))
              (:labels :data labels :using [1 2 3] :options (:left :offset (0.25 0.25) :font ",25" :tc "#0f0f0f" :front)))
       )
     options)))

;;;;
(provide 'gantt)
