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
  estimate
  confidence
  dependencies
  blockers
  tags
  user-data

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
(cl-defun gantt-create-palette (keys &optional start-id)
  (cl-loop
   for i upfrom 0
   for k in (sort keys 'string<)
   as rotations = (/ i 1.0)

   collect
   `(,k
     :id ,(+ i (or start-id 0))
     :color ,(apply
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
                2)))))

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

(cl-defun gantt-calculate-date-by-interval (start-date interval count &optional format-string)
  (when start-date
    (cl-loop
     for dev-day from 0 to (* count interval) by interval
     collect
     `(,(format-time-string
         (or format-string "%m/%d")
         (gantt-day-to-date start-date dev-day))
       ,dev-day))))

;;;###autoload
(cl-defun gantt-calculate-sprint-dates (start-date sprints &optional format-string)
  (gantt-calculate-date-by-interval start-date 10 sprints format-string))

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
         as entry = (car (read-from-string (concat "(\n" line "\n)")))

         as log-out = (when entry
                        (pcase entry
                          ;; Special entries first
                          ;; Anything else is considered a work log entry
                          ((or
                            `(,(and (pred numberp) day (guard (and (>= day 0) (<= day 4))))
                              close
                              ,proj)
                            `(* close ,proj))
                           `((,(format "%s" proj) ,dev ,(+ start-day (or day 4)) close)))
                          ((or
                            ;; TODO: Find better solution to having to explicitly specify permutations
                            ;; Full entry
                            `(,(and (pred numberp) start (guard (and (>= start 0) (<= start 4))))
                              ,(and (pred numberp) end (guard (and (>= end 0) (<= end 4))))
                              ,proj
                              ,(and (pred numberp) effort))

                            ;; Effort omitted
                            `(,(and (pred numberp) start (guard (and (>= start 0) (<= start 4))))
                              ,(and (pred numberp) end (guard (and (>= end 0) (<= end 4))))
                              ,proj)

                            ;; Special range symbol
                            `(* ,proj ,(and (pred numberp) effort))
                            `(* ,proj))

                           (cl-loop
                            with range-start = (+ start-day (or start 0))
                            with range-end = (+ start-day (or end 4))

                            for day from range-start to range-end collect
                            `(,(format "%s" proj) ,dev ,day ,(or effort 1.0))))

                          (_ (error "Malformed work log entry in %s: %s" path line))))

         when log-out append log-out)))))

(cl-defun gantt-work-log-from-directory (dir start-date)
  (when (f-exists? dir)
    (cl-loop
     for log-path in (f-entries dir nil t)
     as data = (gantt-parse-work-log log-path start-date)
     when data append data)))

(cl-defun gantt-read-forms-from-file (path)
  ;; TODO: Support files not wrapped into a single form/list
  (with-temp-buffer
    (insert-file-contents-literally path)
    (read (current-buffer))))

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
     for (dev . log) in (--sort (string< (car it) (car other)) (-group-by 'cadr combined)) collect
     (cons
      dev
      (-group-by 'car log)))))

(cl-defun gantt-transform-effort (start-date effort-data default-effort)
  (if (numberp effort-data)
      `(lambda (simulation-date) ,effort-data)

    (cl-loop
     with conditional-effort
     with effort = default-effort

     for exp in effort-data
     if (numberp exp) do
     (progn
       (setq effort exp))

     else collect
     (pcase exp
       (`(after ,date ,val)
        `(when (>= simulation-date ,(gantt-date-to-day start-date (format "%s" date)))
           (setq effort ',val)))

       (`(before ,date ',val)
        `(progn
           (when (< simulation-date ,(gantt-date-to-day start-date (format "%s" date)))
             (setq effort ',val))))

       (`(between ,begin-date ,end-date ,val)
        `(when (and (>= simulation-date ,(gantt-date-to-day start-date (format "%s" begin-date)))
                    (<= simulation-date ,(gantt-date-to-day start-date (format "%s" end-date))))
           (setq effort ',val)))

       (_ 'ERROR))
     into conditional-effort

     finally return
     `(lambda (simulation-date)
        (or
         ,@conditional-effort
         ,effort)))))

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

(cl-defun gantt-calculate-duration (data)
  (+
   (* (or (plist-get data :months) 0) 20)
   (* (or (plist-get data :sprints) 0) 10)
   (* (or (plist-get data :weeks) 0) 5)
   (or (plist-get data :days) 0)))

(cl-defun gantt-transform-projects (start-date data)
  (cl-loop
   with projects = (make-hash-table :test 'equal)

   for (id . proj) in data
   as id = (format "%s" id)
   as dev-days = (gantt-calculate-duration proj)
   as estimate = (plist-get proj :estimate)
   as estimate-days = (when estimate (gantt-calculate-duration estimate))

   do
   (puthash
    id
    (make-gantt-project
     :id id
     :name (or (plist-get proj :name) id)
     :work dev-days
     :estimate estimate-days
     :confidence (plist-get proj :confidence)
     :tags (--map (format "%s" it) (plist-get proj :tags))
     :user-data (plist-get proj :user-data)
     :dependencies (gantt-transform-project-dependencies start-date (plist-get proj :deps))

     :work-remaining dev-days
     :resource-log nil)
    projects)
   finally return projects))

(cl-defun gantt-get-tagged-projects (projects query)
  (cl-loop
   for proj in projects
   as tags = (gantt-project-tags proj)
   when (--all? (cl-member it tags :test 'string=) query) collect proj))

(cl-defun gantt-transform-devs (start-date data projects)
  (cl-loop
   for (id . dev) in data
   collect
   `(,(format "%s" id)
     :effort ,(gantt-transform-effort start-date (plist-get dev :effort) 1.0)

     :dev-work
     ,(cl-loop
       for entry in (plist-get dev :projects) append
       (cl-loop
        with selected-projects = (pcase entry
                                   (`(:tags . ,tagset)
                                    (--map
                                     (list (gantt-project-id it))
                                     (gantt-get-tagged-projects
                                      (hash-table-values projects)
                                      tagset)))

                                   ((pred symbolp)
                                    `((,(format "%s" entry))))

                                   ;; TODO: This should instead destructure from plist
                                   (`(,id . ,proj-data)
                                    `((,(format "%s" id)
                                       ,(--when-let (plist-get proj-data :effort)
                                          (gantt-transform-effort start-date it 1.0))))))

        for (id effort-form) in selected-projects
        collect
        `(lambda (dev day default-effort project-lookup)
           (let ((proj (gethash ,id project-lookup))
                 (proj-effort ,(if effort-form
                                   `(funcall ,effort-form day)
                                 'default-effort)))
             (and proj
                  (> (gantt-project-work-remaining proj) 0)
                  (> proj-effort 0)
                  (funcall (gantt-project-dependencies proj) day project-lookup)

                  ;; Only contribute once per day
                  (not
                   (--any? (and (eq (car it) dev)
                                (eq day (nth 2 it)))
                           (gantt-project-resource-log proj)))

                  (cons proj proj-effort)))))))))

;;;###autoload
(cl-defmacro gantt-derive-dev-form (&key projects devs start-date simulation-date work-log global-effort)
  (let* ((projects (if (stringp projects)
                       (gantt-read-forms-from-file projects)
                     projects))
         (devs (if (stringp devs)
                   (gantt-read-forms-from-file devs)
                 devs))
         (global-effort (if (stringp global-effort)
                            (gantt-read-forms-from-file global-effort)
                          global-effort))
         (transformed-projects (gantt-transform-projects start-date projects)))

    `(let ((start-date ,start-date)
           (simulation-start-day ,(pcase simulation-date
                                    ((pred stringp)
                                     (gantt-date-to-day start-date simulation-date))
                                    (_ 0)))
           (projects ,transformed-projects)
           (devs ',(gantt-transform-devs start-date devs transformed-projects))
           (global-effort ,(gantt-transform-effort start-date global-effort nil)))

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

            for (proj-id dev day effort) in (--sort (< (nth 2 it) (nth 2 other)) ,work-log)
            as proj = (gethash proj-id projects)
            as closing-proj = (eq effort 'close)
            as effort = (pcase effort
                          ('close (gantt-project-work-remaining proj))
                          (_ effort))

            ,@(unless (eq simulation-date 'latest)
                `(while (< day simulation-start-day)))

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

              (unless closing-proj
                (setf (gantt-project-resource-log proj)
                      (append
                       resource-log
                       (list
                        (list dev day effort)))))

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
                `(finally do (setq simulation-start-day (or (and max-day (1+ max-day)) 0))))))

       (cl-loop
        for day from simulation-start-day to ,gantt-max-days
        as simulation-date = (format-time-string "%F" (gantt-day-to-date ,start-date day))

        do
        (cl-loop
         for (dev . data) in devs
         as default-effort = (or (funcall global-effort day)
                                 (funcall (plist-get data :effort) day))

         unless (eq default-effort 'PTO) do
         (cl-loop
          with daily-effort = 0 ;; Can never exceed a single day worth of time
          with proj
          with proj-effort

          for selector in (plist-get data :dev-work)
          as proj-data = (funcall selector dev day default-effort projects)

          when proj-data
          do (setq
              proj (car proj-data)
              proj-effort (cdr proj-data))

          while (and (< daily-effort 1.0)
                     (< daily-effort (or proj-effort default-effort)))

          when proj do
          (let* ((remaining (gantt-project-work-remaining proj))
                 (new-remaining (max 0 (- remaining proj-effort)))
                 (work-done (- remaining new-remaining))
                 (started (gantt-project-started proj))
                 (ended (gantt-project-ended proj))
                 (resources (gantt-project-resources proj))
                 (resource-log (gantt-project-resource-log proj)))

            (when (> work-done 0)
              (setq daily-effort (+ daily-effort work-done))
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
                (setq daily-effort 1.0)))))))

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
         (hash-table-values projects))))))

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
(defmacro with-gantt-simulation-projects (simulation &rest forms)
  `(cl-loop
    with start-date = (gantt-simulation-start-date simulation)
    with simulation-start = (gantt-simulation-simulation-start simulation)
    with projects = (gantt-simulation-projects simulation)
    for proj in (--sort
                 (string< (gantt-project-name it) (gantt-project-name other))
                 projects)

    as out =
    (pcase-let* (((cl-struct gantt-project id name started ended work estimate resources resource-log start-blocker user-data) proj)
                 (resources-string (s-join " " resources))
                 (started-string (when started
                                   (format-time-string
                                    "%F"
                                    (gantt-day-to-date start-date (floor started)))))

                 (ended-string (when ended
                                 (format-time-string
                                  "%F"
                                  (gantt-day-to-date start-date (ceiling ended))))))

      (progn ,@forms))

    when out collect out))

;;;###autoload
(cl-defun gantt-simulation-to-completion-table (simulation)
  (with-gantt-simulation-projects
   simulation
   name
   resources
   start
   end))

;;;###autoload
(cl-defun gantt-simulation-to-plot (simulation &rest options)
  (let* ((simulation-start (gantt-simulation-simulation-start simulation))
         (projects (gantt-simulation-projects simulation))
         (palette (gantt-create-palette (--map (gantt-project-name it) projects) 4))
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

                       as entry = (pcase-let* (((cl-struct gantt-project id name started ended resources resource-log start-blocker) proj)
                                               (last-day (or ended
                                                             (--reduce-from (max acc (nth 1 it))
                                                                            (or started 0)
                                                                            resource-log)))
                                               (style-data (cdr (assoc name palette)))
                                               (style-id (plist-get style-data :id)))

                                    (when start-blocker
                                      (push `(0 ,i ,(cdr start-blocker) 0 ,(format "[{/:Bold %s}]" (car start-blocker))) blockers))
                                    (unless ended
                                      (push `(,(1+ (or last-day 0)) ,i ,name) fails))
                                    (when last-day
                                      `(,started ,i
                                                 ,(1+ (- last-day (or started 0)))
                                                 0 ,id ,name ,style-id)))
                       when entry collect entry))

       (:data blockers ,@blockers)
       (:data fails ,@fails)

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw ,(* scale 3) lc "#Eedd82") ;Period boundaries
       (:set style arrow 2 nohead lw ,(* scale 20) lc "#8deeee") ;Projects
       (:set style arrow 3 nohead lw ,(* scale 6) lc "#8b008b") ;Simulation boundary

       ,@(cl-loop
          for (_ . style-data) in palette collect
          `(:set style
                 arrow ,(plist-get style-data :id)
                 nohead
                 lw ,(* 22 scale)
                 lc ,(plist-get style-data :color)))

       ;; TODO: Use actual end date
       (:set arrow 1 from (70 0) to (70 ,height) as 1)
       (:set arrow 2 from (,simulation-start 0) to (,simulation-start ,height) as 3)

       (:set yrange [,height 0])
       (:set grid x y)

       (:set ytics
             :out
             :offset (0.25 0.25)
             :font ",28"
             :textcolor "white")

       (:set lmargin ,(*
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
              (:vectors :data gantt :using [1 2 3 4 7 (ytic 6)] :options (:arrowstyle variable))
              (:vectors :data blockers :using [1 2 3 4] :options (:arrowstyle 3))
              ;; (:labels :data blockers :using [1 2 5] :options (:left :font ",25" :tc "#Cfcfcf" :front))
              (:labels :data fails :using [1 2 3] :options (:left :offset (0.25 0.25) :font ",25" :tc "#Cf0000" :front))))
     options)))

;;;###autoload
(cl-defun gantt-simulation-to-resource-log-plot (simulation &rest options)
  (let* ((data (gantt-generate-resource-log simulation))
         (simulation-start (gantt-simulation-simulation-start simulation))
         (projects (gantt-simulation-projects simulation))
         (palette (gantt-create-palette (--map (gantt-project-name it) projects) 3))
         (height (1+ (length data)))
         (scale (or (plist-get options :fontscale) 1.0))
         labels)
    (apply
     'rysco-plot
     `((:unset key)
       (:data
        worklog
        ,@(cl-loop
           for i upfrom 1
           for (dev . proj-log) in data append
           (cl-loop
            for (proj . entries) in proj-log
            as style-data = (cdr (assoc proj palette))
            as style-id = (plist-get style-data :id)

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
               `(,proj ,dev ,day ,i ,(or 1 effort) 0 ,style-id))))))

       (:data labels ,@labels)

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw ,(* 3 scale) lc "#Eedd82") ;Period boundaries
       (:set style arrow 2 nohead lw ,(* 6 scale) lc "#8b008b") ;Simulation boundary

       ,@(cl-loop
          for (_ . style-data) in palette collect
          `(:set style
                 arrow ,(plist-get style-data :id)
                 nohead
                 lw ,(* 30 scale)
                 lc ,(plist-get style-data :color)))

       ;; TODO: Use actual end date
       (:set arrow 1 from (70 0) to (70 ,height) as 1)
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
