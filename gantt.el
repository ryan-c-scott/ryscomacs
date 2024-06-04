;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl)
(require 'org)

(defvar gantt-max-days 260)
(defvar gantt-max-sprints 26)
(defvar gantt-min-effort-threshold 0.1)

(defvar gantt-plot-with-descriptions nil)
(defvar gantt-output-date-format "%F")
(defvar gantt-project-status-symbols
  '((alpha . "\\U+03B1")
    (beta . "\\U+03B2")
    (gamma . "\\U+03B3")
    (delta . "\\U+0394")
    (omega . "\\U+03A9")
    (closed . "")))

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
  type
  user-data
  description

  ;; Simulation data
  work-remaining
  start-blocker
  started
  ended
  shipped
  resources
  resource-log
  status-log
  current-status)

(cl-defstruct gantt-simulation
  ""
  projects
  resources
  externals
  start-date
  simulation-start
  view-start-day
  view-end-day
  work-log
  dev-form
  key-dates)

;;;###autoload
(cl-defun gantt-create-palette (keys &optional start-id)
  (cl-loop
   with sep = 0.3
   for i upfrom 0
   for k in (sort keys 'string<)
   as rotations = (floor (* i sep))
   as hash = (md5 (format "%s" k))

   collect
   `(,k
     :id ,(+ i (or start-id 0))
     :color ,(apply
              'color-rgb-to-hex
              `(,@(color-hsl-to-rgb
                   (mod
                    (+
                     (* i sep) ;; Primary rotation
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

(cl-defun gantt-day-to-date (start-date day &optional time-format)
  "Convert from working day to real date, assuming 5 working days per week.
Optional TIME-FORMAT will return the result of the date sent to `format-time-string'"
  (let* ((week (/ day 5))
         (start (org-read-date nil t start-date))
         (irl-offset (* week 2))
         (relative-day (+ day irl-offset))
         (date (org-read-date
                nil t
                (format "%s%s"
                        (if (>= relative-day 0)
                            "++"
                          "--")
                        (abs relative-day))
                nil start)))
    (if time-format
        (format-time-string time-format date)
      date)))

(cl-defun gantt-day-to-end-of-quarter-day (start-date day)
  (let* ((date (format-time-string "%F" (gantt-day-to-date start-date day)))
         (q (1+ (/ (cl-parse-integer (substring date 5 7)) 3)))
         (year (cl-parse-integer (substring date 0 4))))
    (gantt-date-to-day start-date (format "%s-%02d-01" year (1+ (* q 3))))))

(cl-defun gantt-calculate-date-by-interval (start-date interval count &optional format-string)
  (when start-date
    (cl-loop
     for dev-day from 0 below (* count interval) by interval
     collect
     `(,(format-time-string
         (or format-string "%m/%d")
         (gantt-day-to-date start-date dev-day))
       ,dev-day))))

(cl-defun gantt-scrub-description (description)
  (s-replace-regexp "[\n|]" " " description))

;;;###autoload
(cl-defun gantt-calculate-sprint-dates (start-date sprints &optional format-string)
  (gantt-calculate-date-by-interval start-date 10 sprints format-string))

(cl-defun gantt-parse-work-log (path start-date)
  ;; TODO: Make this docstring follow Emacs best practices with regards to the data format listing.
  "The date of entries is ascertained from `path' as 'YYYY-MM-DD.log'.
Entries are in the form
  DEV DATA

Where 'DATA' can be:
  DAY close PROJ
  * close PROJ
  START END PROJ EFFORT
  START END PROJ
  * PROJ EFFORT
  * PROJ"
  (let ((path-data (pcase path
                     ((rx (* any) (seq ?/ (let date (+ (any alnum ?-))) ".log"))
                      date))))
    (when path-data
      (with-temp-buffer
        (insert-file-contents-literally path)
        (cl-loop
         with log-date = path-data
         with start-day = (gantt-date-to-day start-date log-date)

         for line in (s-split "\n" (buffer-string) t)
         as entry = (car (read-from-string (concat "(\n" line "\n)")))

         as log-out = (when entry
                        (let ((dev (format "%s" (car entry)))
                              (entry-data (cdr entry)))
                          (pcase entry-data
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

                            (_ (error "Malformed work log entry in %s: %s" path line)))))

         when log-out append log-out)))))

(cl-defun gantt-work-log-from-directory (dir start-date)
  (when (f-exists? dir)
    (cl-loop
     for log-path in (f-entries dir nil t)
     as data = (gantt-parse-work-log log-path start-date)
     when data append data)))

(cl-defun gantt-work-log-entry-equal (a b)
  "Compares two work log entries by their first three elements"
  (and
   (equal (car a) (car b))
   (equal (cadr a) (cadr b))
   (equal (caddr a) (caddr b))))

(cl-defun gantt-work-log-from-files (&rest files)
  "Reads all files listed in FILES as work logs, combines them, and de-duplicates
using the logic in `gantt-work-log-entry-equal'"
  (let ((-compare-fn 'gantt-work-log-entry-equal))
    (-uniq
     (cl-loop
      for path in files append
      (gantt-read-forms-from-file path)))))

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
  "Transforms an effort form.
The first rule that passes is used, so in the case of overlapping time periods,
they should be listed in their order of precedence and not date."
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
       (`(on ,date ,val)
        `(when (= simulation-date ,(gantt-date-to-day start-date (format "%s" date)))
           ',val))

       (`(after ,date ,val)
        `(when (>= simulation-date ,(gantt-date-to-day start-date (format "%s" date)))
           ',val))

       (`(before ,date ,val)
        `(when (< simulation-date ,(gantt-date-to-day start-date (format "%s" date)))
           ',val))

       (`(between ,begin-date ,end-date ,val)
        `(when (and (>= simulation-date ,(gantt-date-to-day start-date (format "%s" begin-date)))
                    (<= simulation-date ,(gantt-date-to-day start-date (format "%s" end-date))))
           ',val))

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
               (if proj
                   (gantt-project-ended proj)
                 t)))

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
     :description (plist-get proj :description)

     :work-remaining dev-days
     :resource-log nil)
    projects)
   finally return projects))

(cl-defun gantt-get-tagged-projects (projects query)
  (cl-loop
   for proj in projects
   as tags = (gantt-project-tags proj)
   when (--all? (cl-member it tags :test 'string=) query) collect proj))

(cl-defmacro gantt-filter-projects (simulation &rest forms)
  `(--filter
    (and ,@forms)
    (gantt-simulation-projects ,simulation)))

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
(cl-defmacro gantt-derive-dev-form (&key projects devs start-date simulation-days simulation-date work-log global-effort view-start-date view-end-date key-dates post-project)
  (let* ((projects (if (stringp projects)
                       (gantt-read-forms-from-file projects)
                     (eval projects)))
         (view-start-day (--when-let view-start-date (gantt-date-to-day start-date it)))
         (view-end-day (--when-let view-end-date (gantt-date-to-day start-date it)))
         (devs (if (stringp devs)
                   (gantt-read-forms-from-file devs)
                 (eval devs)))
         (global-effort (if (stringp global-effort)
                            (gantt-read-forms-from-file global-effort)
                          (eval global-effort)))
         (transformed-projects (gantt-transform-projects start-date projects)))

    `(let ((start-date ,start-date)
           (simulation-days ,(or simulation-days gantt-max-days))
           (simulation-start-day ,(pcase simulation-date
                                    ((pred stringp)
                                     (gantt-date-to-day start-date simulation-date))
                                    ('monday
                                     (gantt-date-to-day
                                      start-date
                                      (org-read-date nil nil "++Mon" nil (org-read-date nil t "-1w"))))
                                    (_ 0)))
           (projects ,transformed-projects)
           (devs ',(gantt-transform-devs start-date devs transformed-projects))
           (global-effort ,(gantt-transform-effort start-date global-effort nil))
           (key-dates ',(when key-dates (gantt-read-forms-from-file key-dates))))

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
            as effort-override = (funcall global-effort day)
            as effort = (pcase effort
                          ;; NOTE: Deprecated; use (:status closed)
                          ('close (gantt-project-work-remaining proj))
                          (_ effort))
            as status = (pcase effort
                          (`(:status closed)
                           (setq effort (gantt-project-work-remaining proj))
                           'closed)
                          (`(:status ,status)
                           status))

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
                   (new-remaining (max 0 (- remaining (if (numberp effort) effort 0))))
                   (started (gantt-project-started proj))
                   (ended (gantt-project-ended proj))
                   (resources (gantt-project-resources proj))
                   (resource-log (gantt-project-resource-log proj))
                   (status-log (gantt-project-status-log proj)))

              (setf (gantt-project-work-remaining proj) new-remaining)

              (unless (or status effort-override)
                (setf (gantt-project-resources proj)
                      (-uniq (append resources (list dev)))))

              (unless (or status effort-override)
                (setf (gantt-project-resource-log proj)
                      (append
                       resource-log
                       (list
                        (list dev day effort)))))

              (when status
                (setf (gantt-project-current-status proj) status)
                (setf (gantt-project-status-log proj)
                      (append
                       status-log
                       (list
                        (list day status)))))

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

       ;; Special global effort projects (holidays, etc.)
       ,(when global-effort
          `(cl-loop
            for day from 0 to simulation-days
            as effort-id = (funcall global-effort day)
            as specialp = (and effort-id (not (numberp effort-id)))
            as proj = (gethash effort-id projects)

            when (and specialp (not proj)) do
            (setq proj
                  (puthash
                   effort-id
                   (make-gantt-project
                    :id effort-id
                    :name effort-id
                    :type 'global-events
                    :work 0
                    :work-remaining 0)
                   projects))

            ;; Make appropriate work entries for all devs into project
            when specialp do
            (cl-loop
             for (dev . data) in devs
             as resource-log = (gantt-project-resource-log proj)
             do
             (setf (gantt-project-resource-log proj)
                   (append
                    resource-log
                    (list
                     (list dev day effort-id)))))))

       ;; Main simulation
       (cl-loop
        for day from 0 to simulation-days
        as simulation-date = (format-time-string "%F" (gantt-day-to-date ,start-date day))

        do
        (cl-loop
         for (dev . data) in devs
         as default-effort = (or (funcall global-effort day)
                                 (funcall (plist-get data :effort) day))
         as special-entry = (not (numberp default-effort))

         when special-entry do
         (let* ((proj (gethash default-effort projects))
                (proj (or proj
                          (puthash
                           default-effort
                           (make-gantt-project
                            :id default-effort
                            :name default-effort
                            :type 'dev-events
                            :work 0
                            :work-remaining 0)
                           projects)))
                (resource-log (gantt-project-resource-log proj)))
           (setf (gantt-project-resource-log proj)
                 (append
                  resource-log
                  (list
                   (list dev day default-effort)))))

         when (and (>= day simulation-start-day) (not special-entry)) do
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

          while (and (> (- 1.0 daily-effort) ,gantt-min-effort-threshold)
                     (> (- (or proj-effort default-effort) daily-effort) ,gantt-min-effort-threshold))

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

       ,(when post-project
          `(cl-loop
            for proj in (hash-table-values projects) do
            (pcase-let* (((cl-struct gantt-project id name started ended work estimate resources resource-log start-blocker type user-data description) proj)
                         (start-date ,start-date))
              ,post-project)))

       (make-gantt-simulation
        :start-date ,start-date
        :simulation-start simulation-start-day
        :view-start-day ,view-start-day
        :view-end-day ,view-end-day
        :key-dates key-dates
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
    with view-start-day = (gantt-simulation-view-start-day simulation)
    with view-end-day = (gantt-simulation-view-end-day simulation)
    for proj in (--sort
                 (string< (gantt-project-name it) (gantt-project-name other))
                 projects)

    as out =
    (pcase-let* (((cl-struct gantt-project id name started ended shipped work estimate resources resource-log status-log current-status start-blocker type user-data description) proj)
                 (resources-string (s-join " " (--remove (eq it 'SYSTEM) resources)))
                 (started-string (when started
                                   (format-time-string
                                    gantt-output-date-format
                                    (gantt-day-to-date start-date (floor started)))))

                 (ended-string (when ended
                                 (format-time-string
                                  gantt-output-date-format
                                  (gantt-day-to-date start-date (ceiling ended)))))
                 (shipped-string (when shipped
                                   (format-time-string
                                    gantt-output-date-format
                                    (gantt-day-to-date start-date (ceiling shipped)))))
                 (description-safe (when description
                                     (gantt-scrub-description description))))

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
         (start-date (gantt-simulation-start-date simulation))
         (all-projects (gantt-simulation-projects simulation))
         (projects (gantt-filter-projects
                    simulation
                    (not (gantt-project-type it))))
         (view-start (or (gantt-simulation-view-start-day simulation) 0))
         (view-end (or (gantt-simulation-view-end-day simulation) '*))
         (key-dates (gantt-simulation-key-dates simulation))
         (palette (gantt-create-palette (--map (gantt-project-name it) all-projects) 5))
         (height 1)
         (scale (or (plist-get options :fontscale) 1.0))
         blockers
         fails
         shipping
         statuses)
    (apply
     'rysco-plot
     `((:unset key)
       (:data gantt ,@(cl-loop
                       for proj in projects

                       as entry = (pcase-let* (((cl-struct gantt-project id name started ended shipped resources resource-log status-log start-blocker description) proj)
                                               (last-day (or ended
                                                             (--reduce-from (max acc (nth 1 it))
                                                                            (or started 0)
                                                                            resource-log)))
                                               (style-data (cdr (assoc name palette)))
                                               (style-id (plist-get style-data :id)))

                                    (when status-log
                                      (cl-loop
                                       for (day status) in status-log do
                                       (push `(,day
                                               ,height
                                               ,(--if-let (cdr (assoc status gantt-project-status-symbols))
                                                    it
                                                  (format "%s" status)))
                                             statuses)))
                                    (when start-blocker
                                      (push `(0 ,height ,(cdr start-blocker) 0 ,(format "[{/:Bold %s}]" (car start-blocker))) blockers))
                                    (unless ended
                                      (push `(,(1+ (or last-day 0)) ,height
                                              ,(if gantt-plot-with-descriptions
                                                      description
                                                 name))
                                            fails))
                                    (when (and last-day (>= last-day view-start))
                                      (when shipped
                                        (push
                                         `(,(1+ last-day) ,height
                                           ,(max 0 (- shipped last-day 1))
                                           0)
                                         shipping))

                                      `(,started ,height
                                                 ,(1+ (- last-day (or started 0)))
                                                 0 ,id
                                                 ,(if gantt-plot-with-descriptions
                                                      description
                                                    name)
                                                 ,style-id)))
                       when entry collect entry
                       when entry do (cl-incf height)))

       (:data blockers ,@blockers)
       (:data fails ,@fails)
       (:data shipping ,@shipping)
       (:data statuses ,@statuses)

       ,(when key-dates
          `(:data
            keydates
            ,@(cl-loop
               for (date label) in key-dates
               as day = (gantt-date-to-day start-date date)
               collect
               `(,date ,label ,day 0 0 ,height))))

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw ,(* 2 scale) lc "#999999") ;Key dates
       (:set style arrow 2 nohead lw ,(* scale 20) lc "#8deeee") ;Projects
       (:set style arrow 3 nohead lw ,(* scale 6) lc "#8b008b") ;Simulation boundary
       (:set style arrow 4 filled size (1 90 0) lw ,(* 2 scale) lc "#999999") ;Ship dates

       ,@(cl-loop
          for (_ . style-data) in palette collect
          `(:set style
                 arrow ,(plist-get style-data :id)
                 nohead
                 lw ,(* 22 scale)
                 lc ,(plist-get style-data :color)))

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
                        :rotate by 20 right
                        :textcolor "white")
              :data
              ,(gantt-calculate-sprint-dates (gantt-simulation-start-date simulation) gantt-max-sprints))

       (:set x2tics
             nomirror
             :out
             :font ",20"
             :textcolor "#999999")

       (:set rmargin ,(* 5 scale))
       (:set bmargin ,(* 5 scale))

       (:plot [,view-start ,view-end]
              ,@(-non-nil
                 `(,(when key-dates
                      `(:vectors :data keydates :using [3 4 5 6 (x2tic 2)] :options (:arrowstyle 1)))
                   (:vectors :data gantt :using [1 2 3 4 7 (ytic 6)] :options (:arrowstyle variable))
                   ,(when blockers
                      (:vectors :data blockers :using [1 2 3 4] :options (:arrowstyle 3)))
                   ,(when shipping
                      `(:vectors :data shipping :using [1 2 3 4] :options (:arrowstyle 4)))
                   ;; (:labels :data blockers :using [1 2 5] :options (:left :font ",25" :tc "#Cfcfcf"))
                   ,(when fails
                      `(:labels :data fails :using [1 2 3] :options (:left :offset (0.25 0.25) :font ",25" :tc "#Cf0000")))
                   ,(when statuses
                      `(:labels :data statuses :using [1 2 3] :options (:left :offset (0 0.125) :font ",23" :tc "white")))))))
     options)))

;;;###autoload
(cl-defun gantt-simulation-to-resource-log-plot (simulation &rest options)
  (let* ((data (gantt-generate-resource-log simulation))
         (simulation-start (gantt-simulation-simulation-start simulation))
         (start-date (gantt-simulation-start-date simulation))
         (view-start (or (gantt-simulation-view-start-day simulation) 0))
         (view-end (or (gantt-simulation-view-end-day simulation) '*))
         (key-dates (gantt-simulation-key-dates simulation))
         (projects (gantt-simulation-projects simulation))
         (palette (gantt-create-palette (--map (gantt-project-name it) projects) 4))
         (height 1)
         (scale (or (plist-get options :fontscale) 1.0))
         labels)
    (apply
     'rysco-plot
     `((:unset key)
       (:data
        worklog
        ,@(cl-loop
           for i upfrom 1
           for (dev . proj-log) in data

           as dev-has-work = nil
           as dev-work = (cl-loop
                          for (proj . entries) in proj-log
                          as style-data = (cdr (assoc proj palette))
                          as style-id = (plist-get style-data :id)
                          as proj-obj = (--first (string= (gantt-project-id it) proj) projects)
                          as proj-type = (gantt-project-type proj-obj)

                          append
                          (cl-loop
                           with first-in-view
                           for (_ _ day effort) in (--sort
                                                    (< (nth 2 it)
                                                       (nth 2 other))
                                                    entries)

                           when (and (not first-in-view)
                                     (>= day view-start))
                           do (setq first-in-view day)

                           collect
                           (progn
                             (when (and first-in-view
                                        (= day first-in-view)
                                        (not (eq proj-type 'global-events)))
                               (setq dev-has-work t)
                               (push `(,day ,height ,(if gantt-plot-with-descriptions
                                                         (or (gantt-project-description proj-obj)
                                                             proj)
                                                       proj)) labels))
                             `(,proj ,dev ,day ,height
                                     ,(if (symbolp effort)
                                          1
                                        effort)
                                     0 ,style-id))))
           when dev-has-work do (cl-incf height)
           when dev-has-work append dev-work))

       (:data labels ,@labels)

       ,(when key-dates
          `(:data
            keydates
            ,@(cl-loop
               for (date label) in key-dates
               as day = (gantt-date-to-day start-date date)
               collect
               `(,date ,label ,day 0 0 ,height))))

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw ,(* 2 scale) lc "#999999") ;Key dates
       (:set style arrow 2 nohead lw ,(* 6 scale) lc "#8b008b") ;Simulation boundary

       ,@(cl-loop
          for (_ . style-data) in palette collect
          `(:set style
                 arrow ,(plist-get style-data :id)
                 nohead
                 lw ,(* 30 scale)
                 lc ,(plist-get style-data :color)))

       (:set arrow 2 from (,simulation-start 0) to (,simulation-start ,height) as 2)

       (:set yrange [,height 0])
       (:set grid x y)

       (:set ytics
             :out
             :font ",28"
             :textcolor "white")

       (:set x2tics
             nomirror
             :out
             :font ",20"
             :textcolor "#999999")

       (:tics x
              :options (:out
                        :font ",25"
                        :rotate by 20 right
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

       (:plot [,view-start ,view-end]
              ,@(-non-nil
                 `(,(when key-dates
                      `(:vectors :data keydates :using [3 4 5 6 (x2tic 2)] :options (:arrowstyle 1)))
                   (:vectors :data worklog :using [3 4 5 6 7 (ytic 2)] :options (:arrowstyle variable))
                   (:labels :data labels :using [1 2 3] :options (:rotate by 20 left :offset (0.95 0.7) :font ",20" :tc "black"))
                   (:labels :data labels :using [1 2 3] :options (:rotate by 20 left :offset (1.0 0.75) :font ",20" :tc "white"))))))
     options)))

;;;;
(provide 'gantt)
