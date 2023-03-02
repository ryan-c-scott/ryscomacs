;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl)

(cl-defstruct gantt-project
  ""
  id
  name
  work
  confidence
  adjustment
  dependencies
  resources
  user-data
  blockers
  actual-started
  actual-ended

  ;; Simulation data
  work-remaining
  start-blocker
  started
  ended
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

(cl-defun gantt-calculate-sprint-dates (start-date sprints &optional format-string)
  (when start-date
    (cl-loop
     for dev-day from 0 to (* sprints 10) by 10
     collect
     `(,(format-time-string
         (or format-string "%m/%d")
         (gantt-day-to-date start-date dev-day))
       ,dev-day))))

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
     (`(after ,begin-date ,val)
      `(when (string> simulation-date ,(format "%s" begin-date))
         (setq effort ,val)))

     (`(before ,end-date ,val)
      `(when (string< simulation-date ,(format "%s" begin-date))
         (setq effort ,val)))

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

(cl-defun gantt-transform-projects (data)
  (cl-loop
   with projects = (make-hash-table :test 'equal)

   for (id . proj) in data
   as id = (format "%s" id)
   as days = (plist-get proj :days)
   as adjustment = (plist-get proj :adjustment)

   do
   (puthash
    id
    (make-gantt-project
     :id id
     :name (or (plist-get proj :name) id)
     :work (plist-get proj :days)
     :confidence (plist-get proj :confidence)
     ;; :adjustment adjustment
     ;; :dependencies dependencies
     ;; :resources resources
     ;; :blockers blockers
     ;; :actual-started actual-started
     ;; :actual-ended actual-ended
     ;; :user-data rest

     :work-remaining (+ days (or adjustment 0))
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

(cl-defun gantt-transform-devs (data)
  (cl-loop
   for (id . dev) in data
   collect
   `(,(format "%s" id)
     :effort ,(gantt-transform-effort (plist-get dev :effort))
     :projects
     ,(cl-loop
       for proj in (plist-get dev :projects) collect
       (gantt-transform-dev-project proj)))))

;;;###autoload
(cl-defmacro gantt-derive-dev-form (start-date &rest forms)
  (let ((projects (plist-get forms :projects))
        (devs (plist-get forms :devs)))

    `(let ((projects ,(gantt-transform-projects projects))
           (devs ',(gantt-transform-devs devs)))

       '(:errors
         ,@(-uniq
            (cl-loop
             for dev in devs append
             (cl-loop
              for proj in (plist-get (cdr dev) :projects)
              as id = (if (listp proj) (car proj) proj)
              unless (assoc id projects) collect
              (format "No project '%s' defined in :PROJECTS section (referenced in dev '%s')" id (car dev))))))

       (cl-loop
        with completed
        until completed
        for day from 0 to 100
        as simulation-date = (format-time-string "%F" (gantt-day-to-date ,start-date day))

        do
        (cl-loop
         for (dev . data) in devs
         collect
         (cl-loop
          with effort = (funcall (plist-get data :effort) simulation-date)
          while (> effort 0)
          for (dev-proj-id . dev-proj-data) in (plist-get data :projects)
          as proj = (gethash dev-proj-id projects)
          ;; TODO: Project specific effort (min of dev and project forms)
          ;; as project-effort
          ;; TODO: dependencies
          as remaining = (gantt-project-work-remaining proj)
          as new-remaining = (max 0 (- remaining effort))
          as work-done = (- remaining new-remaining)
          as started = (gantt-project-started proj)
          as ended = (gantt-project-ended proj)
          as resources = (gantt-project-resources proj)
          as resource-log = (gantt-project-resource-log proj)

          when (and (> remaining 0)
                    (> work-done 0))
          do
          (progn
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
              (setq effort 0)
              )))))

       (make-gantt-simulation
        :start-date ,start-date
        :simulation-start 0
        :work-log nil
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
   as end = (ceiling (or (gantt-project-ended proj) 0))
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
         blockers)
    (apply
     'rysco-plot
     `((:unset key)
       (:data gantt ,@(cl-loop
                       for i upfrom 1
                       for proj in projects
                       as entry = (pcase-let (((cl-struct gantt-project id name started ended resources start-blocker) proj))
                                    (when start-blocker
                                      (push `(0 ,i ,(cdr start-blocker) 0 ,(format "[{/:Bold %s}]" (car start-blocker))) blockers))
                                    `(,started ,i ,(- (or ended 100) (or started 0)) 0 ,id ,(format "%s: %s" name resources)))
                       when entry collect entry))

       (:data blockers ,@blockers)

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw 3 lc "#Eedd82") ;Period boundaries
       (:set style arrow 2 nohead lw 20 lc "#8deeee") ;Projects
       (:set style arrow 3 filled lw 3 lc "#8b008b") ;External blockers

       (:set arrow 1 from (60 0) to (60 ,height) as 1)
       (:set arrow 2 from (,simulation-start 0) to (,simulation-start ,height) as 1)

       (:set yrange [,height 0])
       (:set grid x y)

       (:set ytics
             :out
             :font ",28"
             :textcolor "white")

       (:set lmargin ,(*
                       1.8 ;; HACK: Magic number to create space for the larger ytics
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
              ,(gantt-calculate-sprint-dates (gantt-simulation-start-date simulation) 10))

       (:set rmargin 5)
       (:set bmargin 5)

       (:plot [* *]
              (:vectors :data gantt :using [1 2 3 4 (ytic 6)] :options (:arrowstyle 2))
              (:vectors :data blockers :using [1 2 3 4] :options (:arrowstyle 3))
              (:labels :data blockers :using [1 2 5] :options (:left :font ",25" :tc "#Cfcfcf" :front)))
       )
     options)))

;;;###autoload
(cl-defun gantt-simulation-to-resource-log-plot (simulation &rest options)
  (let* ((data (gantt-generate-resource-log simulation))
         (simulation-start (gantt-simulation-simulation-start simulation))
         (projects (gantt-simulation-projects simulation))
         (height (1+ (length data)))
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
             for (_ _ day effort) in entries collect
             `(,proj ,dev ,day ,i ,(or 1 effort) 0 ,style-id))

            finally do (setq project-count (hash-table-count project-colors)))))

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw 3 lc "#Eedd82")
       (:set style arrow 2 nohead lw 20 lc "#8deeee")

       ,@(cl-loop
          for i upfrom 0
          for color in (gantt-create-palette project-count)
          collect
          `(:set style arrow ,(+ i 3) nohead lw 30 lc ,color))

       (:set arrow 1 from (60 0) to (60 ,height) as 1)
       (:set arrow 2 from (,simulation-start 0) to (,simulation-start ,height) as 1)

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
              ,(gantt-calculate-sprint-dates (gantt-simulation-start-date simulation) 10))

       (:set lmargin ,(cl-loop
                        for proj in projects maximize
                        (pcase-let (((cl-struct gantt-project id name resources) proj))
                          (length (format "%s: %s" name resources)))))
       (:set rmargin 5)
       (:set bmargin 5)

       (:plot [0 *]
              (:vectors :data worklog :using [3 4 5 6 7 (ytic 2)] :options (:arrowstyle variable)))
       )
     options)))

;;;;
(provide 'gantt)
