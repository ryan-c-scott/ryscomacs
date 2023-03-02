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
  (loop
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
    (loop
     for dev-day from 0 to (* sprints 10) by 10
     collect
     `(,(format-time-string
         (or format-string "%m/%d")
         (gantt-day-to-date start-date dev-day))
       ,dev-day))))

(cl-defun gantt-calculate-blocker (blockers externals start-date)
  (loop
   with latest-blocker

   for b in blockers
   as date = (cadr (assoc b externals 'string=))
   as day = (and date (gantt-date-to-day start-date date))

   when (and day
             (or (not latest-blocker)
                 (> day (cdr latest-blocker))))
   do
   (setq latest-blocker `(,b . ,day))

   finally return latest-blocker))

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

(cl-defun gantt-simulation-actual-work (simulation &optional start-date current-date)
  (cl-loop
   with end-day = (gantt-date-to-day start-date current-date)
   with resource-data = (gantt-simulation-resources simulation)

   for proj in (gantt-simulation-projects simulation)
   as proj-end = (or (gantt-project-ended proj) end-day)
   as id = (gantt-project-id proj)

   append
   (loop
    for (res . start) in (gantt-project-resource-log proj)
    as res-power = (or (cadr (assoc res resource-data)) 1)

    when (< start end-day) collect
    `(,id ,res ,start ,(min proj-end end-day) ,res-power))))

(cl-defun gantt-simulation--calculate-blockers (simulation)
  (loop
   with externals = (gantt-simulation-externals simulation)
   with start-date = (gantt-simulation-start-date simulation)

   for proj in (gantt-simulation-projects simulation) do
   (setf (gantt-project-start-blocker proj)
         (gantt-calculate-blocker
          (gantt-project-blockers proj)
          externals
          start-date))))

(cl-defun gantt-simulation--calculate-historic-work (simulation &optional simulation-start)
  "Generate a lookup table of `(ID . HOURS) from a simulation's historic data.
SIMULATION-START is day of simulation start and historic work will be clamped appropriately."
  (loop
   with simulation-start = (or simulation-start 0)
   for (id . entries) in (--group-by
                          (car it)
                          (gantt-simulation-work-log simulation))
   collect `(,id . ,(loop
                     for (_ _ start end effort) in entries
                     as end = (min end simulation-start)
                     as effort = (pcase effort
                                   ("" 1)
                                   (_ effort))

                     when (< start simulation-start) sum
                     (* (- end start) (or effort 1))))))

;;;###autoload
(cl-defun gantt-simulate (simulation)
  (let ((projects (gantt-simulation-projects simulation))
        (resource-data (gantt-simulation-resources simulation))
        (simulation-start-day (or (gantt-simulation-simulation-start simulation) 0))
        projects-completed)

    (gantt-simulation--calculate-blockers simulation)

    ;; Reduce all projects days by any historical efforts
    (loop
     with historic-work = (gantt-simulation--calculate-historic-work simulation simulation-start-day)

     for proj in projects
     as id = (gantt-project-id proj)
     as previous-effort = (cdr (assoc id historic-work))

     when previous-effort do
     (decf (gantt-project-work-remaining proj) previous-effort)

     when (<= (gantt-project-work-remaining proj) 0) do
     (progn
       (push id projects-completed)

       ;; NOTE: These are actually just dummy values to suppress their simulation
       ;; .They could instead be kept out of the simulation in some other way (maybe with another flag?)
       (setf (gantt-project-started proj) simulation-start-day
             (gantt-project-ended proj) simulation-start-day)))

    ;; Project simulation
    (setf
     (gantt-simulation-projects simulation)
     (loop
      with active-resources = (make-hash-table :test 'equal)

      for day from simulation-start-day to 100

      ;; TODO: Pivot to looping through devs
      ;; .A project will then need to be chosen based on listed priority
      ;; .Potential effort for each project listed will be determined
      ;; .Potential daily effort will be decremented from dev
      ;; .Early out if no effort is left
      ;; .projects-remaining and active-resources won't be needed at that point
      ;;
      ;; .More clearly, loop for each dev; for each dev project, loop until out of available effort, logging each to the projects
      as projects-remaining = (- (length projects) (length projects-completed))
      while (> projects-remaining 0) do
      (loop
       for proj in projects

       as id = (gantt-project-id proj)
       as started = (gantt-project-started proj)
       as ended = (gantt-project-ended proj)

       as resources = (gantt-project-resources proj)
       as available-resources = (--remove (gethash it active-resources) resources)

       as dependencies = (gantt-project-dependencies proj)
       as dependencies-met = (or (not dependencies)
                                 (--all? (member it projects-completed) dependencies))
       as start-blocker = (gantt-project-start-blocker proj)
       as blocked-externally = (and start-blocker
                                    (< day (cdr start-blocker)))
       as remaining = (gantt-project-work-remaining proj)

       ;; Handle starting based on resources
       when (and (not started)
                 (> remaining 0)
                 dependencies-met
                 (not blocked-externally)
                 (or (not resources)
                     (> (length available-resources) 0)))
       do
       (loop
        with resource-log
        for res in available-resources do
        (puthash res id active-resources)

        collect `(,res . ,day) into resource-log

        finally do
        (setf (gantt-project-started proj) day
              (gantt-project-resource-log proj) resource-log))

       ;; Step project simulation
       when (and started (not ended) dependencies-met) do
       (let* ((remaining (gantt-project-work-remaining proj))
              (devs (-union available-resources (mapcar 'car (gantt-project-resource-log proj))))
              (dev-power (gantt-calculate-resource-power resource-data devs))
              (new-remaining (- remaining dev-power)))

         ;; Detect and log added resources
         (loop
          for res in devs
          as log = (gantt-project-resource-log proj)
          unless (assoc res log 'equal) do
          (progn
            (puthash res id active-resources)
            (setf (gantt-project-resource-log proj) (append log `((,res . ,day))))))

         ;; Decrement remaining work
         (setf (gantt-project-work-remaining proj) new-remaining)
         (when (<= new-remaining 0)
           ;; Free resources
           (loop
            for res in resources
            as on-project = (equal (gethash res active-resources) id)

            when on-project do
            (puthash res nil active-resources))

           (push id projects-completed)
           (setf (gantt-project-ended proj) day))))

      finally return
      (--sort
       (let ((it-start (or (gantt-project-started it) 0))
             (it-end (or (gantt-project-ended it) 1000))
             (other-start (or (gantt-project-started other) 0))
             (other-end (or (gantt-project-ended other) 1000)))
         (if (= it-start other-start)
             (< it-end other-end)
           (< it-start other-start)))
       projects)))
    simulation))

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
        :projects (hash-table-values projects)))))

(cl-defun gantt-calculate-resource-power (resource-data devs)
  (cl-loop
   for d in devs
   as commitment = (cadr (assoc d resource-data))
   sum (or commitment 1)))

(cl-defun gantt-table-to-simulation (&key projects resources externals work-log start-date current-date)
  (make-gantt-simulation
   :resources (cdr resources)
   :externals (cdr externals)
   :work-log (cdr work-log)
   :start-date start-date
   :simulation-start (if current-date
                         (gantt-date-to-day start-date current-date)
                       0)

   :projects
   (cl-loop
    with data = (cdr projects)
    for (key name days confidence adjustment deps resources blockers started completed . rest) in data
    as dependencies = (s-split "" deps t)
    as resources = (s-split " " resources t)
    as blockers = (s-split " " blockers t)
    as adjustment = (pcase adjustment
                      ((pred stringp)
                       (when (not (string-empty-p adjustment))
                         (cl-parse-integer adjustment)))
                      (_ adjustment))
    as actual-started = (unless (string-empty-p started) (gantt-date-to-day start-date started))
    as actual-ended = (unless (string-empty-p completed) (gantt-date-to-day start-date completed))

    unless (s-match "[!^_$#*/]" key)
    collect
    (make-gantt-project
     :id key
     :name name
     :work days
     :confidence confidence
     :adjustment adjustment
     :dependencies dependencies
     :resources resources
     :blockers blockers
     :actual-started actual-started
     :actual-ended actual-ended
     :user-data rest

     :work-remaining (+ days (or adjustment 0))
     :resource-log nil))))

;;;###autoload
(cl-defun gantt-simulate-from-table (data &key resources externals work-log start-date current-date)
  (gantt-simulate
   (gantt-table-to-simulation
    :projects data
    :resources resources
    :externals externals
    :work-log work-log
    :start-date start-date
    :current-date current-date)))

;;;###autoload
(cl-defun gantt-simulation-to-table (simulation)
  (loop
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

;;;###autoload
(cl-defun gantt--find-table (name)
  (org-element-map (org-element-parse-buffer) 'table
    (lambda (tbl)
      (when (string= name (org-element-property :name tbl))
        tbl))
    nil t))

(cl-defun gantt--goto-and-delete-table (tbl)
  (-when-let* ((start (org-element-property :post-affiliated tbl))
               (end (org-element-property :end tbl)))
      (goto-char start)
      (delete-region start end)))

(cl-defun gantt--find-and-replace-table (name header data)
  (-when-let* ((tbl (gantt--find-table name))
               (post-blank (org-element-property :post-blank tbl)))
    (gantt--goto-and-delete-table tbl)
    (insert
     (orgtbl-to-orgtbl
      (append header data)
      nil)
     (make-string (1+ post-blank) ?\n))))

;;;###autoload
(cl-defun gantt-reorganize-tables (projects-name work-log-name)
  ;; TODO: Undo if error

  (let ((projects (gantt--find-table projects-name))
        (work-log (gantt--find-table work-log-name)))

    (when (not (and projects work-log))
      (error "Tables missing or malformed"))

    ;; Generate map and fix tables
    (let* ((projects-begin (org-element-property :post-affiliated projects))
           (projects-end (org-element-property :end projects))
           (projects-raw (-split-at 3 (org-table-to-lisp (buffer-substring-no-properties projects-begin projects-end))))
           (projects-header (car projects-raw))
           (projects-data (cadr projects-raw))

           (mapping (loop
                     with new-id = ?A
                     for entry in projects-data
                     as map = (pcase entry
                                (`(,id . ,_)
                                 (cons id (char-to-string new-id)))
                                (_))
                     when map collect map
                     when map do (incf new-id)))

           (work-log-begin (org-element-property :post-affiliated work-log))
           (work-log-end (org-element-property :end work-log))
           (work-log-raw (-split-at 3 (org-table-to-lisp (buffer-substring-no-properties work-log-begin work-log-end))))
           (work-log-header (car work-log-raw))
           (work-log-data (cadr work-log-raw))

           (projects-fixed
            (loop
             with project-mappings = mapping

             for entry in projects-data collect
             (pcase entry
               (`(,_ ,name ,days ,confidence ,adjust ,deps . ,rest)
                (let ((map (pop project-mappings)))
                  `(,(cdr map) ,name ,days ,confidence ,adjust
                    ,(loop
                      for d in (s-split "" deps t) concat
                      (cdr (assoc d mapping)))
                    ,@rest)))
               (_ entry))))

           (work-log-fixed
            (loop
             for entry in work-log-data collect
             (pcase entry
               (`(,id . ,rest)
                `(,(cdr (assoc id mapping)) ,@rest))
               (_ entry)))))

      (save-excursion
        (gantt--find-and-replace-table work-log-name work-log-header work-log-fixed)
        (gantt--find-and-replace-table projects-name projects-header projects-fixed)))))

;;;;
(provide 'gantt)
