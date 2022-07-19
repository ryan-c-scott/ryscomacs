;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl)

(cl-defstruct gantt-project
  ""
  id
  name
  work
  confidence
  dependencies
  resources
  user-data

  ;; Simulation data
  work-remaining
  started
  ended
  resource-log)

(cl-defstruct gantt-simulation
  ""
  projects
  resources
  start-date)

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

(cl-defun gantt-generate-resource-log (simulation)
  (--group-by
   (car it)
   (loop for proj in (gantt-simulation-projects simulation)
         as id = (gantt-project-id proj)
         as ended = (gantt-project-ended proj)
         as resource-log = (gantt-project-resource-log proj)
         append
         (loop for (res . start) in resource-log collect
               `(,res ,proj ,start ,ended)))))

;;;###autoload
(cl-defun gantt-simulate (projects &optional resource-data start-date)
  (make-gantt-simulation
   :start-date start-date
   :projects
   (loop
    with projects-remaining = (length projects)
    with active-resources = (make-hash-table :test 'equal)
    with projects-completed

    for day upfrom 0 to 100

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

     ;; Handle starting based on resources
     when (and (not started)
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

         (decf projects-remaining)
         (push id projects-completed)
         (setf (gantt-project-ended proj) day))))

    finally return
    (--sort
     (let ((it-start (gantt-project-started it))
           (it-end (gantt-project-ended it))
           (other-start (gantt-project-started other))
           (other-end (gantt-project-ended other)))
       (if (= it-start other-start)
           (< it-end other-end)
         (< it-start other-start)))
     projects))

   :resources resources))

(cl-defun gantt-calculate-resource-power (resource-data devs)
  (loop
   for d in devs
   as commitment = (cadr (assoc d resource-data))
   sum (or commitment 1)))

(cl-defun gantt-table-to-simulation (data)
  (cl-loop
   with data = (cdr data)
   for (key name days confidence deps resources . rest) in data
   as dependencies = (s-split "" deps t)
   as resources = (s-split " " resources t)
   unless (s-match "[!^_$#*/]" key)
   collect
   (make-gantt-project
    :id key
    :name name
    :work days
    :confidence confidence
    :dependencies dependencies
    :resources resources
    :user-data rest

    :work-remaining days
    :resource-log nil
    )))

;;;###autoload
(cl-defun gantt-simulate-from-table (data &optional resources start-date)
  (gantt-simulate
   (gantt-table-to-simulation data)
   (cdr resources)
   start-date))

;;;###autoload
(cl-defun gantt-simulation-to-table (simulation)
  (loop
   for proj in (gantt-simulation-projects data)
   as start = (floor (or (gantt-project-started proj) 0))
   as end = (ceiling (or (gantt-project-ended 0)))
   collect
   `(,(gantt-project-name proj)
     ,(s-join " " resources)
     ,(concat
       (make-string start ?\_)
       (make-string (- end start) ?#)))))

;;;###autoload
(cl-defun gantt-simulation-to-plot (simulation &rest options)
  (apply
   'rysco-plot
   `((:unset key)
     (:data gantt ,@(loop
                     for i upfrom 1
                     for proj in (gantt-simulation-projects simulation) collect
                     (pcase-let (((cl-struct gantt-project id name started ended resources) proj))
                       `(,started ,i ,(- (or ended 0) (or started 0)) 0 ,id ,(format "%s: %s" name resources)))))

     (:set :border lc "white")

     (:set style line 1 lc "yellow")

     (:set style arrow 1 nohead lw 3 lc "#Eedd82")
     (:set style arrow 2 nohead lw 20 lc "#8deeee")

     (:set arrow 1 from (60 0) to (60 ,(length projects)) as 1)

     (:set yrange [,(length projects) 0])
     (:set grid x y)

     (:set ytics
           :out
           :font ",28"
           :textcolor "white")

     (:set lmargin ,(*
                     3 ;; HACK: Magic number to create space for the larger xtics
                     (loop
                      for (id name _ _ _ resources . rest) in projects maximize
                      (length (format "%s: %s" name resources)))))

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
            (:vectors :data gantt :using [1 2 3 4 (ytic 6)] :options (:arrowstyle 2)))
     )
   options))

;;;###autoload
(cl-defun gantt-simulation-to-resource-log-plot (simulation &rest options)
  (let* ((data (gantt-generate-resource-log simulation))
         (height (1+ (length data)))
        project-count)
    (apply
     'rysco-plot
     `((:unset key)

       (:data gantt ,@(loop
                       with project-colors = (make-hash-table :test 'equal)
                       with next-project-id = 3

                       for i upfrom 1
                       for (res . log) in data append
                       (loop for (_ proj start end) in log
                             as project-id = (gantt-project-name proj)
                             as project-name = (gantt-project-name proj)
                             as style-id = (gethash project-id project-colors)

                             unless style-id do
                             (setq style-id (puthash project-id next-project-id project-colors)
                                   next-project-id (1+ next-project-id))

                             collect
                             `(,start ,i ,(- end start) 0 ,project-name ,res ,style-id))
                       finally do (setq project-count (hash-table-count project-colors))))

       (:set :border lc "white")

       (:set style line 1 lc "yellow")

       (:set style arrow 1 nohead lw 3 lc "#Eedd82")
       (:set style arrow 2 nohead lw 20 lc "#8deeee")

       ,@(loop
          for i upfrom 0
          for color in (gantt-create-palette project-count)
          collect
          `(:set style arrow ,(+ i 3) nohead lw 30 lc ,color))

       (:set arrow 1 from (60 0) to (60 ,height) as 1)

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

       (:set lmargin ,(*
                       1 ;; HACK: Magic number to create space for the larger xtics
                       (loop
                        for (id name _ _ _ resources . rest) in projects maximize
                        (length (format "%s: %s" name resources)))))
       (:set rmargin 5)
       (:set bmargin 5)

       (:plot [* *]
              (:vectors :data gantt :using [1 2 3 4 7 (ytic 6)] :options (:arrowstyle variable))
              (:labels :data gantt :using [1 2 5] :options (:left :font ",20")))
       )
     options)))

;;;###autoload
(cl-defun gantt-resource-log-to-table (data)
  (loop
   for (res . log) in data collect
   `(,res
     ,(loop
       with last = 0
       for (_ proj start end) in log
       as id = (gantt-project-id proj)
       concat (make-string (- start last) ?\s)
       concat (make-string (- end start) (aref id 0))
       do (setq last end)))))

;;;###autoload
(cl-defun gantt-reorganize-table ()
  (interactive)
  (let* ((begin (org-table-begin))
         (end (org-table-end))
         (raw (-split-at 3 (org-table-to-lisp)))
         (header (car raw))
         (data (cadr raw))
         (mapping (loop
                   with new-id = ?A
                   for entry in data
                   as map = (pcase entry
                              (`(,id . ,_)
                               (cons id (char-to-string new-id)))
                              (_ t))
                   when map collect map
                   do (incf new-id))))

    (save-excursion
      (delete-region begin end)
      (insert
       (format
        "%s\n"
        (orgtbl-to-orgtbl
         (append
          header
          (loop
           for map in mapping
           for entry in data collect

           (pcase entry
             (`(,_ ,name ,days ,confidence ,deps ,resources)
              `(,(cdr map) ,name ,days ,confidence
                ,(loop
                  for d in (s-split "" deps t) concat
                  (cdr (assoc d mapping)))
                ,resources))

             (_ entry)
             )))
         nil))))))
