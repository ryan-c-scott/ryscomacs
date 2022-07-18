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
  resource-log
  )

;;;###autoload
(cl-defun gantt-generate (data)
  ;; Convert to table
  (loop
   for proj in data collect
   `(,(gantt-project-id proj)
     ,(gantt-project-name proj)
     ,(gantt-project-started proj)
     ,(gantt-project-ended proj)
     ,(gantt-project-confidence proj)
     ,(gantt-project-dependencies proj)
     ,(gantt-project-resources proj)
     ,@(gantt-project-user-data proj))))

;;;###autoload
(cl-defun gantt-simulate (projects &optional resource-data)
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
       (setf (gantt-project-resource-log proj) (append log `((,res . ,day)))))

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
    projects)))

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
(cl-defun gantt-simulate-from-table (data &optional resources)
  (gantt-simulate
   (gantt-table-to-simulation data)
   (cdr resources)))

;;;###autoload
(cl-defun gantt-generate-from-table (data &optional resources)
  (gantt-generate
   (gantt-simulate-from-table data resources)))

;;;###autoload
(cl-defun gantt-to-latex (data &optional title)
  (s-join
   "\n"
   (cl-loop
    with out
    with project-end = 0
    for (_ name start end confidence) in data
    do (setq project-end (max project-end end))
    collect
    (format "\\ganttbar[progress=%s, progress label text={%s\\%%}]{%s}{%s}{%s} \\\\"
            confidence
            confidence
            name
            (1+ start)
            end)
    into out
    finally do
    (setq project-end (+ project-end
                         (- 10 (mod project-end 10))))
    finally return
    `("\\begin{ganttchart}["
      "expand chart=\\textwidth,"
      "y unit title=0.7cm,"
      "vgrid, hgrid,"
      "bar/.append style={fill=green!25},"
      "y unit chart=0.6cm]"
      ,(format "{1}{%s}," project-end)

      ,(when title
         (format
          "\\gantttitle{%s}{%s} \\\\"
          title
          project-end))

      ,(format "\\gantttitlelist{1,...,%s}{10} \\\\" (/ project-end 10))

      ,@out

      "\\end{ganttchart}"))))

;;;###autoload
(cl-defun gantt-simulation-to-table (data)
  (loop
   for (_ name start end confidence deps resources) in data
   as start = (floor (or start 0))
   as end = (ceiling (or end 0))
   collect
   `(,name
     ,(s-join " " resources)
     ,(concat
       (make-string start ?\_)
       (make-string (- end start) ?#)))))

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
