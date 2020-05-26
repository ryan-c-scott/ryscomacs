;;; -*- lexical-binding: t; -*-

(cl-defun gantt-get-start-time (key data &optional accum)
  (-let (((_ _ days _ depencies) (assoc key data))
         (time (or accum 0)))
    (-
     (if depencies
         (cl-loop
          with latest = 0
          for d in depencies do
          (setq latest
                (max latest
                     (gantt-get-start-time d data (+ time days))))
          finally return latest)
       (+ days time))
     (if accum 0 days))))

(cl-defun gantt-generate (data)
  (--sort
   (-let (((_ _ it-start it-end) it)
          ((_ _ other-start other-end) other))
     (if (= it-start other-start)
         (< it-end other-end)
       (< it-start other-start)))

   (cl-loop
    for (key name days . misc) in data
    as start = (gantt-get-start-time key data)
    collect
    `(,key ,name ,start ,(+ start days) ,@misc))))

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
