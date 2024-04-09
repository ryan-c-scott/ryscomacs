;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'gantt)

(describe "(Gantt) Project Planning DSL"
  (it "Converts from date to project day"
    (expect
     (gantt-date-to-day "2023-05-01" "2023-05-05")
     :to-be 4))

  (it "Converts from project day to date"
    (expect
     (format-time-string
      "%F"
      (gantt-day-to-date "2023-05-01" 4))
     :to-equal "2023-05-05"))

  (it "Handles weekends"
    (expect
     (format-time-string
      "%F"
      (gantt-day-to-date "2023-05-01" 10))
     :to-equal "2023-05-15"))
  )
