(defun milles-per-gallon
    (initial-odometer-reading
     final-odometer-reading
     gallons-consumed)
  ((/ (- final-odometer-reading initial-odometer-reading) gallons-consumed)))
