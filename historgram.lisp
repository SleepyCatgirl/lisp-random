(defvar *HIST-ARRAY* )
(setf *hist-array* nil)
(defvar *total-points*)
(setf *total-points* 0)

(defun new-histogram (num-bins)
  (setf *total-points* 0)
  (setf *hist-array*
        (make-array num-bins
                    :initial-element 0))
  t)


(defun record-value (number)
  (let ((array-len (length *hist-array*)))
    (incf *total-points*)
    (if (or
         (> number array-len)
         (< number 0))
        (format t "~&Number too big or below 0")
        (incf (aref *hist-array* number)))))


(defun print-hist-line (val)
  (let ((array-val (aref *hist-array* val)))
    (format t "~2S [~3S] " val array-val)
    (dotimes (i array-val)
      (format t "*"))))


(defun print-hist ()
  (let ((array-len (length *hist-array*)))
    (dotimes (i array-len)
      (format t "~&")
      (print-hist-line i))
    (format t "~&   ~3S" *total-points*)))
