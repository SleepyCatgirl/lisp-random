;; assigment
;; updading a global variable : setf - set field
;; global variables naming convention: Wrapped in *s
(setf *total-glasses* 8)

(defun sell (n)
  (setf *total-glasses* (+ *total-glasses* n))
  (format t "That makes ~S so far today"
          *total-glasses*))
;; macros for working on global variables through setf
;; To express (setf a (+ a n)) or -
(setf *a* 2)
(incf *a*)
(decf *a*)

(defun sell (n)
  (incf *total-glasses* n)
  (format t "That makes ~S so far today"
          *total-glasses*))

;; To express (setf a (cons n a))
(setf mystack nil)
(push 'dish1 mystack)
(push 'dish2 mystack)
(push 'dish3 mystack)
(pop mystack)
;; pop is equiliavent to
;; (let ((top-element (first mystack)))
;;(setf mystack (rest mystack))
;;top-element)
(setf *friends* nil)
(setf *friends-more* 0)
(defun meet (person)
  (cond ((equal person (first *friends*))
         (incf *friends-more*)
         'we-just-met)
        ((member person *friends*)
         (incf *friends-more*)
         'we-know-eachother)
        (t (push person *friends*)
           'pleased-to-meet-you)))

(defun forget (person)
  (cond ((not (member person *friends*))
         (format t "~%~S is not on the list!" person))
        (t (setf *friends* (remove person *friends*))
           (format t "FORGOTTEN"))))
