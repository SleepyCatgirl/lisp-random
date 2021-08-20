(defun double-2 (n) (* n 2))
;; setf macro
;; assign a value to variable
;; if the variable exists,
;; replace
;; vovels by itself will give error, unassigned value
(setf vowels '(a e i o u))
(length vowels)
(rest vowels)
(cdr vowels)
(car vowels)
(setf vowels '(a e i o u and sometimes y))
;; Global variables are useful
;; to hold on values
;; so we don't have to retype them
;; e.g pi, or long-list of letters
(setf long-list '(a b c d e f g i j k l))
(setf head (first long-list))
(setf tail (rest long-list))
(cons head tail)
(equal long-list (cons head tail))
(list head tail)
(random 5.0)
;; good practice, setf only on global
