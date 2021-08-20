;; 3.22
;; c.
(defun myfun (alpha beta)
  (list (cons alpha nil) beta))
(myfun 'alpha 'beta)
;; d.
(defun firstp (a1 l1)
  (equal a1 (car l1)))
(firstp 'foo '(foo bar baz))
(firstp 'boing '(foo bar baz))
;; e.
(defun mid-add1 (x)
  (list
   (first x)
   (+ 1 (second x))
   (third x)))
(mid-add1 '(1 2 3))
;; f.
(defun f-to-c (x)
  (/ (* 5.0 (- x 32)) 9))
(f-to-c 80)
;; 3.16
;; functions without args
(defun test () (* 85 97))
(test)
;; quote
(quote foo)
(quote up)
(cons (quote up) (quote (down nya)))
;; double quotes and what not, since ` -> quote
``foo
(list `quote `foo)
(length ``foo)
;; symbol-name and symbol-function
(symbol-name 'equal)
(symbol-function 'equal)
;; lambda
;; This way, f(x) = x + 3 in lambda
;; λx.(3+x)
(lambda (x) (+ 3 x))
;; λx.(3+x) 3
((lambda (x) (+ 3 x)) 3)
;; f(x,y) = 3x + y^2
;; λ(x,y).(3x+y^2)
(lambda (x y) (+ (* 3 x) (* y y)))
;; Apply
;; Primitive funcion
;; takes function and a list of objects
;; First argument should be quoted with
;; #'
;; is the proepr way to quote function supplied as inputs
;; to other functions
(apply #'+ '(2 3))
(apply #'equal '(12 17))
(apply #'cons '(as (you like it)))


;; Exercises
(list 'cons t nil)
(eval (list 'cons t nil))
(eval (eval (list 'cons t nil)))
(apply #'cons '(t nil))
(eval nil)
(list 'eval nil)
(eval (list 'eval nil))
;; conditionals
;; (if (test) true-part false-part)
;; if test is true, return true-part, if false, return false-part
(if (oddp 1) 'odd 'even)
(if (oddp 2) 'odd 'even)
(if t 'test-was-true 'test-was-false)
(if nil 'test-was-true 'test-was-false)
(if (symbolp 'foo) (* 5 5) (+ 5 5))
(if (symbolp 1) (* 5 5) (+ 5 5))

;; absolute value
(defun my-abs (x)
  (if (< x 0) (- x) x))
