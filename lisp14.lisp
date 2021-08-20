;;; PPMX - pretty prints a macro expansion
;;;
;;; From the book "Common Lisp:  A Gentle Introduction to
;;;      Symbolic Computation" by David S. Touretzky.
;;; The Benjamin/Cummings Publishing Co., 1990.
;;;
;;; Example of use:  (ppmx (incf a))


(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion:")
	    (pprint exp))
	   (t (format t "~&First step of expansion:")
	      (pprint exp1)
	      (format t "~%~%Final expansion:")
	      (pprint exp)))
     (format t "~%~%")
     (values)))

(defvar x)
(setf x '(a b c))

(ppmx (pop x))

(defstruct starship
  (name nil)
  (condition ’green))


(defmacro simple-incf (var)
  (list 'setq var (list '+ var 1)))
;; optional args
(defmacro simple-incf-op (var &optional (amount 1))
  (list 'setq var (list '+ var amount)))

(defstruct enemy
  (health 0)
  (armor 0)
  (attack 0))
(defvar nya)
(setf nya (make-enemy
           :health 5
           :armor 5
           :attack 10))


(defun faulty-incf (var)
  (setq var (+ var 1)))

(defmacro set-nil (var)
  (list 'setq var nil))

(defun ex-f (nyan)
  (format t "~A"
          (typecase nyan
            (enemy "A ENEMY!")
            (t (format nil "a ~A" (type-of nyan))))))

;; backquote char
;; and comma
(defvar name)
(setf name `fred)
`(this is ,name from pittsburh)
`(i gave ,name about ,(* 25 8) dollars)
;; so with macros
(defmacro simpl-inc (var &optional (amount 1))
  `(setq ,var (+ ,var ,amount)))


(defmacro simple-rotatef (x y)
  `(let* ((placeholder-x ,x)
          (placeholder-y ,y))
     (setq ,x placeholder-y)
     (setq ,y placeholder-x)))

;;(defun showvar (var)
;;  (format t "~& The value of ~S is ~S" 'var var))
(defmacro showvar (var)
  `(format t "~&The value of ~S is ~S" ',var ,var))
(defun f (x y)
  (showvar x)
  (showvar y)
  (* x y))


(defvar name)
(setf name 'fred)

(defvar address)
(setf address '(16 maple drive))

`(,name lives at ,address now)
`(,name lives at ,@address now)


(defmacro set-zero (&rest variables)
  `(progn ,@(mapcar #'(lambda (var)
                        (list 'setf var 0))
                    variables)
          `(zeroed ,variables)))

(defmacro set-mutual (var1 var2)
  `(progn
     (setf ,var1 ',var2)
     (setf ,var2 ',var1)))


(defmacro variable-chain (&rest variables)
  `(progn
     ,@(do ((var-l variables (rest var-l))
            (res nil))
           ((null (rest var-l)) (reverse res))
         (push `(setf ,(first var-l)
                      ',(second var-l))
               res))))
