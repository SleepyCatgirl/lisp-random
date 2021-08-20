;; array
;; contigous block of storage
;; one-dimensional array - vector
;; (Only minor step from vectors to matrices and higher dimentionsal arrays)
;; components of vector of length n are numbered 0 - (n - 1)
;;
;;
(defvar my-vec)
(setf my-vec '#(tuning violin 440 a))
;; car or cdr, list operations dont work on vectors
;; storage is contiguouys, so accessing each element is as fast as
;; any other element
;; with lists, we have to get from one cons cell to next one

;; to print array
(setf *print-array* nil)
;; set print-array to t
(setf *print-array* t)

;; access vector with aref
(aref my-vec 1)
;; also as a place name for setf
;;tuning violin 440 a
(setf (aref my-vec 2) '340)
;; some functions we laerned, work on not only lists, but sequences, which
;; include vectors
(length my-vec)
(reverse my-vec)
(find-if #'numberp my-vec)
;; functions that dont work:
;; cara cdr member set functions, subst sublis nconc
;;
;;
;;
;; making arrays
(make-array 5)
;; creates 5-long array, filled with 0s
;; but,
(make-array 5 :initial-element 1)
;; will do same, but fill with 1s
;;
;; initial-contents
;; it must be, shorter or as long as the array.
(make-array 5 :initial-contents '(a e o i u))

;; strings are special kind of vectors
;; so things like length, reverse will work
(reverse "Nyaaa")
(aref "nyAAAAAa" 5)
(length "NYAAA")
;; character objects like #\k evalutae to themselves
(type-of #\k)

;; hash tables can be only made with funcsion
(make-hash-table)
(defvar h)
(setf h (make-hash-table))

(type-of h)
(setf (gethash 'john h)
      '(attorney (16 maple drivers)))
(setf (gethash 'mary h)
      '(physicst (23 cedar court)))
(gethash 'john h)
(gethash 'bill h)

(describe h)


;;property list
(setf (get 'fred 'sex) 'male)
(setf (get 'fred 'age) 23)
(setf (get 'fred 'siblings) '(george wanda))


(defun addprop (sym elem prop)
  (pushnew elem (get sym prop)))
(defun record-meeting (x y)
  (addprop x y 'has-met)
  (addprop y x 'has-met)
  t)

(defun subprop (symb value prop)
  (let ((prop-list (get symb prop)))
    (setf (get symb prop) (remove value prop-list))))

(defun forget-meeting (x y)
  (subprop x y 'has-met)
  (subprop y x 'has-met)
  'forgotten)


(defun get-nya (symb prop)
  (do ((got (symbol-plist symb) (cddr got)))
      ((null got) nil)
    (if (equal (first got) prop)
        (return (second got)))))
(defun hasprop (symb prop)
  (do ((got (symbol-plist symb) (cddr got)))
      ((null got) nil)
    (if (equal (first got) prop)
        (return t))))
