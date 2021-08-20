;; iteration and block structure
;; dotimes and dolist
;;
;;
;; (dotimes (index-var n [result-form])
;;       body)
;; (dolist (index-var list [result-form])
;;       body)



(dotimes (i 4)
  (format t "~&I is ~S" i))
(dolist (x '(red blue green) 'flowers)
  (format t "~&Roeses are ~S" x))

;;exiting loop/return
(defun find-first-odd (list-of-numbers)
  (dolist (e list-of-numbers)
    (format t "~&Testing: ~S" e)
    (when (oddp e)
      (format t "~&Found odd number")
      (return e))))

(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))


(defun member-iter (item list-items)
  (dolist (x list-items)
    (when (equal x item)
      (format t "~&Found odd number")
      (return x))))
(defun it-assoc (item list-items)
  (dolist (x list-items)
    (when (equal (car x) item)
      (format t "~&Found assoc item")
      (return x))))
;; comparing recursive and iterative
(defun rec-ffo (x)
  "find first odd number"
  (cond ((null x) nil)
        ((oddp (first x)) (first x))
        (t (rec-ffo (rest x)))))
(defun iter-ffo (x)
  (dolist (e x)
    (if (oddp e) (return e))))

(defun it-fact (n)
  (let ((prod 1))
    (dotimes (i n prod)
      (setf prod (* prod (+ i 1))))))

(defun it-intersect (x y)
  (let ((result-set nil))
    (dolist (element x result-set)
      (when (member element y)
        (push element result-set)))))

(defun it-length (x)
  (let ((len 0))
    (dolist (index x len)
      (setf len (+ len 1)))))

(defun it-nth (n ls)
  (let ((nthls ls))
    (dotimes (index n (car nthls))
      (setf nthls (cdr nthls)))))

(defun it-union (x y)
  (let ((union-y nil))
    (dolist (element x union-y)
      (when (member element y)
        (setf union-y (cons element union-y))))))

;; simplest way to apply function to every element is mapcar
(defun app-square-list (list-numbers)
  (mapcar #'(lambda (n) (* n n))
          list-numbers))

;; dolist
(defun faulty-it-square-list (list-numbers)
  (let ((result nil))
    (dolist (e list-numbers result)
      (push (* e e) result))))
;; but its backwards, so

(defun it-square-list (list-numbers)
  (let ((result nil))
    (dolist (e list-numbers (reverse result))
      (push (* e e) result))))

(defun it-intersect-corrected (x y)
  (let ((result-set nil))
    (dolist (element x (reverse result-set))
      (when (member element y)
        (push element result-set)))))

(defun it-reverse (list-elements)
  (let ((result nil))
    (dolist (e list-elements result)
      (push e result))))


;; do macro
;; most powerful
;; but syntax is rather complex
;; (do ((var1 init1 [update1])
;;      (var2 init2 [update2])
;;        ...)
;;      (test action-1 ... action-n)
;;    body)
;; LAUNCH
(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "Blast off"))
    (format t "~S.." cnt)))

(defun check-all-odd-do (list-num)
  (do ((num list-num (cdr num)))
      ((null num) t)
    (format t "~&Checking ~S..." (car num))
    (if (evenp (car num)) (return nil))))

(defun launch-dotimes (n)
  (let ((num (+ 1 n)))
    (dotimes (index n nil)
      (setf num (- num 1))
      (format t "~S..." num)
      (if (zerop (- num 1))
          (format t "LIFT OFF")))))



(defun count-slices (loaf)
  (do ((cnt 0 (+ cnt 1))
       (z loaf (rest z)))
      ((null z) cnt)))

(defun fact-it (n)
  (do ((i n (- i 1))
       (result 1 (* result i)))
      ((zerop i) result)))

(defun ffo-with-do (list-of-numbers)
  (do ((x list-of-numbers (rest x)))
      ((null x) nil)
    (if (oddp (first x)) (return (first x)))))

(defun ffo-with-do* (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (e (first x) (first x)))
       ((null x) nil)
    (if (oddp e) (return e))))

(defun find-largest (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (e (first x) (if
                      (> (first x) (second x))
                      e
                      (second x))))
       ((null x) nil)
    (format t "~S" e)))
(defun find-largest (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (e (first x) (if
                      (> e (first x))
                      e
                      (first x))))
        ((null (cdr x)) e)
    (format t "~&~S" e)))
(defun power-of-2 (n)
  "2 to the n power"
  (do ((index 1 (+ 1 index))
       (two-n 2 (* 2 two-n)))
      ((equal index n) two-n)))

(defun first-non-integer (x)
  (let ((first-element nil))
    (dolist (index x first-element)
      (when (not (integerp index))
        (setf first-element index)
        (return first-element)))))


;;(defun broken-ffo-with-do* (list-of-numbers)
;;  (do   ((x list-of-numbers (rest x))
;;        (e (first x) (first x)))
;;       ((null x) nil)
;;    (if (oddp e) (return e))))
;;


(defun bug-ffo-with-do (x)
  (do ((z x (rest z))
       (e (first x) (first z)))
      ((null z) nil)
    (if (oddp e) (return e))))
;; infintie loops
(defun read-a-number ()
  (do ((answer nil))
      (nil)
    (format t "~&Please type a number: ")
    (setf answer (read))
    (if (numberp answer)
         (return answer))
        (format t
                "~&Sorry, ~S is not number. Try again" answer)))
