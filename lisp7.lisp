;; Applicative programming
;; functions are data
;; can pass to functions like such
;;
;;
;; Funcall
;; calls a function on some inputs
;; e.g
(funcall #'cons 'a 'b)
;; => (A . B)
;; #' is correct way to quote a function
(setf fn #'cons)
(type-of fn)
(funcall fn 'c 'd)
;; only ordinary functiosn can be quoted with #'
;; e.g #'if would error
;; or #'turnips, its undefined
;;
;; mapcar
;; applies function to each element of list
(defun square (n) (* n n))
(square 3)
;; (square '(1 2 3 4))
;; would error
;;
;; but with mapcar
(mapcar #'square '(1 2 3 4))
(mapcar #'square '(2 48 53 2 1 -6))
;;
;;suppose we wanted to set the global variable words
(setf words '((one un)
              (two deux)
              (three trois)
              (four quatre)
              (five cinq)))
(mapcar #'car words)
(mapcar #'second words)
(mapcar #'reverse words)
(defun translate (x)
  (second (assoc x words)))
(mapcar #' translate '(three one four one))


;; exercises
(defun add1 (x)
  (+ 1 x))
(mapcar #'add1 '(1 3 5 7 9))

(setf daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
                     (kent clark 089-52-6767 reporter)
                     (lane lois 341-24-6313 reporter)
                     (white perry 255-21-2211 editor)))
(mapcar #'third daily-planet)
(mapcar #'zerop '(2 0 3 5 0 -6 5))

(defun gr-t-five-p (x)
  (> x 5))
(mapcar #'gr-t-five-p '(2 0 3 7 3 33))
;; lambda
;; two ways to use by applicative operator like mapcar
;; first is to defun a function and specify it by #'name
;;
;;second is, to pass function definition directly
;; done by writing a list called a lambda expression
;; eg.
(lambda (n) (* n n))
(mapcar #'(lambda (n) (* n n)) '(1 2 3))
;; lambda is a marker that says - this list represents a funcsion
;; useful for synthesizing one input functions from related functions of twu inputs
;; for example multiply every element of list by 10
;; mapcar #'* '(1 2 3 4 5)
;; but where is 10 supposed t odo?
;; mapcar gives only one input.
;; solution
(mapcar #'(lambda (n) (* n 10)) '(1 2 3 4 5))
;; or
(mapcar #'(lambda (n) (list 'hi 'there n)) '(joe fred wanda))
#'(lambda (n) (* n 10))
;; exercises
(funcall #'(lambda (n) (- n 7)) 8)
(lambda (n)
  (if (or (equal n t) (equal n nil))
      t nil))
(mapcar #'(lambda (n) (if (equal n 'up) 'down 'up)) '(down up down down))

;; find-if
;; applicative operator
;; give predicate and lis as input
;; will find the first element for which the predicate returns true, any non-nil value
(find-if #'oddp '(2 4 6 7 8 9))
(find-if #'(lambda (x) (> x 3)) '(2 4 5 6 7 8))
;; our assoc
;; assoc searches for a table entry with a specified key
(defun my-assoc (key table)
  (find-if #'(lambda (entry)
               (equal key (first entry)))
           table))
(my-assoc 'two words)

;; exercise
(defun rough-list (x k)
  (find-if #'(lambda (num)
               (if (and (> num (- k 10)) (< num (+ k 10))) t nil)) x))
(rough-list '(33 64 66 78 80) 87)
;; and now book answer
(defun roughly-equal (e k)
  (and (not (< e (- k 10)))
       (not (> e (+ k 10)))))
(defun find-first-roughly-equal (x k)
  (find-if #'(lambda (e) (roughly-equal e k)) x))
(defun find-list (k)
  (find-if #'consp k))



(setf note-table '((c   .   1)
                   (c-sharp .   2)
                   (d   .   3)
                   (d-sharp .   4)
                   (e   .   5)
                   (f   .   6)
                   (f-sharp .   7)
                   (g   .   8)
                   (g-sharp .   9)
                   (a   .   10)
                   (a-sharp .   11)
                   (b   .   12)))
(defun numbers (list)
  (mapcar #'(lambda (n) (cdr (assoc n note-table))) list))
(defun table-search (numb)
  (car (rassoc numb note-table)))
(defun notes (list)
  (mapcar #'table-search list))
(defun raise (num list)
  (mapcar #'(lambda (n) (+ n num)) list))
(defun normalize-number (num)
  (cond ((< num 1) (+ num 12))
        ((> num 12) (- num 12))
        (t num)))
(defun normalize (list)
  (mapcar #'normalize-number list))
(defun transpose (n song)
  (notes (normalize (raise n (numbers song)))))


;; remove-if
;; takes predicate
;; removes all items that satisfy the predicate
(remove-if #'numberp '(2 for 1 sale))
(remove-if #'oddp '(1 2 3 4 5 6 7 8))
(remove-if #'(lambda (x) (not (plusp x))) '(2 0 -4 3 -4 7 6))
;; remove-if-not
;; returns a list of all itmes that satisfy the predicate
(remove-if-not #'plusp '(2 0 4 -5 -3))
(remove-if-not #'oddp '(2 0 -4 6 8 10))
(remove-if #'(lambda (x)
               (if
                (and
                 (< x 6)
                 (> x 0)) t nil)) '(0 1 2 3 4 5 3 6 7 9 -1 2))
(defun count-the (list)
  (length (remove-if-not #'(lambda (x)
                     (equal x 'the)) list)))
(defun pick-pairs (pairs)
  (remove-if-not #'(lambda (x)
                     (eq 2 (length x))) pairs))


(defun rank (list)
  (car list))
(defun suit (list)
  (car (cdr list)))
(setf my-hand '((3 hearts)
                (5 clubs)
                (2 diamonds)
                (4 diamonds)
                (ace spades)))
(defun count-suit (c-suit hand)
  (length (remove-if-not #'(lambda (x) (equal (suit x) c-suit)) hand)))
(setf colors '((clubs black)
               (diamonds red)
               (hearts red)
               (spades black)))
(defun color-of (card)
  (car (cdr (assoc (suit card) colors))))

(defun first-red (hand)
  (find-if #'(lambda (x) (equal 'red (color-of x))) hand))
(defun black-cards (hand)
  (remove-if #'(lambda (x) (equal 'black (color-of x))) hand))

(defun what-ranks (card-color hand)
  (mapcar #'rank (remove-if-not #'(lambda (x) (equal (suit x) card-color)) hand)))



;; words
(setf spanish '(uno dos tres quatro cinco))
(mapcar #'(lambda (x y) (append x (list y))) words spanish)
;; as ' is shorthand for quote special function
;; #' is shorthand for function special function
;; #'cons vs 'cons
;; #'(lambda (x) (+ x 2))
(setf g #'(lambda (x) (* x 10)))
(funcall g 12)
;; applicative ops accept keywords
(find-if #'oddp '(2 3 4 5 6) :from-end t)
(reduce #'cons '(a b c d e))
;;vs
(reduce #'cons '(a b c d e)  :from-end t)


(defun my-assoc (key table)
  (find-if #'(lambda (entry) (equal key (first entry)))
           table))
(my-assoc 'two words)

;; writing an applicative op
(defun inalienable-rights (fn)
  (funcall fn
           '(life liberty and the pursuit of happiness)))
(inalienable-rights #'length)
(inalienable-rights #'reverse)
(inalienable-rights #'(lambda (x) (cons 'high x)))

;; write function whose val is another functions
(defun make-greater-than-predicate (n)
  #'(lambda (x) (> x n)))
;; value returned by it is a lexical closure

(setf pred (make-greater-than-predicate 3))
