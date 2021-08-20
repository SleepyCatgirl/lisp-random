;; recursion
(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))
(defun anyoddp-if (x)
  (if
   (null x)
   nil
   (if
    (oddp (first x))
    t
    (anyoddp-if (rest x)))))

(defun factorial (x)
  (cond ((equal x 0) 1)
        (t (* x (factorial (- x 1))))))
(defun factorial-ii (x)
  (cond ((zerop x) 1)
        (t (* x (factorial-ii (- x 1))))))
(defun count-slices (loaf)
  (cond ((null loaf) 0)
        (t (+ 1 (count-slices (rest loaf))))))



(defun laugh (n)
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))

(defun add-up (list-num)
  (cond ((null list-num) 0)
        (t (+ (first list-num) (add-up (rest list-num))))))
(defun alloddp (list-num)
  (cond
    ((null list-num) t)
    ((oddp (first list-num)) (alloddp (rest list-num)))
    (t nil)))
(defun alloddp-2 (x)
  (cond ((null x) t)
        ((evenp (first x)) nil)
        (t (alloddp-2 (rest x)))))

(defun rec-member (word x)
  (cond ((null x) nil)
        ((equal word (car x)) x)
        (t (rec-member word (rest x)))))
(defun rec-assoc (key table)
  (cond ((null table) nil)
        ((equal key (car (first table))) (car table))
        (t (rec-assoc key (rest table)))))

(defun rec-nth (n list)
  (cond ((null list) nil)
        ((zerop (- n 1)) (car list))
        (t (rec-nth (- n 1) (cdr list)))))
(defun add1 (x)
  (+ x 1))
(defun sub1 (x)
  (- x 1))
(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

(defun fib (x)
  (cond ((equal x 0) 1)
        ((equal x 1) 1)
        (t (+ (fib (- x 1)) (fib (- x 2))))))
(defun inf-f ()
  (inf-f))

;; rec templates
;;
;; double-test tail recursion
;; rec function has two end tests
;; if either is true, return value
;; if neither is true, proceed with recursion
;; (defun func (x)
;;      (cond (end-test-1 end-value-1)
;;            (end-test-2 end-value-2)
;;            (t (func reduced-x))))
;;
;; examples
;; anyoddp,
;; tail recursive, because:
;; action part of last cond clause does not do any work
;; after recursive call.
;;
;;
;;
;; what if we change 1 and 2 clause in anyoddp
(defun anyoddp (x)
  (cond
    ((oddp (first x)) t)
    ((null x) nil)
    (t (anyoddp (rest x)))))
(defun first-anyoddp (x)
  (cond
    ((null x) nil)
    ((oddp (first x)) (first x))
    (t (first-anyoddp (rest x)))))

;; single test tail recursion
;; if we want to find the first atom in a list,
;; where the list may be nested arbitrarily deeply
;; find first atom does it

(defun last-element (x)
  (cond ((equal (cdr x) nil) x)
        (t (last-element (cdr x)))))
(defun last-element-atom (x)
  (cond ((atom (cdr x)) (car x))
        (t (last-element-atom (cdr x)))))

;; example
;; find ifrst atom
(defun find-first-atom (x)
  (cond ((atom x) x)
        (t (find-first-atom (first x)))))

(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (+ n (add-nums (- n 1))))))
(defun all-equal (num)
  "returns T if first element is equal to second and second to third
   and so on on
   (all-equal '(I I I I)) returns true
   (all-equal '(I I I II)) returns nil"
  (cond ((< (length num) 2) t)
        ((equal (first num) (second num)) (all-equal (rest num)))
        (t nil)))
(defun all-equal-2 (x)
  (cond ((null (rest x)) t)
        ((not (equal (first x) (second x))) nil)
        (t (all-equal-2 (rest x)))))

(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))
(defun appl-fact (n)
  (reduce #'* (count-down n)))


(defun count-down-z (n)
  (cond ((zerop n) '(0))
        (t (cons n (count-down-z (- n 1))))))
(defun count-down-z (n)
  (cond ((zerop (+ n 1)) nil)
        (t (cons n (count-down-z (- n 1))))))

(defun square-list (n)
  (cond ((null n) nil)
        (t (cons (* (car n) (car n)) (square-list (rest n))))))

(defun my-nth (n x)
  (cond ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))
(defun my-nth (n x)
  (cond ((null x) nil)
        ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))
(defun my-member (input list)
  (cond ((null list) nil)
        ((equal (car list) input) list)
        (t (my-member input (rest list)))))
(defun my-assoc (key list)
  (cond ((null list) nil)
        ((equal key (first (first list))) (first key))
        (t (my-assoc key (rest list)))))
(defun which-longer (l1 l2)
  (cond ((and (null l1) (null l2)) 'same-length)
        ((null l1) 'first-shorter)
        ((null l2) 'second-shorter)
        (t (which-longer (rest l1) (rest l2)))))

(defun extract-symbols (x)
  (cond ((null x) nil)
        ((symbolp (first x)) (cons (first x) (extract-symbols (rest x))))
        (t (extract-symbols (rest x)))))

(defun sum-numeric-elm (x)
  (cond ((null x) 0)
        ((numberp (car x)) (+ (car x) (sum-numeric-elm (rest x))))
        (t (sum-numeric-elm (rest x)))))
(defun my-remove (element list)
  (cond ((null list) nil)
        ((equal element (car list)) (my-remove element (cdr list)))
        (t (cons (car list) (my-remove element (cdr list))))))

(defun my-intersection (l1 l2)
  (cond ((or (null l1) (null l2)) nil)
        ((member (car l1) l2)
         (cons (car l1) (my-intersection (cdr l1) l2)))
        ((member (car l2) l1)
         (cons (car l2) (my-intersection l1 (cdr l2))))
        (t (my-intersection (cdr l1) (cdr l2)))))
(defun my-intersec (l1 l2)
  (cond ((null l1) nil)
        ((member (first l1) l2)
         (cons (first l1)
               (my-intersec (rest l1) l2)))
        (t (my-intersec (rest l1) l2))))
(defun my-set-diff (l1 l2)
  (cond ((null l1) nil)
        ((not (member (first l1) l2))
         (cons (first l1)
               (my-set-diff (rest l1) l2)))
        ((not (member (first l2) l1))
         (cons (first l2)
               (my-set-diff l1 (rest l2))))
        (t (my-set-diff (rest l1) l2))))
(defun count-odd (x)
  (cond ((null x) 0)
        ((oddp (first x)) (+ 1 (count-odd (rest x))))
        (t (count-odd (rest x)))))


(defun combine (x y)
  (+ x y))

(defun fib (x)
  (cond ((equal x 0) 1)
        ((equal x 1) 1)
        (t (combine (fib (- x 1)) (fib (- x 2))))))
;; nested lists
(defun find-number (x)
  (cond ((numberp x) x)
        ((atom x) nil)
        (t (or (find-number (car x))
               (find-number (cdr x))))))

(defun atoms-to-q (x)
  (cond ((null x) nil)
        ((atom x) 'q)
        (t (cons (atoms-to-q (car x)) (atoms-to-q (cdr x))))))
(defun count-atoms (x)
  (cond ((null x) 1)
        ((atom x) 1)
        (t (+ (count-atoms (car x)) (count-atoms (cdr x))))))
(defun count-cons (x)
  (cond ((atom x) 0)
        (t (+ 1 (count-cons (car x)) (count-cons (cdr x))))))

(defun sum-tree (x)
  (cond ((numberp x) x)
        ((atom x) 0)
        (t (+ (sum-tree (car x)) (sum-tree (cdr x))))))

(defun my-subst (new old tree)
  (cond ((null tree) nil)
        ((equal tree old) new)
        ((atom tree) tree)
        (t (cons (my-subst new old (car tree)) (my-subst new old (cdr tree))))))
(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (flatten (car tree)) (flatten (cdr tree))))))
(defun paren-depth (tree)
  (cond ((null tree) 0)
        ((atom tree) 0)
        (t (max (+ 1 (paren-depth (first tree))) (paren-depth (rest tree))))))


(defun count-up-recursively (cnt n)
  (cond ((> cnt n) nil)
        (t (cons cnt (count-up-recursively (+ cnt 1) n)))))
(defun count-up (n)
  (count-up-recursively 1 n))

(defun count-up (n)
  (cond ((zerop n) nil)
        (t (append (count-up (- n 1)) (list n)))))

(defun make-loaf (n)
  (if (zerop n) nil (cons 'x (make-loaf (- n 1)))))

(defun bury (word n)
  (cond ((zerop n) word)
        (t (list (bury word (- n 1))))))

(defun pairings (l1 l2)
  (cond ((or (null l1) (null l2)) nil)
        (t (cons (list (car l1) (car l2)) (pairings (cdr l1) (cdr l2))))))

(defun sublists (l1)
  (cond ((null l1) l1)
        ((atom l1) l1)
        (t (list (cons l1 (sublists (cdr l1)))))))
;;(defun my-reverse (cnt l1)
;;  (cond ((zerop cnt) nil)
;;        (t (cons (my-reverse (- cnt 1) (cdr l1)) (car l1)))))
;;(defun my-rev (li1)
;;  (my-reverse ((length li1) li1)))
(defun my-union (l1 l2)
  (cond ((and (null l1) (null l2)) nil)
        ((member (car l1) l2) nil)
        (t (append (list (car l1)) (list (car l2)) (my-union (cdr l1) (cdr l2))))))
(defun my-union (l1 l2)
  (append l1 (union-rec l1 l2)))
(defun union-rec (l1 l2)
  (cond ((null l2) nil)
        ((member (first l2) l1) (union-rec l1 (rest l2)))
        (t (cons (first l2) (union-rec l1 (rest l2))))))



(defun largest-even (num)
  (cond ((null num) 0)
        ((oddp (car num)) (largest-even (cdr num)))
        (t (max (car num) (largest-even (cdr num))))))
