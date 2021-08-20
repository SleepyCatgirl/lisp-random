;; absolute value
(defun my-abs (x)
  (if (< x 0) (- x) x))
;; symboltest
(defun symbol-test (x)
  (if (symbolp x) (list 'yes x 'is 'a 'symbol)
      (list 'no x 'is 'not 'a 'symbol)))
;; 4.1 exercise and later
(defun make-even (x)
  (if (evenp x) x (+ 1 x)))
(defun further (x)
  (if (< x 0) (- x 1) (+ 1 x)))
;; my not
(defun my-not (x)
  (if (equal x T) nil t))
(defun my-not2 (x)
  (if x nil t))

(defun oredered (x y)
  (if (< x y) (list x y) (list y x)))
;; cond
;; (cond (test-1 consequent-1)
;;       (test-2 consequent-2)
;;       ....
;;       (test-n consequent-n)
(defun compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
        ((< x y) 'first-is-smaller)
        ((> x y) 'first-is-bigger)))
;; (T consqeuent) - always will eval
(defun where-is (x)
  (cond ((equal x 'paris) 'france)
        ((equal x 'london) 'england)
        ((equal x 'beijing) 'china)
        (t 'unknown)))
;; 4.6
(defun my-abs-cond (x)
  (cond ((< x 0) (- x))
        (t x)))
