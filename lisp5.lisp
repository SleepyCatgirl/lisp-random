(defun price-change (old new)
  (let* ((diff (- new old))
         (prop (/ diff old))
         (perc (* prop 100.0)))
    (list 'widgets 'changed 'by perc 'percent)))
;; side effects and bugs
(defun coin-with-bug ()
  (cond ((< (random 101) 50) 'heads)
        ((> (random 101) 50) 'tails)
        ((equal (random 101) 50) 'edge)))
;; To fix, lets use let
(defun coin-fix ()
  (let ((toss (random 101)))
    (cond ((< toss 50) 'heads)
          ((> toss 50) 'tails)
          ((equal toss 50) 'edge))))


(defun throw-die ()
  "Function that will generate random number from 1 to 6"
  (let ((r (random 7)))
    (cond ((equal r 0) (throw-die))
        (t r))))
(defun throw-dice ()
  "Function that generates a list of two numbers
    Generally, it represents two dice throws,
    e.g (5 2) means first throw was 5 and second 2"
  (list (throw-die) (throw-die)))
(defun boxcars-p (l)
  "Chceks if the pair of dice rolls is a boxcars (6 6)"
  (if (equal l '(6 6)) t nil))
(defun snake-eyes-p (l)
  "Chceks if the pair of dice rolls is a snakeeyes (1 1)"
  (if (equal l '(1 1)) t nil))
(defun instant-win-p (l)
  "Checks for instant win (7 or 11)"
  (cond ((equal 7 (+ (car l) (car (cdr l)))) t)
        ((equal 11 (+ (car l) (car (cdr l)))) t)
        (t nil)))

(defun instant-loss-p (l)
  "Checks for instant loss (2 3 or 12)"
  (cond ((equal 2 (+ (car l) (car (cdr l)))) t)
        ((equal 3 (+ (car l) (car (cdr l)))) t)
        ((equal 12 (+ (car l) (car (cdr l)))) t)
        (t nil)))
(defun say-throw (l)
  "describes the throw"
  (cond ((boxcars-p l) 'boxcars)
        ((snake-eyes-p l) 'snake-eyes)
        (t (+ (car l) (car (cdr l))))))
(defun craps ()
  "a single roung of craps"
  (let ((toss (throw-dice)))
    (append (list 'THROW (car toss) 'and (car (cdr toss)) '-- (say-throw toss) '--)
            (cond ((instant-win-p toss) '(you win))
                  ((instant-loss-p toss) '(you lose))
                  (t (list 'your 'point 'is (+ (car toss) (car (cdr toss)))))))))
(defun val (throw)
  (+ (car throw) (car (cdr throw))))
(defun try-for-point (point)
  "a single roung of craps"
  (let ((toss (throw-dice)))
    (append (list 'THROW (car toss) 'and (car (cdr toss)) '-- (val toss) '--)
            (cond ((equal (val toss) 7) '(you lose))
                  ((equal (val toss) point) '(you win))
                  (t '(try again))))))
