;; when writing complex program,
;; pay a bit of time to think about overall design,
;; especially what data structures
;;
;;lets start with developing a representation for the board
;; 1 | 2 | 3
;; ---------
;; 4 | 5 | 6
;; ---------
;; 7 | 8 | 9
;; we will represent a board as a list consisting of the symbol
;; board followed by nine numbers
;; that describe the contents.
;; a 0 means pos is empty
;; 1 means it is filled by an )
;; 10 means it is filled by X
(defun make-board ()
  (list 'board
        0 0 0
        0 0 0
        0 0 0
        ))
(defvar *b*)
(setf *b* (make-board))
;; Notice if B is var holding board, positiong one can be accesed
;; by (nth 1 *b*)
;; (nth 0 *b*) return symbol board
;; now lets write functions to print board
;; convert-to-letter converts nums to O/X/Nothing
;;
;;Print-row prints out on row of the board
;;priont-board prints whole using print-row
(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  (format t "~&  ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))
;; we can make move by destructively changing one of board positions
;; from 0 to 1 or 10
;; var player in make-move will be either one or ten,
;; depending on who is moving
(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

;; lets make test
(defvar *computer*)
(defvar *opponent*)
(setf *computer* 10)
(setf *opponent* 1)


;; ways to win,
(defvar *triplets*)
(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)   ;Horizontal triplets
        (1 4 7) (2 5 8) (3 6 9)   ;Vertical triplets
        (1 5 9) (3 5 7)))         ;Diagonal triplets

;; Sum triplet
;; return the sum of numbers in the board position specified by that triplet
;; Sum eleven would indicate, that there is one blank, one O and one X
(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))
;; compute ALL sums
(defun compute-sums (board)
  (mapcar #'(lambda (triplet) (sum-triplet board triplet)) *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

;; now basic framework to play game
;;
(defun play-one-game ()
  (if (y-or-n-p "would you like to go first?")
      (opponent-move (make-board))
      (computer-move (make-board))))
(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You win"))
          ((board-full-p new-board)
           (format t "~&Tie"))
          (t (computer-move new-board)))))
(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t
                   "~&That space is occupied")
           (read-a-legal-move board))
          (t pos))))
(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~S" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie"))
          (t (opponent-move new-board)))))

;;(defun choose-best-move (board)
;;  (random-move-strategy board))
(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "Random move"))
(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))



(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board
                           (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
                           (* 2 *opponent*))))
    (and pos (list pos "block"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                  #'(lambda (trip)
                      (equal (sum-triplet board trip)
                             target-sum))
                  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))
(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))


(defun choose-best-move (board) ;Second version.
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))



;;review exercises
;;(defun ugly (x y)
;;  (when (> x y)
;;    (setf temp y)
;;    (setf y x)
;;    (setf x temp))
;;  (setf avg (/ (+ x y) 2.0))
;;  (setf pct (* 100 (/ avg y)))
;;  (list 'average avg 'is pct 'percet 'of 'max y))

(defun perc (x y)
  (let* ((avg (/ (+ x y) 2.0))
         (pct (* 100 (/ avg (max x y)))))
    (list 'average avg 'is pct 'of 'max (max x y))))
(defvar x nil)
(push x x)


(defvar *corners*)
(setf *corners* '(1 3 7 9))
(defvar *sides*)
(setf *sides* '(2 4 6 8))


(defun sq (board target-sum)
    (let ((triplet (find-if
                  #'(lambda (trip)
                      (equal (sum-triplet board trip)
                             target-sum))
                  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))
