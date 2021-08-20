;; Strings: Vectors of characters
;; different htan lists a bit
;; they eval to themselves
;; "Random strig"
;;
;;
;; Stringp returns t if argument is string
;; nil otherwise
(setf a "This object is a string")
(stringp a)
(setf b 'this-object-is-symbol)
(stringp b)
;; format returns nil
;; but as side effect causes things to be written on the display or file
;; The first argument to
;; FORMAT should be the symbol T when we want to write to the display.
;; (Different values are used when writing to a disk file.)
;; second arg must be string, called format control string
(format t "Nyaa hi")




;; special formatting directives
;; begin with ~
;; ~% causes format to move ot new line
(format t "Sleepy day~%Nyan")
(format t "Sleepy day~%~%Nyan")
;; ~& moves to new line, unless its already new line
(format t "Sleepy day~&~&~&~&nyan")
;;
;; ~S
;; which inserts the printed
;; representation of a Lisp object into the message that FORMAT prints. (The S
;; stands for ‘‘S-expression,’’ or ‘‘symbolic expression,’’ a somewhat archaic
;; term for a Lisp object.) For each occurrence of ~S in the format control string,
;; FORMAT requires one extra argument. In the following example, the first ~S
;; is replaced by the symbol BOSTON, the second ~S is replaced by the list
;; (NEW YORK), and the third ~S is replaced by the number 55.
(format t "Sleepy ~S of ~S ~S" 'nya 'silly 'all)


(defun square-talk (n)
  (format t "~&~S squared is ~S" n (* n n)))
(mapcar #'square-talk '(1 2 3 4 5))


(defun display ()
  (format t "There are old pilots,~%and there are bold pilots,~%but there are no old bold pilots"))


(defun draw-line (n)
  (cond ((zerop n) (format t "~%"))
        (t (format t "*") (draw-line (- n 1)))))

(defun draw-box (x y)
  (cond ((zerop y) nil)
        ((zerop x) nil)
        (t (draw-line x) (draw-box x (- y 1)))))


(defun n-beers (n)
  (cond ((zerop n) (format t "~%NULL"))
        (t (format t "~S bottles of beer on the wall,~%~S bottles of beer!~%Take one down,~%Pass it around,~%~S bottles of beer on the wall,~%~%" n n (- n 1))
           (n-beers (- n 1)))))


(defun do-verse (n)
  (format t
          "~&~S bottles of beer on the wall,~%" n)
  (format t "~S bottles of beer!~%" n)
  (format t "Take one down,~%Pass it around,~%")
  (format t
          "~S bottles of beer on the wall.~%~%"
          (- n 1)))


(defun print-board (x1 x2 x3 y1 y2 y3 z1 z2 z3)
  (format t " ~S | ~S | ~S " x1 x2 x3)
  (format t "~%-------------~%")
  (format t " ~S | ~S | ~S " y1 y2 y3)
  (format t "~%-------------~%")
  (format t " ~S | ~S | ~S " z1 z2 z3))


(defun print-board-helper (list)
  (cond ((null list) (format t "|"))
        ((equal (mod (length list) 3) 1) (format t "| ~S ~%-------------~%" (car list)) (print-board-helper (cdr list)))
        ((equal (car list) nil) (format t "|   ") (print-board-helper (cdr list)))
        ((equal (car list) 'x) (format t  "| X ") (print-board-helper (cdr list)))
        ((equal (car list) 'o) (format t  "| O ") (print-board-helper (cdr list)))))

;; I give up
;; book solution
(defun print-board (b)
  (let ((b2 (sublis '((x . "X")
                      (o . "O")
                      (nil . " "))
                    b)))
    (format t "~&")
    (print-line b2)
    (format t "-------------~%")
    (print-line (nthcdr 3 b2))
    (format t "-------------~%")
    (print-line (nthcdr 6 b2))))
(defun print-line (line)
  (format t " ~A | ~A | ~A~%"
          (first line)
          (second line)
          (third line)))
;; read
;; reads one lisp object (number, symbol, list, whatever)
;; from kb and returns it as its value
(defun my-square ()
  (format t "Please type in a number: ")
  (let ((x (read)))
    (format t "The number ~S squared is ~S.~%" x (* x x))))

(defun gross-pay ()
  (format t "Please type in hourly wage: ")
  (let ((x (read)))
    (format t "~%Please type in worked hours: ")
    (let ((y (read)))
      (format t "~%Gross pay is: ~S" (* x y)))))

(defun cookie-monster ()
  (format t "GIVE ME COOKIE~%")
  (let ((x (read)))
    (cond ((equal x 'cookie) (format t "Cookie? ~S~%THANK YOU" x))
          (t (format t "Cookie? ~S~%NO WANT ~S~%" x x) (cookie-monster)))))



;;yes-or-no-p
;; ask for yes or no, resulting in either t or nil
(defun riddle ()
  (if (yes-or-no-p "Do you seek Zen?")
      (format t "Then do not ask for it!")
      (format t "you have found it")))
;; y-or-n-p is shorter. asks for y or n
;
;
;
;
;; reading files
;; via with-open-file macro
;; creates local variable like let and sets it to stream object
;; stream objects are special datatype, describes connection to files
;; example is
;; *TERMINAL-IO*
;; holds the stream object lisp uses to read from kb and write to display



;; suppose there is file timber.dat
;; "The North Slope"
;;((45 redwood) (12 oak) (43 maple))
;;100
(defun get-tree-data ()
  (with-open-file (stream "./timber.dat")
    (let* ((tree-loc (read stream))
           (tree-tables (read stream))
           (num-trees (read stream)))
      (format t "~&There are ~S Trees on ~S"
              num-trees tree-loc)
      (format t "~&They are: ~S" tree-tables))))


;; We can also open files for output by passing it keyword :DIRECTION OUTPUT
;; stream can then be used in place of t in format
(defun save-tree-data (tree-loc tree-table num-trees)
  (with-open-file (stream "./timber.dat.bak" :direction :output)
    (format stream "~S~&" tree-loc)
    (format stream "~S~&" tree-table)
    (format stream "~S~&" num-trees)))
