
;; Image size
(defvar image-width)
(defvar image-height)
(setf image-width 256)
(setf image-height 256)
;; render
;;(defvar image-line (make-array image-height))
;;(defun render-line (array-line)
;;  (do* ((aref-red 0 (+ aref-red 3))
;;        (aref-green 1 (+ aref-green 3))
;;        (aref-blue 2 (+ aref-blue 3)))
;;       ((< image-width aref-blue) nil)
;;  (setf (aref array-line aref-red) aref-red)
;;  (setf (aref array-line aref-green) aref-green)
;;  (setf (aref array-line aref-blue) aref-blue)))
;;
;;
;;
;;(defun render-image (out)
;;  (format out "~S~&" 'P3)
;;  (format out " ~S" image-width)
;;  (format out " ~S" image-height)
;;  (format out "~&~S~&" 255)
;;  (do* ((index_j image-height (- index_j 1)))
;;       ((<= index_j 0) nil)
;;    (do* ((index_i 0 (+ index_i 1))
;;          (aref_red 0 index_i)
;;          (aref_green 0 index_j)
;;          (aref_blue 64 64))
;;         ((> index_i image-width) nil)
;;      (format out "~&~S ~S ~S" aref_red aref_green aref_blue))))



(defvar image-line (make-array (list image-width image-height)))
(setf image-line (make-array (list image-width image-height)))
;; create data for ppm file
;;(defun render-image (out)
;;  (format out "~S~&" 'P3)
;;  (format out "~S" image-width)
;;  (format out " ~S" image-height)
;;  (format out "~&~S~&" 255)
;;  (do* ((index-j image-height (- index-j 1)))
;;       ((< index-j 0) nil)
;;    (do* ((index-i 0 (+ index-i 1))
;;          (aref-red 54 index-i)
;;          (aref-green 0 index-j)
;;          (aref-blue 64 64))
;;         ((>= index-i image-width) nil)
;;      (format out "~&~S ~S ~S" aref-red aref-green aref-blue))))


(defun render-image (out)
  (format out "~S~&" 'P3)
  (format out "~S" image-width)
  (format out " ~S" image-height)
  (format out "~&~S~&" 255)
  (do* ((index-j image-height (- index-j 1)))
       ((< index-j 0) nil)
       (format t "~% Scanlines remaining: ~S" index-j)
    (do* ((index-i 0 (+ index-i 1))
          (aref-red 1 (/ index-i image-width))
          (aref-green 1 (/ index-j image-height))
          (aref-blue (/ 1 4))
          (int-red 0 (truncate (* 255.999 aref-red)))
          (int-green 0 (truncate (* 255.999 aref-green)))
          (int-blue 0 (truncate (* 255.999 aref-blue))))
         ((>= index-i image-width) nil)
    (format out "~&~S ~S ~S" int-red int-green int-blue))))
;; create ppm file
;;
(defun write-data ()
  (with-open-file (output #p"image.ppm"
                          :direction :output
                          :if-exists :overwrite)
    (render-image output)))


;; a structure to hold a vector, with x y z
(defstruct vec
  (x 0)
  (y 0)
  (z 0))
;; Helper functions, operations on vectors
(defun vec* (n V)
  "Multiplication for x y z, of vector V, by n"
  (make-vec
   :x (* (vec-x V) n)
   :y (* (vec-y V) n)
   :z (* (vec-z V) n)))
(defun vec+ (n V)
  "Addition for x y z, of vector V, by n"
  (make-vec
   :x (+ (vec-x V) n)
   :y (+ (vec-y V) n)
   :z (+ (vec-z V) n)))
(defun vec/ (n V)
  "Division for x y z, of vector V, by n"
  (make-vec
   :x (/ (vec-x V) n)
   :y (/ (vec-y V) n)
   :z (/ (vec-z V) n)))
(defun vec- (n V)
  "minus for x y z, of vector V, by n"
  (make-vec
   :x (- (vec-x V) n)
   :y (- (vec-y V) n)
   :z (- (vec-z V) n)))
(defun square (x)
  (* x x))
(defun vec-length (V)
  "Length of a vector V"
  (sqrt
   (+
    (square (vec-x V))
    (square (vec-y V))
    (square (vec-z V)))))

;; print out X Y Z
(defun vec-stream (stream V)
  "print V to stream"
  (format stream "~S ~S ~S"
          (vec-x V)
          (vec-y V)
          (vec-z V)))
(defun vec-dot (V Y)
  "dot product of two vectors V and Y"
  (+
   (* (vec-x V) (vec-x Y))
   (* (vec-y V) (vec-y Y))
   (* (vec-z V) (vec-z Y))))
(defun vec-cross (V Y)
  "cross product of two vectors V and Y"
  (make-vec
   :x (- (* (vec-y V) (vec-z Y)) (* (vec-z V) (vec-y Y)))
   :y (- (* (vec-z V) (vec-x Y)) (* (vec-x V) (vec-z Y)))
   :z (- (* (vec-x V) (vec-y Y)) (* (vec-y V) (vec-x Y)))))
(defun vec-minusvec (V Y)
  "Subtract two vectors V and Y"
  (make-vec
   :x (- (vec-x V) (vec-x Y))
   :y (- (vec-y V) (vec-y Y))
   :z (- (vec-z V) (vec-z Y))))
(defun vec-addvec (V Y)
  "add two vectors V and Y"
  (make-vec
   :x (+ (vec-x V) (vec-x Y))
   :y (+ (vec-y V) (vec-y Y))
   :z (+ (vec-z V) (vec-z Y))))
;; Actual ray stuff
(defstruct ray
  (:origin (make-vec))
  (:direction (make-vec)))
(defun raypoint (ray-n tp)
  "Point on ray with parameter t (tp)"
  (vec-addvec (ray-origin ray-n) (vec-* tp (ray-direction ray-n))))
;; Camera - A point of origin for rays?
;; Scene - ???
;;         Objects: Check math, how plane is defined.
;; A procedure: Send ray, iteratively, or recursively,
;;              if it gets to certain t, return color X
;;              If it impacts an object - returns color Y


;; Camera
(defvar *camera* (make-vec :x 0 :y 0 :z 0))
