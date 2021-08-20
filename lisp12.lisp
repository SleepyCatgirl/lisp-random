;; typep
;; returns t if arg is of said type
(typep 3 'integer)
(typep 3 'float)
(typep 3 'number)
(typep 'foo 'symbol)

(type-of nil)
(type-of 5.0)
(type-of 5.0000)
(type-of 5.35235425435435443444444444444444444444444444444)
(type-of (/ 3 5))
(type-of '(bad ba))
(type-of "NYAA")


;; structures
;; objects with arbitrary number of components
;;
(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))
(defvar s1)
(setf s1 (make-starship))
(defvar s2)
(setf s2 '#s(starship speed (warp 3)
                      condition red
                      shields up))
(starship-p s2)
(typep s1 'starship)
;;accessor functions
(starship-name s1)
(starship-condition s1)
(starship-speed s2)

(setf (starship-name s2) "Enterprise")

;;(defun alert (x)
;;  (setf (starship-shields x) 'up)
;;  (if (equal (starship-condition x) 'green)
;;      (setf (starship-condition x) 'yellow))
;;  'shields-raised)
(defun alert (starship)
  (setf (starship-shields starship) 'up)
  (if (equal (starship-condition starship) 'green)
      (setf (starship-condition starship) 'yellow))
  'shields-raised)


(defvar s3)
(setf s3 (make-starship :name "Reliand"
                        :shields 'damaged))

(defstruct starship
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))
(setf s3 (make-starship :captain "Benson"
                        :name "Reliand"
                        :shields 'damaged))



;;print func for structures
;; convention is to print structure descriptions with #< and>
;; for example
;; #<STARSHIP ENTERPSIE>
(defun print-starship (x stream depth)
  (format stream "#<STARSHIP ~A>"
          (starship-name x)))

;; to make lisp call this func, whenever it tries to print a starship,
(defstruct (starship
            (:print-function print-starship))
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))

(defvar s4)
(setf s4 (make-starship :name "RELIAND"))
(starship-shields s4)



(defstruct (captain
            (:print-function print-captain))
  (name nil)
  (age 0)
  (ship nil))
(defvar c1)
(setf c1 (make-captain
          :name "James T. Kirk"
          :age 35
          :ship ship))
(defun print-captain (x stream depth)
  (format stream "#<CAPTAIN ~A>"
          (captain-name x)))
(defvar ship)
(setf ship (make-starship
            :captain c1
            :name "Enterprise"))
