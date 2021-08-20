(defstruct ship
  (name nil)
  (captain nil)
  (crew-size nil))
(defstruct (starship (:include ship))
  (weapons nil)
  (shields nil))
(defstruct (supply-ship (:include ship))
  (cargo nil))

(defvar z1)
(setf z1 (make-starship
          :captain "James Kirk"))
(defvar z2)
(setf z2 (make-supply-ship
          :captain "Harry Madd"))
(ship-p z1)
(starship-p z1)
(ship-captain z1)
(starship-captain z1)
