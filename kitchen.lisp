(setf rooms '((living-room
               (north front-stairs)
               (south dining-room)
               (east kitchen))
              (upstairs-bedroom
               (west library)
               (south front-stairs))
              (dining-room
               (north living-room)
               (east pantry)
               (west downstairs-bedroom))
              (kitchen
               (west living-room)
               (south pantry))
              (pantry
               (north kitchen)
               (east dining-room))
              (downstairs-bedroom
               (north back-stairs)
               (east dining-room))
              (back-stairs
               (south downstairs-bedroom)
               (north library))
              (front-stairs
               (north upstairs-bedroom)
               (south living-room))
              (library
               (east upstairs-bedroom)
               (south back-stairs))))
(defun choices (room)
  (rest (assoc room rooms)))
(defun look (dir room)
  (cdr (assoc dir (choices room))))
(defun set-robbie-location (place)
  "Moves robbie to PLACE by setting the var LOC"
  (setf loc place))
(defun how-many-choices ()
  (length (choices loc)))
(defun upstairp (location)
  (if
   (or
    (equal location 'library)
    (equal location 'upstairs-bedroom))
   t nil))
(defun onstairsp (location)
  (if
   (or
    (equal location 'front-stairs)
    (equal location 'back-stairs))
    t nil))
(defun where ()
  (cond ((onstairsp loc) (append '(ROBBIE IS ON THE) (list loc)))
        ((upstairp loc) (append '(ROBBIE IS UPSTAIRRS IN THE) (list loc)))
        (t (append '(ROBBIE IS DOWNSTAIRS IN THE) (list loc)))))
(defun move (direction)
  (cond ((look direction loc)
         (and (set-robbie-location (car (look direction loc)))
              (where)))
        (t (list 'OUCH))))
 ;; (defun move (dir)
 ;; (let ((new-loc (look dir loc)))
 ;; (cond ((null new-loc)
 ;; '(ouch))
 ;; (t (set-robbie-location new-loc)
 ;; (where)))))
