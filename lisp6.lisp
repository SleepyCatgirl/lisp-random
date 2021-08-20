(defun power (n m)
  (cond
    ((= m 0) 1)
    ((evenp m)
     (let ((m/2 (/ m 2)))
     (power (* n n) m/2)))
    (t (* n (power n (- m 1))))))


;; lists, convenienice, and how it misleads
;; (cons '(a b c) 'd)
;; ((a b c) . d)
;; dot notation, because it ends in d, not nil like most lists
;; append
(append '(friends romans) '(and countrymen))
(append '(april showers) nil)
(append nil '(bring may shower))
(append nil nil)
(append '((a 1) (b 2)) '((c 3) (d 4)))
;; doesnt change value of any variable
;; it is a nondestructive function
(setf who '(only the good))
(append who '(die young))
(defun add-to-end (x e)
  "ADD ELEMENT E TO THE END OF LIST X"
  (append x (list e)))

(reverse '(one two three four five))
(reverse '(l i v e))
;; like append, it is non-destructive
(defun add-to-end-2 (x y)
  (reverse (cons y (reverse x))))
;; nth and nthcdr
(nthcdr 0 '(a b c d))
(nthcdr 3 '(a b c d e f))

(defun nth (n x)
  "Return the n-th element of the list X, counting from 0"
  (car (nthcdr n x)))
;; last
(last '(a b c d e))
(last nil)
(last '(a b c .d))
(last '(ROSE))
(last '((A B C)))
;; remove
(remove 'a '(b a n a n a))
;; nondestructive
;; copies parts of list
;;(setf spell '(a b r a c a d a b r a))
;;(remove 'a spell)
;; spell
(defun lastelement (list)
  (first (last list)))
(defun lastelement-rev (list)
  (first (reverse list)))
(defun last-element-nth (list)
  (nth (- (length list) 1) list))
(defun next-to-last (list)
  (second (reverse list)))
(defun next-to-last-nth (list)
  (nth 1 (reverse list)))
(defun my-but-last (list)
  (reverse (cdr (reverse list))))
(defun palindrome-p (list)
;;  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal list (reverse list)) t)
        (t nil)))
(defun make-palindrome (list)
  (append list (reverse list)))
;; set,
;; unordered collection of items
;; each item appears once
;; set can be days of the week, set of integers (Infinite set)
;; and set of people in Hackensack, who had spaghetti last ngiht

;; A bit of work done on laptop


;; set difference
(set-difference '(alpha bravo charlie delta)
                '(bravo charlie))
(setf line1 '(all things in moderation))
(setf line2 '(moderation in the defense of liberty is no virtue))
(set-difference line1 line2)
(set-difference line2 line1)
(set-difference nil '(a))
(set-difference '(a b c) nil)

;;subsetp
(subsetp '(a i) '(a e i o u))
(subsetp '(a x) '(a e i o u))
(defun my-substep (list1 list2)
  (cond ((equal (set-difference list1 list2) nil) t)
        (t nil)))
(setf a '(soap water))
(union a '(no soap radio))
(intersection a (reverse a))
(member 'water a)
(member 'soap a)
(set-difference '(a c b) '(a b c))
;;(defun set-equal (list1 list2)
;;  (cond ((equal (set-difference list1 list2) nil) t)
;;        (t nil)))
(defun set-equal (list1 list2)
  (and (subsetp list1 list2) (subsetp list2 list1)))
(defun proper-subset (list1 list2)
  (and (subsetp list1 list2) (not (set-equal list1 list2))))
;;Book answer:
;;(and (subsetp x y) (not (subset y x)))
;;
;;
;;
(defun titled-p (name)
  (member (first name) '(mr ms miss mrs)))
;; whether word is male or female first nmae
(setf male-first-name '(john kim richard fred george))
(setf female-first-name '(jane mary wanda barbara kim))
(defun male-p (name)
  (and (member name male-first-name)
       (not (member name female-first-name))))
(defun female-p (name)
  (and (member name female-first-name)
       (not (member name male-first-name))))
(defun give-title (name)
  "returns a name with proper title"
  (cond ((titled-p name) name)
        ((male-p (first name)) (cons 'mr name))
        ((female-p (first name)) (cons 'ms name))
        (t (append '(mr or ms) name))))

;; exercise
(defun right-side (list)
  (cdr (member '-vs list)))
(defun left-side (list)
  (reverse (cdr (member '-vs (reverse list)))))
(defun count-common (list)
  (length (intersection (left-side list) (right-side list))))
(defun compare (list)
  (cons (count-common list) '(COMMON FEATURES)))
;; table - list of lists - a-list for short, association list
;; each list is called an entry, and the car of each entry is its key
;; Table of 5 words, and french eq.
;; table contains 5 entries, the keys are english words
(setf words '((one un)
              (two deux)
              (three trois)
              (four quatre)
              (five cinq)))





(setf thingies '((object1 large green shiny cube)
                 (object2 small red dull metal cube)
                 (object3 red small dull plastic cube)
                 (object4 small dull blue metal cube)
                 (object5 small shiny red four-sided pyramid)
                 (object6 large shiny green sphere)))
(defun describe-x (x)
  (rest (assoc x thingies)))
;; difference between two objects appear in the first but not second set or second but not first
;; technical term: set exclusive or
(defun differences (x y)
  (set-exclusive-or (describe-x x)
                    (describe-x y)))
;; we can classify properties according to a list of dotted pairs
(setf quality-table
      '((large    . size)
        (small    . size)
        (red      . color)
        (green    . color)
        (blue     . color)
        (shiny    . luster)
        (dull     . luster)
        (metal    . material)
        (plastic  . material)
        (cube     . shape)
        (sphere   . shape)
        (pyramid  . shape)
        (four-sided . shape)))
;; we can use it as part of function that gives us the quality
(defun quality (x)
  (cdr (assoc x quality-table)))
;; using differences and quality, we can write a function to tell use
;; one qulaity that is different between a pair of objects
;;
(defun quality-difference (x y)
  (quality (first (differences x y))))
;; what if we wanted whole list?
;; sublist
(sublis quality-table (differences 'object3 'object4))
;; eleminate duplicates
(defun contrast (x y)
  (remove-duplicates
   (sublis quality-table (differences x y))))


(setf books '((Ascendance-of-bookworm miya-kazuki)
              (spice-and-wolf isuna-hasekura)
              (sufficiently-advanced-magic andrew-rowe)
              (mage-errant john-bierce)
              (colours-of-magic terry-prachett)))
(defun who-wrote (x)
  (cdr (assoc x books)))

(setf nerd-states '((sleeping . eating)
                    (eating . wait)
                    (wait . programming)
                    (programming . debugging)
                    (debugging . sleeping)))
(defun nerdus (state)
  (cdr (assoc state nerd-states)))
(defun sleepless-nerdus (state)
  (let ((status (cdr (assoc state nerd-states))))
    (cond ((equal 'sleeping status) 'eating)
          (t status))))
(defun nerd-on-caffeine (x)
  (nerdus (nerdus x)))

(defun swap-first-last (list)
  (let ((list-l (- (length list) 1)))
    (append (cons (nth list-l list) (cdr (butlast list))) (list (car list)))))
(defun swap-first-last-v2 (listl)
  (let* ((a (reverse (rest listl)))
         (b (reverse (rest a))))
    (cons (car a)
          (append b (list (first listl))))))
(defun rotate-left (listl)
  (append (cdr listl) (list (first listl))))
(defun rotate-right (listl)
  (let* ((last-el (car (reverse listl)))
         (list2 (reverse (cdr (reverse listl)))))
    (cons last-el list2)))


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


;; subst and sublis
;; subst - x y z arguments
;; subst - list of dotted pairs
;; e.g subst ((a . b) (c . d)) list
(defun royal-we (list)
  (subst 'we 'i list))

;; shared structure
;; building list tpyed from kb, is impossible
;; new cons are created.
;;
;;however with cdr car and cons. it is possible
(setf x '(a b c))
(setf y (cons 'd (cdr x)))

;; difference between equal and eq
;; equal - compares elements
;; eq - whether two pointers point to the same object
(setf x1 (list 'a 'b 'c))
(setf x2 (list 'a 'b 'c))
(equal x1 x2)
(eq x1 x2)
(setf z x1)
(eq z x1)
(eq z '(a b c))
(equal z '(a b c))
;; eql for numbers
(eql 'foo 'foo)
(eql 3 3)
(eql 3 3.0)
;; of same types, thats it.
;; for different types, =
(= 3 3.0)
;; equalp is more liberal than equal, ignoes case sensitivity
(equal "foo bar" "FOO bar")
(equalp "foO BaR" "foo bar")

;; Keyword arguments
;; example, REMOVE, takes optional argument called :count
(setf text '(b a n a n a - p a n d a))
;;Remove all As
(remove 'a text)
;;remove 3 As
(remove 'a text :count 3)
;; it also accepts :from-end keyword
;; if the value is non-nil, remove starts from the end
;; so to remove two As from the end
(remove 'a text :count 2 :from-end t)
;; keyword is special symbol,
;; always preceded by colon
;; symbols evaluate to themslevs
:count
(symbolp :count)
(equal :count 'count)
(keywordp :count)
;; membe also takes keyword args
;; normally it uses eql to test.
;; but what if our set contains lists?
;; in that case we must use equal for test
(setf cards '((3 clubs) (5 diamonds) (ace spades)))
(member '(5 diamonds) cards)
(second cards)
;; :test keyword
(member '(5 diamonds) cards :test #'equal)
;; all lists with equalit tests accept that keyword
(remove '(5 diamonds) cards)
(remove '(5 diamonds) cards :test #'equal)
;; also set functions
(remove '(5 diamonds) cards :test (function equal))
;; #'foo => (function foo)
