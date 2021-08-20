(defun complement-base (base)
  (cond ((equal base 'a) 't)
        ((equal base 't) 'a)
        ((equal base 'g) 'c)
        ((equal base 'c) 'g)
        (t nil)))

(defun complement-strand (base-list)
  (let ((result nil))
    (dolist (e base-list (reverse result))
      (push (complement-base e) result))))

(defun make-double (base-list)
  (let ((double-list nil))
    (dolist (index base-list (reverse double-list))
      (push (list index (complement-base index)) double-list))))


;;(defun count-bases (base-list)
;;  (let ((a-counter 0)
;;        (t-counter 0)
;;        (g-counter 0)
;;        (c-counter 0))
;;    (dolist (e base-list (list a-counter t-counter g-counter c-counter))
;;      (cond ((equal e 'a) (incf a-counter))
;;            (t nil)))))
(defvar acnt 0)
(defvar tcnt 0)
(defvar gcnt 0)
(defvar ccnt 0)
(setf acnt 0)
(setf tcnt 0)
(setf gcnt 0)
(setf ccnt 0)


(defun count-single-base (base)
  (cond ((equal base 'a) (incf acnt))
        ((equal base 't) (incf tcnt))
        ((equal base 'g) (incf gcnt))
        ((equal base 'c) (incf ccnt))))
;;(defun count-bases (list-base)
;;  (dolist (element list-base)
;;    (cond ((atom element) (count-single-base element))
;;          (t (count-single-base (first element))
;;             (count-single-base (second element)))))
;;  (list (list 'a acnt)
;;        (list 't tcnt)
;;        (list 'g gcnt)
;;        (list 'c ccnt)))

(defun count-bases (list-base)
  (progn
    (setf acnt 0)
    (setf tcnt 0)
    (setf gcnt 0)
    (setf ccnt 0)
    (dolist (element list-base)
      (cond ((atom element) (count-single-base element))
            (t (count-single-base (first element))
               (count-single-base (second element)))))
    (list (list 'a acnt)
          (list 't tcnt)
          (list 'g gcnt)
          (list 'c ccnt))))
;;(defun prefixp (prefix base-list)
;;  (do* ((x base-list (rest x))
;;        (e prefix (rest e)))
;;       ((not (equal (first x) (first e))) (return nil))
;;    (null e) t))
;;(defun prefixp (prefix base-list)
;;  (do ((x base-list (rest x))
;;       (e prefix (rest e)))
;;      ((null e) t)
;;    (not (equal (first x) (first e))) 'n))
;;
;;(defun prefixp (prefix base-list)
;;  (cond ((null base-list) nil)
;;        ((null prefix) t)
;;        ((equal (first prefix) (first base-list)) (prefixp (rest prefix) (rest base-list)))
;;        (t nil)))
;;(defun prefixp (prefix base-list)
;;  (do* ((x base-list (rest x))
;;        (e prefix (rest e)))
;;       ((and (equal (first x) (first e)) (not (equal (second x) (second e))) t)
;;    (null x) nil)))

(defun prefixp (prefix base-list)
  (do ((s1 prefix (rest s1))
       (s2 base-list (rest s2)))
      ((null s1) t)
    (unless (equal (first s1) (first s2))
      (return nil))))

;;(defun appearsp (prefix base-list)
;;  (do ((s1 base-list (member (first prefix) s1))
;;       (s2 base-list (rest s2)))
;;      ((null s2) (return nil))
;;    (if (prefixp prefix s1) (return t) nil)))
(defun appearsp (prefix base-list)
  (do ((s2 base-list (rest s2)))
      ((null s2) nil)
    (if (prefixp prefix s2) (return t))))


;;(defun coverp (prefix base-list)
;;  (do ((s1 base-list (nthcdr (length prefix) s1)))
;;      ((null s1) (return t))
;;    (if (not (prefixp prefix s1)) (return nil))))
(defun coverp (strand1 strand2)
  (do* ((len1 (length strand1))
        (s2 strand2 (nthcdr len1 s2)))
       ((null s2) t)
    (unless (prefixp strand1 s2)
      (return nil))))


(defun prefix-n (n base-list)
  (do* ((len1 1 (+ len1 1))
        (s1 base-list (rest s1))
        (prefix (list (car s1)) (cons (car s1) prefix)))
       ((equal len1 n) (return (reverse prefix)))))

(defun kernel (base-list)
  (do* ((index 1 (+ index 1))
        (s1 (list (car base-list)) (subseq base-list 0 index)))
       ((coverp s1 base-list) (return s1))))