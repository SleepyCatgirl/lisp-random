(defstruct node
  (name nil)
  (question nil)
  (yes-case nil)
  (no-case nil))

(defvar *NODE-LIST*)
(defun init ()
  (setf *node-list* nil)
  'inited)

(defun add-node (name question yes-case no-case)
  (push (make-node :name name
                :question question
                :yes-case yes-case
                :no-case no-case)
    *node-list*)
  name)


(defun find-node (name)
  (find-if #'(lambda (datastr) (equal (node-name datastr) name)) *node-list*))

;;(defun process-node (name)
;;  (let ((found-node (find-node name)))
;;    (cond ((null found-node) (format t "~&Node not found"))
;;          (t (format t "~&~S" (node-question found-node))
;;             (let ((input (read)))
;;               (cond ((equal input 'yes) (node-yes-case found-node))
;;                     ((equal input 'no) (node-no-case found-node))))))))

(defun process-node (name)
  (let ((nd (find-node name)))
    (if nd
        (if (y-or-n-p "~&~A "
                      (node-question nd))
            (node-yes-case nd)
            (node-no-case nd))
        (format t
                "~&Node ~S not yet defined." name))))


(defun run ()
  (do ((current-node 'start (process-node current-node)))
      ((null current-node) nil)
    (cond ((stringp current-node)
           (format t "~&~A" current-node)
           (return nil)))))

(defun prompt-for (question)
  (format t "~&~A :" question)
  (read))
(defun interactive-add ()
  (let* ((name (prompt-for "Node name: "))
         (question (prompt-for "Question: "))
         (yes-case (prompt-for "Yes case: "))
         (no-case (prompt-for "No case: ")))
    (add-node name question yes-case no-case)))
