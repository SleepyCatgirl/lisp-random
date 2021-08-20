;; Basic operations will be getting a list of files in a library
;; and detemirning whether a file exists.


;; Mechanism for writing implemetaiton specific code
;; read-time conditionalization
;; conditionally include code based on various features
;;
;; *FEATURES* and two extra bits of syntax
;;
;;
;; To avoid symbol overwriting and errors
;; We export only several definitions
(in-package :cl-user)
(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))
(in-package :com.gigamonkeys.pathnames)
;;
;; You can implement a function for single directory, as
;; thin wrapper around DIRECTORY
;;(directory (make-pathname :name :wild :type :wild :defaults home-dir))
;; helper functions, to convert pathname to appripriate wild pathname
;;
;; test whether component of pathname is present
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))
;; test whether pathanme is in directory form
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))
;; pathname-as-directory
;; convert any pathname to a directory form pathname
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames"))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name  nil
         :type  nil
         :defaults pathname)
        pathname)))
;; CLISP
;; DIRECTORY wont return files with no extension,
;; unless type is NIL rather than :wild
;;
;; func that takes pathname, and returns proper wildcard
;; for given implementaiton
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;;(defun list-directory (dirname)
;;  (when (wild-pathname-p dirname)
;;    (error "Can only list concrete directory names"))
;;  (directory (directory-wildcard dirname)))


(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
