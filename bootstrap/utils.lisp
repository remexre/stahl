(in-package #:bootstrap-utils)

(defclass loc ()
  ((file :accessor file :initarg :file :initform nil)
   (line :accessor line :initarg :line)
   (col  :accessor col :initarg :col)))

(defmethod print-object ((loc loc) stream)
  (with-slots (file line col) loc
    (format stream "~a:~a:~a" (or file "<unknown>") line col)))

(defclass syntax-object ()
  ((loc :accessor loc :initarg :loc :initform (error "Must specify :loc"))))

(defclass derived-syntax-object (syntax-object)
  ((origin :accessor origin :initarg :origin :initform (error "Must specify :origin"))))

(defun walk-directory (dir cb)
  (loop for file in (uiop:directory-files dir)
    do (funcall cb file))
  (loop for subdir in (uiop:subdirectories dir)
    do (walk-directory subdir cb)))

(defun walk-directory-to-list (dir)
  (let ((files nil))
    (walk-directory dir #'(lambda (file) (push file files)))
    files))
