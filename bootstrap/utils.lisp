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

(defclass derived-syntax-object ()
  ((loc :initarg :loc :initform nil)
   (origin :accessor origin :initarg :origin :initform (error "Must specify :origin"))))

(defmethod loc ((obj derived-syntax-object))
  (or (slot-value obj 'loc)
      (loc (origin obj))))

(defmacro matches? (expr pat)
  `(match ,expr (,pat t)))

(defun pprint-object-pair (stream pair colonp atsignp)
  (declare (ignore colonp atsignp))
  (format stream ":~(~w~) ~w" (car pair) (cdr pair)))

(defun pprint-object (stream name slots)
  (format stream "#<~@<~(~w~)~{ ~_~/bootstrap-utils::pprint-object-pair/~}~:>>" name slots))

(defun pprint-object-with-slots (stream obj slots)
  (pprint-object stream (class-name (class-of obj))
                 (mapcar #'(lambda (slot) (cons slot (slot-value obj slot))) slots)))

(defun span (pred list)
  (cons
    (loop until (null list)
          until (not (funcall pred (car list)))
          collect (car list)
          do (setf list (cdr list)))
    list))

(defun walk-directory (dir cb)
  (loop for file in (uiop:directory-files dir)
    do (funcall cb file))
  (loop for subdir in (uiop:subdirectories dir)
    do (walk-directory subdir cb)))

(defun walk-directory-to-list (dir)
  (let ((files nil))
    (walk-directory dir #'(lambda (file) (push file files)))
    files))
