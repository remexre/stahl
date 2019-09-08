(in-package #:bootstrap-utils)

(defclass loc ()
  ((file :accessor file :initarg :file :initform nil)
   (start-line :accessor start-line :initarg :start-line)
   (start-col  :accessor start-col  :initarg :start-col)
   (end-line   :accessor end-line   :initarg :end-line)
   (end-col    :accessor end-col    :initarg :end-col)))

(defmethod print-object ((loc loc) stream)
  (with-slots (file start-line start-col end-line end-col) loc
    (format stream "In ~a: ~a:~a-~a:~a" (or file "<unknown>")
      start-line start-line end-line end-col)))

(defclass syntax-object ()
  ((loc :accessor loc :initarg :loc :initform (error "Must specify :loc"))))

(defclass derived-syntax-object ()
  ((loc                     :initarg :loc    :initform nil)
   (origin :accessor origin :initarg :origin :initform (error "Must specify :origin"))))

(defmethod loc ((obj derived-syntax-object))
  (or (slot-value obj 'loc)
      (loc (origin obj))))

(defun assoc-equal-value (item alist)
  (cdr (assoc item alist :test #'equal)))

(defmacro elet1 (pat expr &body body)
  `(ematch ,expr (,pat ,@body)))

(defmacro let-push (var expr &body body)
  `(let ((,var (cons ,expr ,var)))
     ,@body))

(defun make-resizable-vector ()
  (make-array 0 :fill-pointer 0 :adjustable t))

(defmacro matches? (expr pat)
  `(match ,expr (,pat t)))

(defun pprint-object-pair (stream pair colonp atsignp)
  (declare (ignore colonp atsignp))
  (format stream ":~(~w~) ~w" (car pair) (cdr pair)))

(defparameter *pprint-loc* nil)

(defun pprint-object (stream name obj slots)
  (when (and *pprint-loc* (not (assoc 'loc slots)))
    (push (cons 'loc (loc obj)) slots))
  (format stream "#<~@<~(~w~)~{ ~_~/bootstrap-utils::pprint-object-pair/~}~:>>" name slots))

(defun pprint-object-with-slots (stream obj slots)
  (pprint-object stream (class-name (class-of obj)) obj
                 (mapcar #'(lambda (slot) (cons slot (slot-value obj slot))) slots)))

(defun span (pred lst)
  (cons
    (loop until (null lst)
          until (not (funcall pred (car lst)))
          collect (car lst)
          do (setf lst (cdr lst)))
    lst))

(defun toposort (input must-be-after-p)
  "Sorts an input list topologically."
  (let ((output nil))
    (dolist (val input)
      (setf output (insert-after-required output val must-be-after-p)))
    (validate-toposort output must-be-after-p)
    output))

(defun insert-after-required (vals val must-be-after-p)
  (cond
    ((null vals) (list val))
    ((funcall must-be-after-p val (car vals))
     (setf (cdr vals) (insert-after-required (cdr vals) val must-be-after-p))
     vals)
    (t
     (cons val vals))))

(defun validate-toposort (vals must-be-after-p)
  "Because I don't 100% trust that I did the toposort right."
  (loop for val in vals
        for i from 0
        do (loop for earlier in vals
                 for j from 0
                 until (= i j)
                 do (assert (not (funcall must-be-after-p earlier val))))))

#|
(defun walk-directory (dir cb)
  (dolist (file (uiop:directory-files dir))
    (funcall cb file))
  (dolist (subdir (uiop:subdirectories dir))
    (walk-directory subdir cb)))

(defun walk-directory-to-list (dir)
  (let ((files nil))
    (walk-directory dir #'(lambda (file) (push file files)))
    files))
|#
