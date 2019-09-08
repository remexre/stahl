(in-package #:bootstrap)

(defclass forth-program ()
  ((defs :accessor defs :initform (make-resizable-vector))))

(defclass forth-def ()
  ((name :accessor name :initarg :name)
   (words :accessor words :initform (make-resizable-vector))))

(defun create-def (program name)
  (check-type program forth-program)
  (check-type name    string)
  (let ((def (make-instance 'forth-def :name name)))
    (push def (defs program))
    def))
