(in-package #:bootstrap)

(defclass syntax-object ()
  ((loc :accessor loc :initarg :loc)))

(defclass module (syntax-object)
  ((file :accessor file :initarg :file)))

(defclass decl (syntax-object)
  ((ty :accessor ty
	   :initarg :ty
	   :initform (error "Must specify a ty"))
   (expr :accessor expr
	     :initarg :expr
	     :initform (error "Must specify an expr"))))

(defclass expr (syntax-object)
  ((ty :accessor ty
	   :initarg :ty
	   :initform (error "Must specify a ty"))))

(defclass expr-type-of-types (expr) ())
(defclass expr-lam (expr)
  ((var :accessor var :initarg :var)))
(defclass expr-pi (expr)
  ((var :accessor var :initarg :var)))

(defun parse-file (path)
  (with-open-file (file path)
	(loop for foo = (read file)
		  do (format t "~a~%" foo))))
