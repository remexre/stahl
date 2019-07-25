(in-package #:bootstrap)

(defclass module (syntax-object)
  ((file :accessor file :initarg :file)
   (decls :accessor decls :initarg :decls)))

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
  (format t "cst = ~s~%" (parse-file-to-cst path)))
