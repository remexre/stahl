(in-package #:bootstrap)

(defclass module (derived-syntax-object)
  ((name :accessor name :initarg :name)
   (exports :accessor exports :initarg :exports)
   (imports :accessor imports :initarg :imports)
   (decls :accessor decls :initarg :decls)))

(defclass name (derived-syntax-object)
  ((value :accessor value :initarg value :initform (error "Must specify :value"))))

(defclass decl (derived-syntax-object)
  ((ty :accessor ty
       :initarg :ty
       :initform (error "Must specify :ty"))
   (expr :accessor expr
         :initarg :expr
         :initform (error "Must specify :expr"))))

(defclass expr (derived-syntax-object)
  ((ty :accessor ty
       :initarg :ty
       :initform (error "Must specify :ty"))))

(defclass expr-type-of-types (expr) ())
(defclass expr-lam (expr)
  ((var :accessor var :initarg :var)))
(defclass expr-pi (expr)
  ((var :accessor var :initarg :var)))
