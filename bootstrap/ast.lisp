(in-package #:bootstrap)

(defclass module (syntax-object)
  ((name    :accessor name    :initarg :name)
   (exports :accessor exports :initarg :exports)
   (imports :accessor imports :initarg :imports)
   (decls   :accessor decls   :initarg :decls)))

(defmethod print-object ((module module) stream)
  (pprint-object-with-slots stream module '(name exports imports decls)))

(defclass name (derived-syntax-object)
  ((value :accessor value :initarg :value)))

(defmethod print-object ((name name) stream)
  (format stream "~s" (value name)))

(defclass decl (derived-syntax-object)
  ((name :accessor name :initarg :name)
   (ty   :accessor ty   :initarg :ty)
   (expr :accessor expr :initarg :expr)))

(defmethod print-object ((decl decl) stream)
  (pprint-object-with-slots stream decl '(name ty expr)))

(defclass expr (derived-syntax-object)
  ((ty :accessor ty :initarg :ty)))

(defclass expr-type-of-types (expr) ())

(defmethod print-object ((expr expr-type-of-types) stream)
  (pprint-object-with-slots stream expr '()))

(defclass expr-lam (expr)
  ((var  :accessor var  :initarg :var)
   (body :accessor body :initarg :body)))

(defmethod print-object ((expr expr-lam) stream)
  (pprint-object-with-slots stream expr '(var body)))

(defclass expr-pi (expr)
  ((var  :accessor var  :initarg :var)
   (body :accessor body :initarg :body)))

(defmethod print-object ((expr expr-pi) stream)
  (pprint-object-with-slots stream expr '(var body)))
