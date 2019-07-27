(in-package #:bootstrap)

(defclass module (syntax-object)
  ((name :accessor name :initarg :name :initform (error "Must specify :name"))
   (exports :accessor exports :initarg :exports)
   (imports :accessor imports :initarg :imports)
   (decls :accessor decls :initarg :decls)))

(defmethod print-object ((module module) stream)
  (with-slots (name exports imports decls) module
    (format stream "#<module :name ~s :exports ~s :imports ~s :decls ~s>" name
                   exports imports decls)))

(defclass name (derived-syntax-object)
  ((value :accessor value :initarg value :initform (error "Must specify :value"))))

(defmethod print-object ((name name) stream)
  (format stream "~s" (value name)))

(defclass decl (derived-syntax-object)
  ((name :accessor name :initarg :name :initform (error "Must specify :name"))
   (ty :accessor ty
       :initarg :ty
       :initform (error "Must specify :ty"))
   (expr :accessor expr
         :initarg :expr
         :initform (error "Must specify :expr"))))

(defmethod print-object ((decl decl) stream)
  (with-slots (name ty expr) decl
    (format stream "#<decl :name ~s :ty ~s :expr ~s>" name ty expr)))

(defclass expr (derived-syntax-object)
  ((ty :accessor ty
       :initarg :ty
       :initform (error "Must specify :ty"))))

(defclass expr-type-of-types (expr) ())

(defmethod print-object ((expr expr-type-of-types) stream)
  (declare (ignore expr))
  (format stream "#<expr-type-of-types>"))

(defclass expr-lam (expr)
  ((var :accessor var :initarg :var :initform (error "Must specify :var"))
   (body :accessor body :initarg :body :initform (error "Must specify :body"))))

(defmethod print-object ((expr expr-lam) stream)
  (with-slots (var body) expr
    (format stream "#<expr-lam :var ~s :body ~s>" var body)))

(defclass expr-pi (expr)
  ((var :accessor var :initarg :var :initform (error "Must specify :var"))
   (body :accessor body :initarg :body :initform (error "Must specify :body"))))

(defmethod print-object ((expr expr-pi) stream)
  (with-slots (var body) expr
    (format stream "#<expr-pi :var ~s :body ~s>" var body)))
