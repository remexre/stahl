(in-package #:bootstrap)

(defvar *origin*)

(defmacro with-origin (origin &body body)
  `(let ((*origin* ,origin))
     ,@body))

(defmacro match-with-origin (origin &body clauses)
  (alexandria:with-gensyms (origin-name)
    `(let ((,origin-name ,origin))
       (with-origin ,origin-name
         (match ,origin-name ,@clauses)))))

(defmacro ematch-with-origin (origin &body clauses)
  (alexandria:with-gensyms (origin-name)
    `(let ((,origin-name ,origin))
       (with-origin ,origin-name
         (ematch ,origin-name ,@clauses)))))

(defclass module (syntax-object)
  ((name    :accessor name    :initarg :name)
   (exports :accessor exports :initarg :exports)
   (imports :accessor imports :initarg :imports)
   (decls   :accessor decls   :initarg :decls)))

(defmethod print-object ((module module) stream)
  (pprint-object-with-slots stream module '(name exports imports decls)))

(defclass name (derived-syntax-object)
  ((str :accessor str :initarg :str)
   (refers-to :accessor refers-to :initform nil)
   (module-name :accessor module-name :initform nil)))

(defun name= (l r)
  (check-type l name)
  (check-type r name)
  (string= (str l) (str r)))

(defmethod print-object ((name name) stream)
  (with-slots (str refers-to module-name) name
    (if (or refers-to module-name *pprint-loc*)
      (pprint-object-with-slots stream name '(str refers-to module-name))
      (format stream "~s" (str name)))))

(defclass decl (derived-syntax-object)
  ())

(defclass decl-builtin (decl)
  ((name :accessor name :initarg :name)))

(defun make-decl-builtin (name &key loc)
  (check-type name name)
  (check-type loc  (or loc null))

  (make-instance 'decl-builtin :name name :origin *origin* :loc loc))

(defmethod print-object ((decl decl-builtin) stream)
  (pprint-object-with-slots stream decl '(name)))

(defclass decl-def (decl)
  ((name :accessor name :initarg :name)
   (ty   :accessor ty   :initarg :ty)
   (expr :accessor expr :initarg :expr)))

(defun make-decl-def (name ty expr &key loc)
  (check-type name name)
  (check-type ty   expr)
  (check-type expr expr)
  (check-type loc  (or loc null))

  (make-instance 'decl-def :name name :ty ty :expr expr :origin *origin* :loc loc))

(defmethod print-object ((decl decl-def) stream)
  (pprint-object-with-slots stream decl '(name ty expr)))

(defclass decl-type (decl)
  ((name  :accessor name  :initarg :name)
   (kind  :accessor kind  :initarg :kind)
   (ctors :accessor ctors :initarg :ctors)))

(defun make-decl-type (name kind ctors &key loc)
  (check-type name name)
  (check-type kind expr)
  (check-type ctors list) ; of ctors

  (make-instance 'decl-type :name name :kind kind :ctors ctors :origin *origin* :loc loc))

(defmethod print-object ((decl decl-type) stream)
  (pprint-object-with-slots stream decl '(name kind ctors)))

(defclass ctor (derived-syntax-object)
  ((name    :accessor name    :initarg :name)
   (fields  :accessor fields  :initarg :fields)
   (ty-args :accessor ty-args :initarg :ty-args)))

(defun make-ctor (name fields ty-args &key loc)
  (check-type name    name)
  (check-type fields  list) ; of exprs
  (check-type ty-args list) ; of exprs

  (make-instance 'ctor :name name :fields fields :ty-args ty-args :origin *origin* :loc loc))

(defmethod print-object ((ctor ctor) stream)
  (pprint-object-with-slots stream ctor '(name fields ty-args)))

(defclass expr (derived-syntax-object)
  ((ty :accessor ty)))

(defclass expr-app (expr)
  ((func :accessor func :initarg :func)
   (arg  :accessor arg  :initarg :arg)))

(defun make-expr-app (func arg)
  (check-type func expr)
  (check-type arg  expr)
  (make-instance 'expr-app :func func :arg arg :origin *origin*))

(defmethod print-object ((expr expr-app) stream)
  (pprint-object-with-slots stream expr '(func arg)))

(defclass expr-lam (expr)
  ((var  :accessor var  :initarg :var)
   (body :accessor body :initarg :body)
   (implicitp :accessor implicitp :initarg :implicitp :initform nil)))

(defun make-expr-lam (var body &key implicitp)
  (check-type var  name)
  (check-type body expr)
  (make-instance 'expr-lam :var var :body body :implicitp implicitp :origin *origin*))

(defmethod print-object ((expr expr-lam) stream)
  (pprint-object-with-slots stream expr '(var implicitp body)))

(defclass expr-lit-string (expr)
  ((str :accessor str :initarg :str)))

(defun make-expr-lit-string (str)
  (make-instance 'expr-lit-string :str str :origin *origin*))

(defmethod print-object ((expr expr-lit-string) stream)
  (pprint-object-with-slots stream expr '(str)))

(defclass expr-pi (expr)
  ((var :accessor var :initarg :var)
   (dom :accessor dom :initarg :dom)
   (cod :accessor cod :initarg :cod)
   (implicitp :accessor implicitp :initarg :implicitp :initform nil)))

(defun make-expr-pi (var dom cod &key implicitp)
  (check-type var name)
  (check-type dom expr)
  (check-type cod expr)
  (make-instance 'expr-pi :var var :dom dom :cod cod :implicitp implicitp :origin *origin*))

(defmethod print-object ((expr expr-pi) stream)
  (pprint-object-with-slots stream expr '(var implicitp dom cod)))

(defclass expr-type-of-types (expr) ())

(defun make-expr-type-of-types ()
  (make-instance 'expr-type-of-types :origin *origin*))

(defmethod print-object ((expr expr-type-of-types) stream)
  (pprint-object-with-slots stream expr '()))

(defclass expr-var (expr)
  ((name :accessor name :initarg :name)))

(defun make-expr-var (name)
  (make-instance 'expr-var :name name :origin *origin*))

(defmethod print-object ((expr expr-var) stream)
  (pprint-object-with-slots stream expr '(name)))
