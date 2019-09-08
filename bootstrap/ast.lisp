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
  ((str         :accessor str         :initarg :str)
   (refers-to   :accessor refers-to   :initform nil)
   (local       :accessor local       :initform nil)
   (module-name :accessor module-name :initform nil)))

(defun copy-referent (to from)
  (check-type to   name)
  (check-type from name)
  (assert (null (refers-to to)))
  (setf (refers-to to) (refers-to from)))

(defun name= (l r)
  (check-type l name)
  (check-type r name)
  (string= (str l) (str r)))

(defmethod module-name ((name name))
  (unless (local name)
    (or (slot-value name 'module-name)
        (module-name (refers-to name)))))

(defmethod print-object ((name name) stream)
  (pprint-object-with-slots stream name '(str refers-to local module-name)))

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

(defclass decl-elim (decl)
  ((name    :accessor name    :initarg :name)
   (ty-name :accessor ty-name :initarg :ty-name)))

(defun make-decl-elim (name ty-name &key loc)
  (check-type name    name)
  (check-type ty-name name)
  (make-instance 'decl-elim :name name :ty-name ty-name :origin *origin* :loc loc))

(defmethod print-object ((decl decl-elim) stream)
  (pprint-object-with-slots stream decl '(name ty-name)))

(defclass decl-type (decl)
  ((name    :accessor name    :initarg :name)
   (ty-args :accessor ty-args :initarg :ty-args)
   (ctors   :accessor ctors   :initarg :ctors)))

(defun make-decl-type (name ty-args ctors &key loc)
  (check-type name    name)
  (check-type ty-args list) ; of args
  (check-type ctors   list) ; of ctors
  (make-instance 'decl-type :name name :ty-args ty-args :ctors ctors :origin *origin* :loc loc))

(defmethod print-object ((decl decl-type) stream)
  (pprint-object-with-slots stream decl '(name ty-args ctors)))

(defclass ctor (derived-syntax-object)
  ((name         :accessor name         :initarg :name)
   (ctor-args    :accessor ctor-args    :initarg :ctor-args)
   (ctor-ty-args :accessor ctor-ty-args :initarg :ctor-ty-args)))

(defun make-ctor (name ctor-args ctor-ty-args &key loc)
  (check-type name         name)
  (check-type ctor-args    list) ; of args
  (check-type ctor-ty-args list) ; of exprs
  (make-instance 'ctor :name name :ctor-args ctor-args :ctor-ty-args ctor-ty-args
                 :origin *origin* :loc loc))

(defmethod print-object ((ctor ctor) stream)
  (pprint-object-with-slots stream ctor '(name ctor-args ctor-ty-args)))

(defclass arg (derived-syntax-object)
  ((name      :accessor name      :initarg :name)
   (ty        :accessor ty        :initarg :ty)
   (implicitp :accessor implicitp :initarg :implicitp :initform nil)))

(defun make-arg (name ty &key implicitp loc)
  (check-type name name)
  (check-type ty   expr)
  (make-instance 'arg :name name :ty ty :implicitp implicitp :origin *origin* :loc loc))

(defmethod print-object ((arg arg) stream)
  (pprint-object-with-slots stream arg '(name ty implicitp)))

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
  (pprint-object-with-slots stream expr '(func arg))) (defclass expr-lam (expr)
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
