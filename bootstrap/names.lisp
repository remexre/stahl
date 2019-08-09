(in-package #:bootstrap)

(define-condition no-such-module (error)
  ((name   :initarg :name)))

(defmethod print-object ((err no-such-module) stream)
  (with-slots (name) err
    (format stream "At ~a: Couldn't from ~a, since it doesn't exist!"
            (loc name) name)))

(define-condition not-exported (error)
  ((name   :initarg :name)
   (module :initarg :module)))

(defmethod print-object ((err not-exported) stream)
  (with-slots (name module) err
    (format stream "At ~a: Couldn't import ~a from ~a, since it isn't exported!"
            (loc name) name (name module))))

(define-condition unbound-name (error)
  ((name :initarg :name)))

(defmethod print-object ((err unbound-name) stream)
  (with-slots (name) err
    (let ((*pprint-loc* nil))
      (format stream "At ~a: ~a is not defined!"
              (loc name) name))))

(defvar *name-resolution-scope*)

(defun resolve-names-for-module (module loaded-modules)
  (with-slots (name exports imports decls) module
    (let ((*name-resolution-scope* nil))
      (dolist (clause imports)
        (resolve-names-for-import-clause clause loaded-modules))
      (dolist (decl decls)
        (resolve-names-for-decl decl name))
      (dolist (name exports)
        (resolve-name name)))))

(defun resolve-names-for-import-clause (import-clause loaded-modules)
  (destructuring-bind (sought-module . names) import-clause
    (let ((found-module (cdr (assoc sought-module loaded-modules :test #'name=))))
      (unless found-module
        (error 'no-such-module :name sought-module))
      (dolist (name names)
        (let ((referent (car (member name (exports found-module) :test #'name=))))
          (setf (refers-to name) referent)
          (push name *name-resolution-scope*))))))

(defgeneric resolve-names-for-decl (decl module-name))
(defgeneric resolve-names-for-expr (expr))

(defun resolve-name (name)
  (let ((scope-var (car (member name *name-resolution-scope* :test #'name=))))
    (if scope-var
      (setf (refers-to name) scope-var)
      (error 'unbound-name :name name))))

(defmethod resolve-names-for-decl ((decl decl-builtin) module-name)
  (with-slots (name) decl
    (setf (module-name name) module-name)
    (push name *name-resolution-scope*)))

(defmethod resolve-names-for-decl ((decl decl-def) module-name)
  (with-slots (name ty expr) decl
    (setf (module-name name) module-name)
    (resolve-names-for-expr ty)
    (resolve-names-for-expr expr)
    (push name *name-resolution-scope*)))

(defmethod resolve-names-for-decl ((decl decl-elim) module-name)
  (with-slots (name ty-name) decl
    (setf (module-name name) module-name)
    (resolve-name ty-name)
    (push name *name-resolution-scope*)))

(defmethod resolve-names-for-decl ((decl decl-type) module-name)
  (with-slots (name ty-args ctors) decl
    (setf (module-name name) module-name)
    (push name *name-resolution-scope*)

    ; The implicit arguments to the type are in scope for all constructor definitions.
    (let ((*name-resolution-scope* *name-resolution-scope*))
      (dolist (ty-arg ty-args)
        (when (implicitp ty-arg)
          (push (name ty-arg) *name-resolution-scope*))
        (format t "TODO ty-arg ~a~%" ty-arg))

      (dolist (ctor ctors)
        (with-slots (name ctor-args ctor-ty-args) ctor
          (setf (module-name name) module-name)
          (let ((*name-resolution-scope* *name-resolution-scope*))
            (dolist (ctor-arg ctor-args)
              (with-slots (name) ctor-arg
                (setf (local name) t)
                (push name *name-resolution-scope*))
              (format t "TODO ctor-arg    = (THE ~a ~a)~%" (type-of ctor-arg) ctor-arg))
            (format t "scope = ~a~%" *name-resolution-scope*)
            (dolist (ctor-ty-arg ctor-ty-args)
              (resolve-names-for-expr ctor-ty-arg))))))

    ; The constructors themselves aren't defined until after the constructors.
    (dolist (ctor ctors)
      (push (name ctor) *name-resolution-scope*))))

(defmethod resolve-names-for-expr ((expr expr-app))
  (with-slots (func arg) expr
    (resolve-names-for-expr func)
    (resolve-names-for-expr arg)))

(defmethod resolve-names-for-expr ((expr expr-lam))
  (with-slots (var body) expr
    (setf (local var) t)
    (let-push *name-resolution-scope* var
      (resolve-names-for-expr body))))

(defmethod resolve-names-for-expr ((expr expr-lit-string))
  nil)

(defmethod resolve-names-for-expr ((expr expr-pi))
  (with-slots (var dom cod) expr
    (setf (local var) t)
    (resolve-names-for-expr dom)
    (let-push *name-resolution-scope* var
      (resolve-names-for-expr cod))))

(defmethod resolve-names-for-expr ((expr expr-type-of-types))
  nil)

(defmethod resolve-names-for-expr ((expr expr-var))
  (with-slots (name) expr
    (resolve-name name)))
