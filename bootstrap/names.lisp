(in-package #:bootstrap)

(defvar *name-current-module*)
(defvar *name-resolution-scope*)

(defun resolve-names-for-module (module)
  (format t "module = ~a~%" module)
  (let ((*name-current-module* (name module))
        (*name-resolution-scope* nil))
    (mapc #'resolve-names-for-decl (decls module))))

(defgeneric resolve-names-for-decl (decl))

(defmethod resolve-names-for-decl ((decl decl-builtin))
  (setf (module-name (name decl)) *name-current-module*)
  (push (name decl) *name-resolution-scope*))

(defmethod resolve-names-for-decl ((decl decl-def))
  (with-slots (name ty expr) decl
    (setf (module-name name) *name-current-module*)
    (resolve-names-for-expr ty)
    (resolve-names-for-expr expr)
    (push name *name-resolution-scope*)))

(defmethod resolve-names-for-decl ((decl decl-type))
  (with-slots (name kind ctors) decl
    (resolve-names-for-expr kind)
    (push name *name-resolution-scope*)
    (loop for ctor in ctors
          do (with-slots (fields ty-args) ctor
               (loop for field in fields
                     do (format t "field = ~a~%" field))
               (loop for ty-arg in ty-args
                     do (format t "ty-arg = ~a~%" ty-arg))))
    (loop for ctor in ctors
          do (push (name ctor) *name-resolution-scope*))))

(defgeneric resolve-names-for-expr (expr))

(defmethod resolve-names-for-expr ((expr expr-app))
  (with-slots (func arg) expr
    (resolve-names-for-expr func)
    (resolve-names-for-expr arg)))

(defmethod resolve-names-for-expr ((expr expr-lam))
  (with-slots (var body) expr
    (let-push *name-resolution-scope* var
      (resolve-names-for-expr body))))

(defmethod resolve-names-for-expr ((expr expr-lit-string))
  nil)

(defmethod resolve-names-for-expr ((expr expr-pi))
  (with-slots (var dom cod) expr
    (resolve-names-for-expr dom)
    (let-push *name-resolution-scope* var
      (resolve-names-for-expr cod))))

(defmethod resolve-names-for-expr ((expr expr-type-of-types))
  nil)

(define-condition unbound-name (error)
  ((name :initarg :name)))

(defmethod print-object ((err unbound-name) stream)
  (with-slots (name) err
    (let ((*pprint-loc* nil))
      (format stream "At ~a: ~a is not defined!"
              (loc name) name))))

(defmethod resolve-names-for-expr ((expr expr-var))
  (with-slots (name) expr
    (let ((scope-var (car (member name *name-resolution-scope* :test #'name=))))
      (if scope-var
        (setf (refers-to name) scope-var)
        (error 'unbound-name :name name)))))
