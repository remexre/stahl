(in-package #:bootstrap)

(defvar *name-resolution-scope*)

(defun resolve-names-for-module (module loaded-modules)
  (with-slots (name exports imports decls) module
    (let ((*name-resolution-scope* nil))
      (loop for clause in imports
            do (resolve-names-for-import-clause clause loaded-modules))
      (loop for decl in decls
            do (resolve-names-for-decl decl name))
      (format t "TODO handle exports ~a vs scope ~a~%" exports *name-resolution-scope*))))

(defun resolve-names-for-import-clause (import-clause loaded-modules)
  (declare (ignore loaded-modules))
  (destructuring-bind (module . names) import-clause
    (format t "TODO import ~a from ~a~%" names module)))

(defgeneric resolve-names-for-decl (decl module-name))

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

(defmethod resolve-names-for-decl ((decl decl-type) module-name)
  (with-slots (name ty-args ctors) decl
    (format t "TODO ty-args ~a~%" ty-args)
    (push name *name-resolution-scope*)
    (loop for ctor in ctors
          do (with-slots (fields ctor-ty-args) ctor
               (loop for field in fields
                     do (format t "field = ~a~%" field))
               (loop for ctor-ty-arg in ctor-ty-args
                     do (format t "ctor-ty-arg = ~a~%" ctor-ty-arg))))
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
