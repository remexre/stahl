(in-package #:bootstrap)

(defmethod translate-expr ((expr expr-app))
  (with-slots (func arg) expr
    (translate-expr func)
    (translate-expr arg)))

(defmethod translate-expr ((expr expr-lam))
  (with-slots (var body) expr
    (setf (local var) t)
    (let-push *name-resolution-scope* var
      (translate-expr body))))

(defmethod translate-expr ((expr expr-lit-string))
  nil)

(defmethod translate-expr ((expr expr-pi))
  (with-slots (var dom cod) expr
    (setf (local var) t)
    (translate-expr dom)
    (let-push *name-resolution-scope* var
      (translate-expr cod))))

(defmethod translate-expr ((expr expr-type-of-types))
  nil)

(defmethod translate-expr ((expr expr-var))
  (with-slots (name) expr
    (resolve-name name)))
