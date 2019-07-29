(in-package #:bootstrap)

(defun resolve-names-for-module (module)
  (format t "module = ~a~%" module)
  (mapc #'resolve-names-for-decl (decls module)))

(defgeneric resolve-names-for-decl (decl))

(defmethod resolve-names-for-decl ((decl null))
  (format t "TODO: unprocessed decl...")
  nil)

(defmethod resolve-names-for-decl ((decl decl-builtin))
  nil)

(defmethod resolve-names-for-decl ((decl decl-def))
  (format t "free-vars = ~a~%"
          (union (free-vars (ty   decl))
                 (free-vars (expr decl)))))

(defgeneric free-vars (expr))

(defmethod free-vars ((decl decl-builtin))
  nil)

(defmethod free-vars ((decl decl-def))
  (union (free-vars (ty   decl))
         (free-vars (expr decl))))

(defmethod free-vars ((expr expr-app))
  (union (free-vars (func expr))
         (free-vars (arg  expr))))

(defmethod free-vars ((expr expr-lam))
    (set-difference (free-vars (body expr))
                    (list (var expr))))

(defmethod free-vars ((expr expr-lit-string))
  nil)

(defmethod free-vars ((expr expr-pi))
  (union (free-vars (dom expr))
         (set-difference (free-vars (cod expr))
                         (list (var expr)))))

(defmethod free-vars ((expr expr-type-of-types))
  nil)

(defmethod free-vars ((expr expr-var))
  (list (name expr)))
