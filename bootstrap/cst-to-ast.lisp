(in-package #:bootstrap)

(defun parse-file (path)
  (parse-module (parse-file-to-cst path)))

(defun parse-string (str &key (path "<string>"))
  (parse-module (parse-string-to-cst str :path path)))

(defpattern cst-cons (head tail &key loc)
  `(class cst-cons :cst-car ,head :cst-cdr ,tail :loc ,loc))

(defpattern cst-nil (&key loc)
  `(class cst-nil :loc ,loc))

(defpattern cst-list (loc &rest args)
  (if args
    `(cst-cons ,(car args) (cst-list _ ,@(cdr args)) :loc ,loc)
    `(cst-nil :loc ,loc)))

(defpattern cst-list* (loc &rest args)
  (if (cdr args)
    `(cst-cons ,(car args) (cst-list* _ ,@(cdr args)) :loc ,loc)
    (car args)))

(defpattern cst-string (value &key loc)
  `(class cst-string :value ,value :loc ,loc))

(defpattern cst-symbol (value &key loc)
  `(class cst-symbol :value ,value :loc ,loc))

(defun-ematch parse-module (csts)
  ((list* (cst-list loc (cst-symbol "module") module-name) rest)
   (let* ((exports-split (span #'is-export? rest))
          (imports-split (span #'is-import? (cdr exports-split)))
          (exports (mapcan #'parse-export (car exports-split)))
          (imports (mapcar #'parse-import (car imports-split)))
          (decls (mapcar #'parse-decl (cdr imports-split))))
     ; TODO: Some sort of import-merging is needed
     ; e.g. '((foo bar) (quux xyzzy) (foo baz)) -> '((foo bar baz) (quux xyzzy))
     (make-instance 'module :name (parse-name module-name) :exports exports
                    :imports imports :decls decls :loc loc))))

(defun-ematch cst-list-to-list (cst)
  ((cst-nil) nil)
  ((cst-cons head tail) (cons head (cst-list-to-list tail))))

(defun is-export? (cst)
  (matches? cst (cst-list* _ (cst-symbol "export") _)))

(defun is-import? (cst)
  (matches? cst (cst-list* _ (cst-symbol "import") _)))

(define-condition invalid-name (error)
  ((cst :initarg :cst)))

(defmethod print-object ((err invalid-name) stream)
  (with-slots (cst) err
    (format stream "At ~a: ~a is not a valid name!"
            (loc cst)
            (match cst
              ((cst-symbol s) s)
              (_ cst)))))

(defun parse-name (cst)
  (match-with-origin cst
    ((cst-symbol (or "_" "->" "TYPE"))
     (error 'invalid-name :cst cst))
    ((cst-symbol value)
     (make-instance 'name :str value :origin cst))
    (_
     (error 'invalid-name :cst cst))))

(defun-ematch parse-export (cst)
  ((cst-list* _ (cst-symbol "export") names)
   (mapcar #'parse-name (cst-list-to-list names))))

(defun-ematch parse-import (cst)
  ((cst-list _ (cst-symbol "import") module names)
   (let ((names (mapcar #'parse-name (cst-list-to-list names))))
     (cons (parse-name module) names))))

(defun parse-decl (cst)
  (ematch-with-origin cst
    ((cst-list loc (cst-symbol "builtin") name)
     (make-decl-builtin (parse-name name)))
    ((cst-list loc (cst-symbol "def") name ty expr)
     (make-decl-def (parse-name name) (parse-expr ty) (parse-expr expr) :loc loc))
    ((cst-list* loc (cst-symbol "defn") name rest)
     (let* ((name (parse-name name))
            (parts (span #'(lambda (form) (not (matches? form (cst-symbol "->"))))
                         (cst-list-to-list rest)))
            (args (car parts))
            (return-type (caddr parts))
            (body (cadddr parts))
            (ty (parse-expr return-type)))
       (assert (null (cddddr parts)))
       (loop for arg in (reverse args)
             do (ematch arg
                  ((cst-list _ arg-name arg-ty)
                   (setf ty (make-expr-pi (parse-name arg-name)
                                          (parse-expr arg-ty)
                                          ty)))
                  ((cst-list _ (cst-symbol "!") arg-name arg-ty)
                   (setf ty (make-expr-pi (parse-name arg-name)
                                          (parse-expr arg-ty)
                                          ty :implicitp t)))))
       (make-decl-def name ty (parse-expr body))))
    ((cst-list* loc (cst-symbol "type") rest)
     (format t "todo @ ~a: type ~a~%" loc rest))))

(defun parse-expr (cst)
  (ematch-with-origin cst
    ((cst-list* _ func args)
     (let ((func (parse-expr func)))
       (loop for arg in (cst-list-to-list args)
             do (setf func (make-expr-app func (parse-expr arg))))
       func))
    ((cst-string s)
     (make-expr-lit-string s))
    ((cst-symbol "TYPE")
     (make-expr-type-of-types))
    ((cst-symbol _)
     (make-expr-var (parse-name cst)))))
