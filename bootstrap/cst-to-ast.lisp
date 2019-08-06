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

(define-condition invalid-ctor-type (error)
  ((cst     :initarg :cst)
   (ty-name :initarg :ty-name)))

(defmethod print-object ((err invalid-ctor-type) stream)
  (with-slots (cst ty-name) err
    (format stream "At ~a: ~a is not a valid type for a constructor of type ~a"
            (loc cst) cst ty-name)))

(define-condition invalid-kind (error)
  ((kind :initarg :kind)))

(defmethod print-object ((err invalid-kind) stream)
  (with-slots (kind) err
    (format stream "At ~a: ~a is not a valid kind" (loc kind) kind)))

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
    ((cst-list _ (cst-symbol "builtin") name)
     (make-decl-builtin (parse-name name)))
    ((cst-list _ (cst-symbol "def") name ty expr)
     (make-decl-def (parse-name name) (parse-expr ty) (parse-expr expr)))
    ((cst-list* _ (cst-symbol "defn") name rest)
     (multiple-value-bind (args return-type after-type) (parse-infix-type rest)
       (let ((name (parse-name name))
             (body (parse-expr (car after-type)))
             (ty (parse-expr return-type)))
         (assert (null (cdr after-type)))
         (loop for arg in (reverse args)
               do (ematch arg
                    ((cst-list _ arg-name-cst arg-ty-cst)
                     (let ((arg-name (parse-name arg-name-cst))
                           (arg-ty   (parse-expr arg-ty-cst)))
                       (setf ty (make-expr-pi arg-name arg-ty ty))
                       (setf body (make-expr-lam arg-name body))))
                    ((cst-list _ (cst-symbol "!") arg-name-cst arg-ty-cst)
                     (let ((arg-name (parse-name arg-name-cst))
                           (arg-ty   (parse-expr arg-ty-cst)))
                       (setf ty (make-expr-pi arg-name arg-ty ty :implicitp t))
                       (setf body (make-expr-lam arg-name body :implicitp t))))))
         (make-decl-def name ty body))))
    ((cst-list* _ (cst-symbol "type") name (cst-symbol ":") rest)
     (let ((ty-name (parse-name name)))
       (if (has-arrow-p rest)
         (multiple-value-bind (args return-type ctors) (parse-infix-type rest)
           (unless (matches? (parse-expr return-type) (expr-type-of-types))
             (error 'invalid-kind :kind return-type))
           (make-decl-type ty-name (mapcar #'parse-arg args) (parse-ctors ctors ty-name)))
         (elet1 (cst-cons kind ctors) rest
           (unless (matches? (parse-expr kind) (expr-type-of-types))
             (error 'invalid-kind :kind kind))
           (make-decl-type ty-name nil (parse-ctors ctors ty-name))))))))

(defun parse-arg (cst)
  (ematch-with-origin cst
    ((cst-list _ name ty)
     (make-arg (parse-name name) (parse-expr ty)))
    ((cst-list _ (cst-symbol "!") name ty)
     (make-arg (parse-name name) (parse-expr ty) :implicitp t))))

(defun has-arrow-p (cst)
  "Checks if a cst list contains the symbol ->."
  (ematch cst
    ((cst-cons (cst-symbol "->") _) t)
    ((cst-cons _ tl)                (has-arrow-p tl))
    ((cst-nil)                      nil)))

(defun parse-infix-type (cst)
  (let* ((parts (span #'(lambda (form) (not (matches? form (cst-symbol "->"))))
                (cst-list-to-list cst)))
         (args (car parts))
         (return-type (caddr parts))
         (after-type (cdddr parts)))
    (values args return-type after-type)))

(defun parse-ctors (cst ty-name)
  (unless (typep cst 'list)
    (setf cst (cst-list-to-list cst)))
  (let ((parsed nil))
    (loop while cst
          do (push (parse-ctor (car cst) ty-name) parsed)
          do (setf cst (cdr cst)))
    (nreverse parsed)))

(defun parse-ctor (cst ty-name)
  (ematch-with-origin cst
    ((cst-list* _ name (cst-symbol ":") rest)
     (if (has-arrow-p rest)
       (multiple-value-bind (args return-type after-type) (parse-infix-type rest)
         (assert (null after-type))
         (make-ctor (parse-name name) (mapcar #'parse-arg args)
            (parse-ctor-return-ty return-type ty-name)))
       (elet1 (cst-list _ ty) rest
         (make-ctor (parse-name name) nil
            (parse-ctor-return-ty ty ty-name)))))))

(defun parse-ctor-return-ty (cst ty-name)
  (ematch-with-origin cst
    ((cst-symbol name)
     (unless (string= name (str ty-name))
       (error 'invalid-ctor-type :cst cst :ty-name ty-name))
     nil)
    ((cst-list* _ (cst-symbol hd) tl)
     (unless (string= hd (str ty-name))
       (error 'invalid-ctor-type :cst cst :ty-name ty-name))
     (mapcar #'parse-expr (cst-list-to-list tl)))))

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
