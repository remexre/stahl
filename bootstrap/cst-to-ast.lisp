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

(defun-match parse-name (cst)
  ((cst-symbol (or "*" "->"))
   (error 'invalid-name :cst cst))
  ((cst-symbol value :loc loc)
   (make-instance 'name :value value :origin cst :loc loc))
  (_
   (error 'invalid-name :cst cst)))

(defun-ematch parse-export (cst)
  ((cst-list* _ (cst-symbol "export") names)
   (mapcar #'parse-name (cst-list-to-list names))))

(defun-ematch parse-import (cst)
  ((cst-list _ (cst-symbol "import") module names)
   (let ((names (mapcar #'parse-name (cst-list-to-list names))))
     (cons (parse-name module) names))))

(defun-ematch parse-decl (cst)
  (_ (format nil "todo parse-decl ~a" cst)))

(defun-ematch parse-expr (cst)
  ((cst-symbol "*" :loc loc)
   (make-instance 'expr-type-of-types :loc loc)))
