(in-package #:bootstrap)

(defun parse-file (path)
  (parse-module (parse-file-to-cst path)))

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
  ((list* (cst-list loc (cst-symbol "module") (cst-symbol module-name)) rest)
   (let* ((exports-split (span #'is-export? rest))
          (imports-split (span #'is-import? (cdr exports-split)))
          (exports (mapcar #'parse-export (car exports-split)))
          (imports (mapcar #'parse-import (car imports-split)))
          (decls (mapcar #'parse-decl (cdr imports-split))))
     (make-instance 'module :name module-name :exports exports :imports imports
                    :decls decls :loc loc))))

(defun is-export? (cst)
  (matches? cst (cst-list* _ (cst-symbol "export") _)))

(defun is-import? (cst)
  (matches? cst (cst-list* _ (cst-symbol "import") _)))

(defun-ematch parse-export (cst)
  (_ (format nil "todo parse-export ~a" cst)))

(defun-ematch parse-import (cst)
  (_ (format nil "todo parse-import ~a" cst)))

(defun-ematch parse-decl (cst)
  (_ (format nil "todo parse-decl ~a" cst)))

(defun-ematch parse-expr (cst)
  ((cst-symbol "*" :loc loc)
   (make-instance 'expr-type-of-types :loc loc)))
