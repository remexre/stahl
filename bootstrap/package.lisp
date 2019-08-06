(defpackage #:bootstrap-utils
  (:use #:cl #:trivia)
  (:export
    #:loc ; #:file #:line #:col
    #:syntax-object #:derived-syntax-object #:origin
    #:assoc-equal-value #:elet1 #:let-push #:matches? #:pprint-object-with-slots #:span
    #:*pprint-loc*
    ; #:walk-directory #:walk-directory-to-list
    ))

(defpackage #:bootstrap-cst
  (:use #:bootstrap-utils #:cl)
  (:export
    #:cst-cons #:cst-car #:cst-cdr #:cst-nil
    #:cst-string #:cst-symbol
    #:parse-file-to-cst #:parse-string-to-cst))

(defpackage #:bootstrap
  (:use #:bootstrap-cst #:bootstrap-utils #:cl #:trivia)
  (:export #:cli-main #:main))
