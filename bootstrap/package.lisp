(defpackage #:bootstrap-utils
  (:use #:cl #:trivia)
  (:export
    #:loc ; #:file #:line #:col
    #:syntax-object #:derived-syntax-object #:origin
    #:matches? #:pprint-object-with-slots #:span
    ; #:walk-directory #:walk-directory-to-list
    ))

(defpackage #:bootstrap-cst
  (:use #:bootstrap-utils #:cl)
  (:export
    #:cst-cons #:cst-nil #:cst-string #:cst-symbol
    #:parse-file-to-cst #:parse-string-to-cst))

(defpackage #:bootstrap
  (:use #:bootstrap-cst #:bootstrap-utils #:cl #:trivia)
  (:export #:cli-main #:main))
