(defpackage #:bootstrap-utils
  (:use #:cl)
  (:export
    #:loc ; #:file #:line #:col
    #:syntax-object
    #:read-file
    #:walk-directory #:walk-directory-to-list))

(defpackage #:bootstrap-cst
  (:use #:bootstrap-utils #:cl)
  (:export #:parse-file-to-cst))

(defpackage #:bootstrap
  (:use #:bootstrap-cst #:bootstrap-utils #:cl #:trivia)
  (:export #:main))
