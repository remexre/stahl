(asdf:defsystem #:bootstrap
  :description "The Stahl bootstrap compiler"
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license  "Apache-2.0/MIT"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "tmp/bootstrap"
  :entry-point "bootstrap:main"
  :depends-on (:trivia :unix-opts)
  :components ((:file "package")
               (:file "utils")
               (:file "cst")
               (:file "ast")
               (:file "cst-to-ast")
               (:file "main")))
