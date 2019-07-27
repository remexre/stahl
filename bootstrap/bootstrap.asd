(asdf:defsystem #:bootstrap
  :description "The Stahl bootstrap compiler"
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license  "Apache-2.0/MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:trivia)
  :components ((:file "package")
               (:file "utils")
               (:file "cst")
               (:file "ast")
               (:file "cst-to-ast")
               (:file "main")))
