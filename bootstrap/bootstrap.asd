(asdf:defsystem #:bootstrap
  :description "The Stahl bootstrap compiler"
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license  "Apache-2.0/MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "cst")
               (:file "main")
               (:file "utils")))
