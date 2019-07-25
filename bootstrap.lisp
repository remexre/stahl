(push (truename #p"./bootstrap") asdf:*central-registry*)
(ql:quickload :bootstrap)
(bootstrap:main (merge-pathnames (truename "tmp") "stahl-bootstrap.fth"))
(quit)
