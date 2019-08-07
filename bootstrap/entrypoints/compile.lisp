(load #p"bootstrap/bootstrap.asd")
(ql:quickload :bootstrap)
(sb-ext:save-lisp-and-die #p"tmp/bootstrap" :toplevel #'bootstrap:cli-main :executable t)  
