(defvar *swank-path* #p"~/.local/share/nvim/plugged/slimv/slime/start-swank.lisp")

(load #p"bootstrap/bootstrap.asd")
(ql:quickload :bootstrap)
(load *swank-path*)
