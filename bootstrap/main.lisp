(in-package #:bootstrap)

(opts:define-opts
  (:name        :help
   :description "print this help text"
   :short       #\h
   :long        "help")
  (:name        :output
   :description "the output file"
   :short       #\o
   :long        "output"
   :arg-parser  #'identity))

(defun main ()
  (multiple-value-bind (options args) (opts:get-opts)
    (when (getf options :help)
      (opts:describe
        :prefix "The Stahl bootstrap compiler. Usage:"
        :args "[source files]")
      (opts:exit))

     (loop for src-path in args
       do (process-file src-path))))

(defun scratchpad (path)
  (process-file path))

(defun process-file (path)
  (format t "path = ~a~%" path)
  (let ((module (parse-file path)))
    (format t "module = ~a~%" module)))
