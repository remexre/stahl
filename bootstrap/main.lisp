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

(defun cli-main ()
  (multiple-value-bind (options args) (opts:get-opts)
    (when (getf options :help)
      (opts:describe
        :prefix "The Stahl bootstrap compiler. Usage:"
        :args "[source files]")
      (opts:exit))
    (main args (getf options :output-path))))

(defun main (src-paths output-path)
  (declare (ignore output-path))
  (mapc #'process-file src-paths))

(defun process-file (path)
  (format t "path = ~a~%" path)
  (let ((module (parse-file path)))
    (resolve-names-for-module module)
    (format t "module = ~a~%" module)))
