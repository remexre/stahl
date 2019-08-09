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
  (let* ((parsed-modules (mapcar #'parse-file src-paths))
         (loaded-modules nil))
    (format t "parsed-modules = ~a~%" parsed-modules)
    (dolist (module (toposort parsed-modules #'module-depends-on))
      (resolve-names-for-module module loaded-modules)
      (push (cons (name module) module) loaded-modules))))

(defun test-main ()
  "A main function for debugging in SLIME."
  (main
    (list
      "./src/std/list.stahl"
      "./src/std/io.stahl"
      "./src/std/string.stahl"
      "./src/std/vect.stahl"
      "./src/std/nat.stahl"
      "./src/std.stahl"
      "./src/bin/compiler.stahl")
    "tmp/stahl-bootstrap.fth"))
