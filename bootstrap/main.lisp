(in-package #:bootstrap)

(opts:define-opts)

(defun main ()
  (multiple-value-bind (options args) (opts:get-opts)
	(let ((output-file (car args))
		  (src-paths (cdr args)))
       (format t "output-file = ~a~%" output-file)
       (loop for src-path in src-paths
	     do (process-file src-path)))))

(defun scratchpad (path)
  (process-file path))

(defun process-file (path)
  (format t "path = ~a~%" path)
  (let ((module (parse-file path)))
  	(format t "module = ~a~%" module)))
