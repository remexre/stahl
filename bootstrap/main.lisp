(in-package #:bootstrap)

(defun main (output-file)
  (format t "output-file = ~a~%" output-file)
  ; (with-open-file (*standard-output* output-file :direction :output)
  (loop for path in (walk-directory-to-list "src/")
		do (process-file path)))

(defun process-file (path)
  (format t "path = ~a~%" path)
  (let ((module (parse-file path)))
  	(format t "module = ~a~%" module)))
