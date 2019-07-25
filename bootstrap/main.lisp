(in-package #:bootstrap)

(defun main (output-file)
  (format t "output-file = ~a~%" output-file)
  (with-open-file (*standard-output* output-file :direction :output)
	(walk-directory-to-list "src/")))
