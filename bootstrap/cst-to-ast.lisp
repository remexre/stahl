(in-package #:bootstrap)

(defun parse-file (path)
  (parse-module (parse-file-to-cst path)))

(defun parse-module (csts)
  (loop for cst in csts
        do (format t "~a~%" cst)))
