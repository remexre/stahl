(in-package #:bootstrap)

(defun read-file-to-lines (path)
  (let (lines)
    (with-open-file (stream path)
      (loop for line = (read-line stream nil)
            while line
            do (push line lines)))
    (nreverse lines)))

(defun html-highlighted-source (path)
  (let* ((lines (read-file-to-lines path))
         (module (parse-file path)))
    (resolve-names-for-module module nil)
    (format t "module = ~a~%" module)))
