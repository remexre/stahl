(in-package #:bootstrap-cst)

; We can't just use READ, since symbols are non-case-sensitive.

(defclass cst-cons (syntax-object)
  ((cst-car :accessor cst-car :initarg :cst-car)
   (cst-cdr :accessor cst-cdr :initarg :cst-cdr)))

(defclass cst-nil (syntax-object) ())

(defclass cst-symbol (syntax-object)
  ((value :accessor value :initarg :value)))

(defun parse-file-to-cst (path)
  (with-open-file (file path)
	(parse-stream file path)))

(defun parse-stream (stream &optional path)
  (let ((line 0))
    (format t "done~%")
    (loop for str = (read-line stream nil)
          while str
          do (incf line)
          collect (parse-line str line path))))

(defun parse-line (str line &optional path)
  (defun loc-at (col)
    (make-instance 'loc :file path :line line :col col))
  (format t "TODO (parse-line ~s ~s ~s)~%" str line path))
