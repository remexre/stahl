(in-package #:bootstrap)

(defun walk-directory (dir cb)
  (loop for file in (uiop:directory-files dir)
	do (funcall cb file))
  (loop for subdir in (uiop:subdirectories dir)
	do (walk-directory subdir cb)))
