(in-package #:bootstrap)

(defun module-depends-on (depender dependee)
  (member (name dependee)
          (mapcar #'car (imports depender))
          :test #'name=))
