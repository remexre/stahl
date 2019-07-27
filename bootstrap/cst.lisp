(in-package #:bootstrap-cst)

; We can't just use READ, since the reader treats symbols as non-case-sensitive.

(defgeneric to-lisp (node))

(defclass cst (syntax-object) ())
(defmethod print-object ((node cst) stream)
  (format stream "~s" (to-lisp node)))

(defclass cst-cons (cst)
  ((cst-car :accessor cst-car :initarg :cst-car :initform (error "Must specify :cst-car"))
   (cst-cdr :accessor cst-cdr :initarg :cst-cdr :initform (error "Must specify :cst-cdr"))))
(defmethod to-lisp ((node cst-cons))
  (with-slots (cst-car cst-cdr) node
    (cons cst-car cst-cdr)))

(defclass cst-nil (cst) ())
(defmethod to-lisp ((node cst-nil))
  (declare (ignore node))
  nil)

(defclass cst-string (cst)
  ((value :accessor value :initarg :value :initform (error "Must specify :value"))))
(defmethod to-lisp ((node cst-string))
  (value node))

(defclass cst-symbol (cst)
  ((value :accessor value :initarg :value :initform (error "Must specify :value"))))
(defmethod to-lisp ((node cst-symbol))
  ; Should this actually be a symbol?
  (make-symbol (value node)))

(defclass loc-stream ()
  ((col :initform 0)
   (line :initform 1)
   (file :initarg :file :initform nil)
   (inner-stream :initarg :stream :initform (error "Must specify :stream"))
   (buffer-char :accessor buffer-char :initform nil)
   (buffer-loc :accessor buffer-loc :initform nil)))

(defun loc-at (stream)
  (with-slots (col line file) stream
    (make-instance 'loc :col col :line line :file file)))

(defun peek-loc-stream (stream)
  (with-slots (buffer-char buffer-loc inner-stream) stream
    (unless buffer-char
      (setf buffer-loc (loc-at stream))
      (setf buffer-char (read-char inner-stream)))
    (values buffer-char buffer-loc)))

(defun next-char (stream)
  (multiple-value-bind (ch loc) (peek-loc-stream stream)
    (with-slots (col line) stream
      (cond
        ((eq ch #\newline) (incf line) (setf col 1))
        (t                 (incf col))))
    (setf (buffer-char stream) nil)
    (values ch loc)))

(defmacro push-if (expr place)
  (let ((name (gensym)))
    `(let ((,name ,expr))
       (when ,name
         (push ,name ,place)))))

(defun parse-file-to-cst (path)
  (with-open-file (plain-stream path)
    (let ((stream (make-instance 'loc-stream :file path :stream plain-stream))
          (output nil))
      (handler-case (loop do (push-if (parse-expr stream) output))
        (end-of-file ()))
      (reverse output))))

(defun parse-expr (stream)
  (multiple-value-bind (ch loc) (peek-loc-stream stream)
    (cond
      ((spacep ch)     (next-char stream) nil)
      ((eq ch #\")     (next-char stream) (make-instance 'cst-string :loc loc
                                                         :value (parse-string stream)))
      ((eq ch #\#)     (loop for ch = (next-char stream) until (eq ch #\newline)) nil)
      ((eq ch #\()     (next-char stream) (parse-list-tail stream loc))
      ((symbolishp ch) (make-instance 'cst-symbol :loc loc :value (parse-symbol stream)))
      (t               (error "Unexpected character ~s~%" ch)))))

(defun parse-list-tail (stream loc)
  (let ((acc #'(lambda (last) last))
        (last (make-instance 'cst-nil :loc loc)))
    (loop for ch = (peek-loc-stream stream)
          until (eq ch #\))
          do (cond
               ((spacep ch) (next-char stream))
               ((eq ch #\|)
                (next-char stream)
                (setf last (parse-expr stream)))
               (t
                (let ((head (parse-expr stream)))
                  (when head
                    (let ((prev-acc acc))
                      (setf acc #'(lambda (last)
                        (funcall prev-acc (make-instance 'cst-cons :cst-car head :cst-cdr last
                                                         :loc loc))))))))))
    (next-char stream)
    (funcall acc last)))

(defun parse-string (stream)
  (let ((chs (make-array 0 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop for raw-ch = (next-char stream)
          until (eq raw-ch #\")
          for ch = (if (eq raw-ch #\\) (parse-escape stream) raw-ch)
          do (vector-push-extend ch chs))
    (coerce chs 'simple-string)))

(defun parse-symbol (stream)
  (let ((chs (make-array 0 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop for ch = (peek-loc-stream stream)
          while (symbolishp ch)
          do (vector-push-extend ch chs)
          do (next-char stream))
    (coerce chs 'simple-string)))

(defun spacep (ch)
  (char< ch #\!))

(defun symbolishp (ch)
  (or (and (char<= #\0 ch) (char<= ch #\9))
      (and (char<= #\A ch) (char<= ch #\Z))
      (and (char<= #\a ch) (char<= ch #\z))
      (member ch '(#\* #\+ #\- #\/ #\: #\< #\= #\> #\?))))
