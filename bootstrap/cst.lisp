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
    (cons (to-lisp cst-car) (to-lisp cst-cdr))))

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
  ((file :initarg :file :initform nil)
   (inner-next-col :initform 0)
   (inner-next-line :initform 1)
   (inner-stream :initarg :stream :initform (error "Must specify :stream"))
   (buffer-char :accessor buffer-char :initform nil)))

(defun loc-from (stream marker)
  (with-slots (file inner-next-line inner-next-col) stream
    (make-instance 'loc :start-line (car marker) :start-col (cdr marker)
                   :end-line inner-next-line :end-col inner-next-col
                   :file file)))

(defun loc-marker (stream)
  (with-slots (inner-next-line inner-next-col) stream
    (cons inner-next-line inner-next-col)))

(defun peek (stream)
  (with-slots (buffer-char inner-stream) stream
    ; Should only need to fire once?
    (unless buffer-char
      (setf buffer-char (read-char inner-stream)))
    (values buffer-char (loc-marker stream))))

(defun peek-or-nil (stream)
  (handler-case (peek stream)
    (end-of-file () nil)))

(defun advance (stream)
  (let ((ch (peek stream)))
    (with-slots (inner-next-col inner-next-line) stream
      (cond
        ((eq ch #\newline) (incf inner-next-line)
                           (setf inner-next-col 1))
        (t                 (incf inner-next-col))))
    (setf (buffer-char stream) nil)
    ch))

(defmacro push-if (expr place)
  (let ((name (gensym)))
    `(let ((,name ,expr))
       (when ,name
         (push ,name ,place)))))

(defun parse-exprs (stream)
  (let ((output nil))
    (handler-case (loop do (push-if (parse-expr stream) output))
      (end-of-file () nil))
    (reverse output)))

(defun parse-file-to-cst (path)
  (with-open-file (stream path)
    (parse-exprs (make-instance 'loc-stream :file path :stream stream))))

(defun parse-string-to-cst (str &key (path "<string>"))
  (let ((stream (make-string-input-stream str)))
    (parse-exprs (make-instance 'loc-stream :file path :stream stream))))

(defun parse-expr (stream)
  (loop while (and (peek stream) (spacep (peek stream)))
        do (advance stream))
  (multiple-value-bind (ch marker) (peek stream)
    (cond
      ((eq ch #\")     (advance stream)
                       (parse-string stream marker))
      ((eq ch #\#)     (loop for ch = (advance stream)
                             until (eq ch #\newline))
                       nil)
      ((eq ch #\()     (advance stream)
                       (parse-list-tail stream marker))
      ((eq ch #\{)     (advance stream)
                       (parse-brack-tail stream marker))
      ((symbolishp ch) (parse-symbol stream marker))
      (t               (error "Unexpected character ~s~%" ch)))))

(defun parse-brack-tail (stream marker)
  (let ((acc #'(lambda (last) last))
        (last (make-instance 'cst-nil :loc (loc-from stream marker))))
    (loop for ch = (peek stream)
          until (eq ch #\})
          do (cond
               ((spacep ch) (advance stream))
               (t
                (let ((head (parse-expr stream)))
                  (when head
                    (let ((prev-acc acc))
                      (setf acc #'(lambda (last)
                        (funcall prev-acc (make-instance 'cst-cons :cst-car head :cst-cdr last
                                                         :loc (loc-from stream marker)))))))))))
    (advance stream)
    (make-instance 'cst-cons :loc (loc-from stream marker)
      :cst-car (make-instance 'cst-symbol :loc (loc-from stream marker) :value "!")
      :cst-cdr (funcall acc last))))

(defun parse-list-tail (stream marker)
  (let ((acc #'(lambda (last) last))
        (last (make-instance 'cst-nil :loc (loc-from stream marker))))
    (loop for ch = (peek stream)
          until (eq ch #\))
          do (cond
               ((spacep ch) (advance stream))
               ((eq ch #\|)
                (advance stream)
                (setf last (parse-expr stream)))
               (t
                (let ((head (parse-expr stream)))
                  (when head
                    (let ((prev-acc acc))
                      (setf acc #'(lambda (last)
                        (funcall prev-acc (make-instance 'cst-cons :cst-car head :cst-cdr last
                                                         :loc (loc-from stream marker)))))))))))
    (advance stream)
    (funcall acc last)))

(defun parse-string (stream marker)
  (let ((chs (make-array 0 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop for raw-ch = (advance stream)
          until (eq raw-ch #\")
          for ch = (if (eq raw-ch #\\) (parse-escape stream) raw-ch)
          do (vector-push-extend ch chs))
    (make-instance 'cst-string :loc (loc-from stream marker) :value (coerce chs 'simple-string))))

(defun parse-symbol (stream marker)
  (let ((chs (make-array 0 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop for ch = (peek-or-nil stream)
          while (symbolishp ch)
          do (vector-push-extend ch chs)
          do (advance stream))
    (make-instance 'cst-symbol :loc (loc-from stream marker) :value (coerce chs 'simple-string))))

(defun spacep (ch)
  (char< ch #\!))

(defun symbolishp (ch)
  (and ch
    (or (and (char<= #\0 ch) (char<= ch #\9))
        (and (char<= #\A ch) (char<= ch #\Z))
        (and (char<= #\a ch) (char<= ch #\z))
        (member ch '(#\! #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\?)))))
