#lang racket

(require threading)
(require "syntax.rkt")

(define (get-srcs)
  (~>> (in-directory "src")
       sequence->stream
       (stream-filter file-exists?)
       (stream-map (lambda (p) (list p (file->list p))))))

(define (main)
  (for ([src (get-srcs)])
    (match-define (list path data) src)
    (printf "~a ~a~%" path data)))

(main)
