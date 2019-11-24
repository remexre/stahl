#lang cKanren

(provide
  listofo
  rangeo)

(require cKanren/miniKanren)

(define (listofo l pred)
  (conde
    ((== l null))
    ((fresh (a d)
       (pred a)
       (listofo d pred)
       (== l (cons a d))))))

(define (rangeo m n)
  (if (= n 0)
      fail
      (conde
        ((== m (- n 1)))
        ((rangeo m (- n 1))))))
