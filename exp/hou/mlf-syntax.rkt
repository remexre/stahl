#lang cKanren

(provide
  type
  term
  context
  signature
  constr)

(require cKanren/attributes)
(require cKanren/miniKanren)
(require "utils.rkt")

(define (type ty [depth 0])
  (conde
    ((== ty 'Set))
    ((fresh (tm)
       (term tm depth)
       (== ty `(term ,tm))))
    ((fresh (ty1 ty2)
       (type ty1 depth)
       (type ty2 (+ 1 depth))
       (== ty `(pi ,ty1 ,ty2))))))

(define (term tm [depth 0])
  (conde
    ((fresh (var)
       (rangeo var depth)
       (== tm `(var ,var))))
    ((fresh (con)
       (symbol con)
       (== tm `(con ,con))))
    ((fresh (tm1 tm2)
       (term tm1 depth)
       (term tm2 depth)
       (== tm `(app ,tm1 ,tm2))))
    ((fresh (tm1)
       (term tm1 (+ 1 depth))
       (== tm `(lam ,tm1))))))

(define (context ctx)
  (listofo ctx type))

(define (signature sig) (listofo sig signature1))
(define (signature1 sig1)
  (conde
    ((fresh (name ty)
       (symbol name)
       (type ty)
       (== sig1 `(,name : ,ty))))
    ((fresh (name ty tm)
       (symbol name)
       (type ty)
       (term tm)
       (== sig1 `(,name : ,ty = ,tm))))
    ((fresh (name ty tm C)
       (symbol name)
       (type ty)
       (term tm)
       (constr C)
       (== sig1 `(,name : ,ty = ,tm when ,C))))))

(define (constr C)
  (fresh (l r)
    (conde
      ((type l)
       (type r)
       (== C `(=-type ,l ,r)))
      ((term l)
       (term r)
       (== C `(=-term ,l ,r)))
      ((listofo l term)
       (listofo r term)
       (== C `(=-terms ,l ,r))))))
