#lang cKanren

(provide
  add-const
  add-meta
  in-scope
  lookup
  set)

; (require cKanren/attributes)
(require cKanren/miniKanren)
(require cKanren/neq)
(require "mlf-syntax.rkt")

(define (meta-not-in-sig sig var)
  (conde
    ((== sig '()))
    ((fresh (a ad d var2)
       (conso a d sig)
       (conso 'meta ad a)
       (caro ad var2)
       (=/= var var2)
       (not-in-sig d var)))))

(define (not-in-sig sig con)
  (conde
    ((== sig '()))
    ((fresh (a d con2)
       (conso a d sig)
       (caro a con2)
       (=/= con con2)
       (not-in-sig d con)))))

(define (add-const sig-before sig-after con ty tm C)
  (fresh ()
    (signature sig-before)
    (signature sig-after)
    (not-in-sig sig-before con)
    (== sig-after (cons `(,con : ,ty = ,tm when ,C) sig-before))))

(define (add-meta sig-before sig-after var ty)
  (fresh ()
    (signature sig-before)
    (signature sig-after)
    (meta-not-in-sig sig-before var)
    (== sig-after (cons `(meta ,var : ,ty) sig-before))))

(define (in-scope sig-before sig-after) 'todo)

(define (lookup sig-before sig-after con ty)
  (fresh ()
    (signature sig-before)
    (signature sig-after)
    (membero `(,con : ,ty) sig-before)
    (== sig-after sig-before)))

(define (set sig-before sig-after) 'todo)
