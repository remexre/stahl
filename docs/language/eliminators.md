Eliminators
===========

**TODO**: Get this under testing!

To simplify (read: avoid implementing) termination analysis, Stahl has functions called eliminators instead of pattern matching or recursion. When a type is defined with `defty`, an eliminator function is defined for that type.

For example, the declaration:

```stahl
defty Vect (pi ((T TYPE) (_ Nat)) TYPE)
  nil-Vect  (pi ((T TYPE))                              (Vect T zero))
  cons-Vect (pi ((T TYPE) (n Nat) (_ T) (_ (Vect T n))) (Vect T (succ n)))
```

will generate a function named `elim-Vect` with the type:

```stahl
pi
  group
    T TYPE
    m (pi ((n Nat) (_ (Vect T n))) TYPE)
    _ (m Z (nil-Vect T))
    _ (pi ((n Nat) (h T) (t (Vect T n)) (_ (m n t))) (m (S n) (vcons T n h t)))
    n Nat
    v (Vect T n)
  m n v
```

This function internally performs pattern-matching and primitive structural recursion.

Example Eliminator Types
------------------------

### Void

```stahl
; elim-Void
pi
  group
    m (pi ((_ Void)) TYPE)
    v Void
  m v
```

Note that `elim-Void` lets us trivially define the principle of explosion as:

```stahl
def explosion (pi ((P TYPE) (fls Void)) P)
  fn (P fls)
    elim-Void 
      fn (_) P
      fls
```

### Unit

```stahl
; elim-Unit
pi
  group
    m (pi ((_ Unit)) TYPE)
    _ (m u)
    u Unit
  m u
```

### Bool

```stahl
; elim-Bool
pi
  group
    m (pi ((_ Bool)) TYPE)
    _ (m false)
    _ (m true)
    b Bool
  m b
```

### Nat

```stahl
; elim-Nat
pi
  group
    m (pi ((_ Nat)) TYPE)
    _ (m Z)
    _ (pi ((n Nat) (_ (m n))) (m (S n)))
    n Nat
  m n
```
