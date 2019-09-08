Categorical Abstract Machine
============================

These are some notes on the CAM. Read the original paper for more detail.

Static Combinators
------------------

Nullary:

-	`app`
-	`fst`
-	`snd`
-	`'` (takes a value as argument)

Unary: `Λ`

Binary: `_ ∘ _`, `<_, _>`

### Translation from λ

(where `0`, `1+n` are de Brujin indices)

-	`[[0]] = snd`
-	`[[1+n]] = [[n]] ∘ fst`
-	`[[c]] = 'c`
-	`[[f x]] = App ∘ <[[f]], [[x]]>`
-	`[[λ x]] = Λ([[x]])`
