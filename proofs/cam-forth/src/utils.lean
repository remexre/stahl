import data.vector

universes u v
variables {α : Type u} {β : Type v} {m n : nat}



notation x ` ▶ `:1 f:0 := f x

def compose_all : list (α → α) → α → α
| list.nil := (λ x, x)
| (list.cons h t) := h ∘ compose_all t

def compose_all' : α → list (α → α) → α := flip compose_all

example : compose_all [(λ n, n * 2), (λ n, n + 1)] 1 = 4 := rfl
example : compose_all [(λ n, n + 1), (λ n, n * 2)] 1 = 3 := rfl
example : compose_all' 1 [(λ n, n * 2), (λ n, n + 1)] = 4 := rfl
example : compose_all' 1 [(λ n, n + 1), (λ n, n * 2)] = 3 := rfl



namespace fin
  def pred' : fin (nat.succ n) → option (fin n)
  | ⟨0, _⟩ := option.none
  | ⟨nat.succ k, prf⟩ := some ⟨k, nat.lt_of_succ_lt_succ prf⟩

  lemma zero_lt_ne_zero : Π n, n ≠ 0 → 0 < n
  | 0 prf := absurd rfl prf
  | (nat.succ n) _ := nat.zero_lt_succ n

  def zero {n : nat} : n ≠ 0 → fin n := λprf, ⟨0, zero_lt_ne_zero n prf⟩
end fin



namespace list
  def snoc : α → list α → list α
  | x list.nil        := list.cons x list.nil
  | x (list.cons h t) := list.cons h (snoc x t)

  theorem snoc_len (x : α) : Π l, list.length (list.snoc x l) = nat.succ (list.length l)
  | list.nil := rfl
  | (list.cons h t) := congr_arg nat.succ (snoc_len t)
end list

namespace vector
  def snoc : α -> vector α n -> vector α (nat.succ n)
  | x ⟨lst, prf⟩ := ⟨list.snoc x lst, by { rw (symm prf), rw (list.snoc_len x lst) }⟩
end vector
