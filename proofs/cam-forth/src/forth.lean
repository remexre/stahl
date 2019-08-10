import data.vector
import utils

-- The words of our stack language that can appear in definitions.
inductive forth_word : Type
| app : forth_word
| drop : forth_word
| dup : forth_word
| fst : forth_word
| literal : int → forth_word
| print : forth_word
| quote : nat → forth_word
| snd : forth_word
| swap : forth_word

inductive forth_word_any : Type
| exit : forth_word_any
| word : forth_word → forth_word_any

namespace forth_word_any
  def from_option : option forth_word → forth_word_any
  | (some w) := word w
  | none := exit
end forth_word_any

-- Programs in our stack language.
structure forth_program :=
(fresh : nat)
(defs : vector (list forth_word) fresh)
(prf_main_exists : fresh ≠ 0)

-- The program counter, constructed so as to always represent a valid
-- definition, and either a valid word, or one word past the end of the definition
-- (for exit).
structure forth_pc (prg : forth_program) :=
(def_idx : fin (forth_program.fresh prg))
(word_idx : fin (nat.succ (list.length (vector.nth (forth_program.defs prg) def_idx))))

namespace forth_program
  def empty : forth_program :=
    forth_program.mk 1 (vector.cons list.nil vector.nil) (nat.succ_ne_zero _)

  def define : list forth_word → forth_program → forth_program :=
    λ new_word old,
      forth_program.mk
        (nat.succ (forth_program.fresh old))
        (vector.snoc new_word (forth_program.defs old))
        (nat.succ_ne_zero _)
end forth_program

namespace forth_pc
  def advance {prg : forth_program} : forth_pc prg → forth_pc prg :=
    λ pc,
      let def_len := vector.nth (forth_program.defs prg) (def_idx pc) in
      sorry

  def word {prg : forth_program} : forth_pc prg → forth_word_any :=
    λ pc,
      forth_word_any.from_option
        (list.nth
          (vector.nth (forth_program.defs prg) (def_idx pc))
          (word_idx pc).1)
end forth_pc

inductive forth_value (prg : forth_program) : Type
| addr : forth_pc prg → forth_value
| num  : int → forth_value

structure forth_vm (prg : forth_program) :=
(pc : forth_pc prg)
(data_stack : list (forth_value prg))
(return_stack : list (forth_value prg))
(output : list int)

namespace forth_vm
  variables {α β : Type}
  variables {prg : forth_program}

  def load : forth_vm prg :=
    mk
      (forth_pc.mk (fin.zero (forth_program.prf_main_exists prg)) 0)
      list.nil list.nil list.nil

  structure eval (prg : forth_program) (α : Type) : Type :=
  (run : state_t (forth_vm prg) (except_t string id) α)

  @[inline]
  protected def pure (a : α) : eval prg α :=
    eval.mk (pure a)

  @[inline]
  protected def bind (x : eval prg α) (f : α → eval prg β) : eval prg β :=
    eval.mk (do a ← x.run, (f a).run)

  instance : monad (eval prg) :=
  { pure := λ α, @forth_vm.pure α prg
  , bind := λ α β, @forth_vm.bind α β prg
  }

  instance : monad_except string (eval prg) :=
  { throw := λ α, eval.mk ∘ state_t.lift ∘ except_t.mk ∘ except.error
  , catch := λ α x f, eval.mk (state_t.mk (λ s,
               except_t.catch (x.run.run s) (λ e, (f e).run.run s)))
  }

  instance : monad_state (forth_vm prg) (eval prg) :=
  { lift := λ α m, eval.mk (monad_state.lift m)
  }

  def advance_pc : eval prg unit :=
    modify (λ prev,
      mk (forth_pc.advance (pc prev)) (data_stack prev) (return_stack prev)
         (output prev))

  protected def pop_value_helper : list α → eval prg (α × list α)
  | list.nil := throw "Stack underflow"
  | (list.cons h t) := return (h, t)

  def pop_value : eval prg (forth_value prg) := do
    vm ← get,
    stack ← forth_vm.pop_value_helper (data_stack vm),
    put (mk (pc vm) stack.2 (return_stack vm) (output vm)),
    return stack.1

  protected def pop_int_helper : forth_value prg → eval prg int
  | (forth_value.num _ n) := return n
  | val := throw "Type error"

  def pop_int : eval prg int := pop_value >>= forth_vm.pop_int_helper

  def push_value : forth_value prg → eval prg unit :=
    λ val, do
      vm ← get,
      let stack := list.cons val (data_stack vm),
      put (mk (pc vm) stack (return_stack vm) (output vm)),
      return ()

  def next_word : eval prg forth_word_any := do
    vm ← get,
    return (forth_pc.word (pc vm))

  def next : eval prg unit := do
    word ← next_word,
    return ()
end forth_vm

namespace forth_vm
  open forth_value
  open forth_vm
  open forth_word
  open forth_word_any

  variables {prg : forth_program}

  def print_int : int → eval prg unit :=
    λ n, sorry

  def do_word : forth_word_any → eval prg unit
  | exit := sorry
  | (word app) := sorry
  | (word drop) := sorry
  | (word dup) := sorry
  | (word fst) := sorry
  | (word (literal n)) := sorry
  | (word print) := pop_int >>= print_int
  | (word (quote name)) := sorry
  | (word snd) := sorry
  | (word swap) := do
      a ← pop_value,
      b ← pop_value,
      push_value a,
      push_value b
end forth_vm
