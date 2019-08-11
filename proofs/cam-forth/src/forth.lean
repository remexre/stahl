import data.vector
import utils

-- The words of our stack language that can appear in definitions.
inductive forth_word : Type
| app : forth_word
| drop : forth_word
| dup : forth_word
| fst : forth_word
| lit : int → forth_word
| print : forth_word
| quote : nat → forth_word
| snd : forth_word
| swap : forth_word

inductive forth_word_any : Type
| exit : bool → forth_word_any
| word : forth_word → forth_word_any

namespace forth_word_any
  def from_option : option forth_word → forth_word_any
  | (some w) := word w
  | none := exit ff -- TODO: bool should be "was it from main"
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
  def new : list forth_word → forth_program :=
    λ main, forth_program.mk 1 (vector.cons main vector.nil) (nat.succ_ne_zero _)

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

structure eval (prg : forth_program) (α : Type) : Type :=
  (run : state_t (forth_vm prg) (except_t string id) α)

namespace eval
  open forth_vm

  variables {α β : Type}
  variable {prg : forth_program}

  @[inline]
  protected def pure (a : α) : eval prg α :=
    eval.mk (pure a)

  @[inline]
  protected def bind (x : eval prg α) (f : α → eval prg β) : eval prg β :=
    eval.mk (do a ← x.run, (f a).run)

  instance : monad (eval prg) :=
  { pure := λ α, @eval.pure α prg
  , bind := λ α β, @eval.bind α β prg
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
      forth_vm.mk (forth_pc.advance (pc prev)) (data_stack prev)
                  (return_stack prev) (output prev))

  protected def pop_value_helper : list α → eval prg (α × list α)
  | list.nil := throw "Stack underflow"
  | (list.cons h t) := return (h, t)

  def pop_value : eval prg (forth_value prg) := do
    vm ← get,
    stack ← eval.pop_value_helper (data_stack vm),
    put (forth_vm.mk (pc vm) stack.2 (return_stack vm) (output vm)),
    return stack.1

  protected def pop_int_helper : forth_value prg → eval prg int
  | (forth_value.num _ n) := return n
  | val := throw "Type error (wanted int)"

  def pop_int : eval prg int :=
    pop_value >>= eval.pop_int_helper

  protected def pop_pc_helper : forth_value prg → eval prg (forth_pc prg)
  | (forth_value.addr pc) := return pc
  | val := throw "Type error (wanted forth_pc)"

  def pop_pc : eval prg (forth_pc prg) :=
    pop_value >>= eval.pop_pc_helper

  def push_value : forth_value prg → eval prg unit :=
    λ val, do
      vm ← get,
      let stack := list.cons val (data_stack vm),
      put (forth_vm.mk (pc vm) stack (return_stack vm) (output vm)),
      return ()

  def next_word : eval prg forth_word_any := do
    vm ← get,
    return (forth_pc.word (pc vm))
end eval

namespace forth_vm
  open eval
  open forth_value
  open forth_vm
  open forth_word
  open forth_word_any

  variable {prg : forth_program}

  def new : Π (prg : forth_program), forth_vm prg :=
    λ prg, mk
      (forth_pc.mk (fin.zero (forth_program.prf_main_exists prg)) 0)
      list.nil list.nil list.nil

  protected def print_int : int → eval prg unit :=
    λ n, sorry

  def do_word : forth_word_any → eval prg unit
  | (exit from_main) := sorry
  | (word app) := sorry
  | (word drop) := do
      pop_value,
      return ()
  | (word dup) := do
      a ← pop_value,
      push_value a,
      push_value a
  | (word fst) := sorry
  | (word (lit n)) := push_value (num prg n)
  | (word print) := pop_int >>= forth_vm.print_int
  | (word (quote name)) := sorry
  | (word snd) := sorry
  | (word swap) := do
      a ← pop_value,
      b ← pop_value,
      push_value a,
      push_value b

  def step : eval prg unit := eval.next_word >>= do_word

  def stopped : forth_vm prg → bool :=
    λ vm, ff -- TODO

  def steps : nat → eval prg unit
  | 0 := return ()
  | (n+1) := do
    vm ← get,
    if stopped vm then
      return ()
    else
      (step >> steps n)
end forth_vm

namespace forth_program
  def run : nat → forth_program → except string (option (list int)) :=
    λ num_steps prg,
      let output_if_done : forth_vm prg → option (list int) :=
        λ vm, not vm.stopped |> vm.output in
      let m := (forth_vm.steps num_steps).run.run (forth_vm.new prg) in
      except.map (output_if_done ∘ prod.snd) m.run
end forth_program
