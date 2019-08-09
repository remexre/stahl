import data.vector
import utils

-- The words of our stack language.
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

-- Programs in our stack language.
structure forth_program :=
(fresh : nat)
(defs : vector (list forth_word) fresh)
(prf_main : fresh ≠ 0)

namespace forth_program
  def empty : forth_program :=
    forth_program.mk 1 (vector.cons list.nil vector.nil) (nat.succ_ne_zero _)

  def define : list forth_word → forth_program → forth_program :=
    λnew_word old,
      forth_program.mk
        (nat.succ (forth_program.fresh old))
        (vector.snoc new_word (forth_program.defs old))
        (nat.succ_ne_zero _)
end forth_program

structure forth_pc (prg : forth_program) :=
(def_idx : fin (forth_program.fresh prg))
(word_idx : fin (nat.succ (list.length (vector.nth (forth_program.defs prg) def_idx))))

namespace forth_pc
  def advance {prg : forth_program} : forth_pc prg → forth_pc prg :=
    sorry
end forth_pc

inductive forth_value {prg : forth_program} : Type
| addr : forth_pc prg → forth_value
| num  : int → forth_value

structure forth_vm :=
(prg : forth_program)
(pc : forth_pc prg)
(data_stack : list (@forth_value prg))
(return_stack : list (@forth_value prg))
(output : list int)

namespace forth_vm
  def load : forth_program → forth_vm :=
    λprg, mk
      prg
      (forth_pc.mk (fin.zero (forth_program.prf_main prg)) 0)
      list.nil list.nil list.nil

  @[reducible]
  def eval : Type → Type := state_t forth_vm (except string)

  def advance_pc : eval unit :=
    modify (λprev,
      mk (prg prev) (forth_pc.advance (pc prev)) (data_stack prev)
         (return_stack prev) (output prev))

  def cur_word : eval forth_word := do
    vm ← get,
    let pc := pc vm,
    sorry

  def next_word : eval forth_word := do
    word ← cur_word,
    advance_pc,
    return word

  def next : eval unit := do
    word ← next_word,
    return ()
end forth_vm
