Require Import List Nat String Sumbool.
Set Implicit Arguments.

Definition fmap {a b : Set} (f : a -> b) (x : option a) : option b :=
  match x with
  | Some x => Some (f x)
  | None   => None
  end.

Definition ap {a b : Set} (f : option (a -> b)) (x : option a) :=
  match f, x with
  | Some f', Some x' => Some (f' x')
  | _,       _       => None
  end.

Notation "F <$> X" := (fmap F X) (at level 40, left associativity).
Notation "F <*> X" := (ap   F X) (at level 40, left associativity).

Fixpoint nth {a : Set} (n : nat) (l : list a) : option a :=
  match n, l with
  | O,   h::_ => Some h
  | S n, _::t => nth n t
  | _,   _  => None
  end.

Inductive ty : Set :=
| Arr   : ty -> ty -> ty
| UnitT : ty.

Fixpoint eq_ty (a b : ty) : bool :=
  match a, b with
  | Arr lx ly, Arr rx ry => eq_ty lx rx && eq_ty ly ry
  | UnitT,     UnitT     => true
  | _,         _         => false
  end.

Inductive tm : Set :=
| App   : tm -> tm -> tm
| Lam   : string -> ty -> tm -> tm
| UnitV : tm
| Var   : string -> tm.

Definition empty : string -> option ty := fun _ => None.
Definition extend (ctx : string -> option ty) (name : string) (type : ty) :=
  fun n => if string_dec n name then Some type else ctx n.

Inductive typing (ctx : string -> option ty) : tm -> ty -> Prop :=
| TypingApp : forall f x t t', typing ctx f (Arr t t')
                            -> typing ctx x t
                            -> typing ctx (App f x) t'
| TypingLam : forall b n t t', typing (extend ctx n t) b t'
                            -> typing ctx (Lam n t b) (Arr t t')
| TypingUnit : typing ctx UnitV UnitT
| TypingVar : forall n t, (ctx n = Some t) -> typing ctx (Var n) t.

Hint Constructors typing.

Inductive value : tm -> Prop :=
| ValueLam : forall b n t, value (Lam n t b)
| ValueUnit : value UnitV.

Hint Constructors value.

Reserved Notation "'[' x ':=' s ']' t" (at level 20).

Fixpoint subst (x : string) (s : tm) (t : tm) : tm :=
  match t with
  | t => t
  end.

Notation "'[' x ':=' s ']' t" := (subst x s t) (at level 20).

Inductive step : tm -> tm -> Prop :=
| StepApp1 : forall f f' x, step f f' -> step (App f x) (App f' x)
| StepApp2 : forall f x x', value f -> step x x' -> step (App f x) (App f x')
| StepBeta : forall b n t x, value x -> step (App (Lam n t b) x) ([n:=x]b).

Hint Constructors step.

Theorem progress : forall e T,
  typing empty e T ->
  value e \/ exists e', step e e'.
Proof.
  intros e T H.
  induction H; auto.
  - admit.
  - admit.
Admitted.

Inductive fv : string -> tm -> Prop :=
| FVApp1 : forall e e' n, fv n e  -> fv n (App e e')
| FVApp2 : forall e e' n, fv n e' -> fv n (App e e')
| FVLam : forall b n n', bool_of_sumbool (string_dec n n') = false -> fv n b -> fv n (Lam n t b)
| FVVar : forall n, fv n (Var n).

Hint Constructors ty.

Theorem preservation : forall e e' T,
  step e e' ->
  typing empty e  T ->
  typing empty e' T.
Proof.
  intros e e' T Step Ty.
  induction Step; auto.
  - apply (TypingApp _ _).
