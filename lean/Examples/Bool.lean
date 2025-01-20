namespace Examples.Bool

inductive B where
  | t : B
  | f : B

open B

def true := t

def not (b : B) : B :=
  match b with
  | t => f
  | f => t

def and : B → B → B :=
  λ a b => match a with
            | t => b
            | f => f

def or : B → B → B := λ
  | t, _ => t
  | f, b => b

def or' : B → B → B := λ
  | t => λ _ => t
  | f => λ b => b

-- Example proofs

theorem double_neg (b : B) : not (not b) = b :=
  match b with
  | t => rfl
  | f => rfl

theorem de_morgan₁ (a b : B) : not (and a b) = or (not a) (not b) :=
  match a with
  | t => rfl
  | f => rfl

theorem de_morgan₂ (a b : B) : not (or a b) = and (not a) (not b) :=
  match a with
  | t => rfl
  | f => rfl

theorem and_com (a b : B) : and a b = and b a :=
  match a, b with
    | t, t => rfl
    | t, f => rfl
    | f, t => rfl
    | f, f => rfl

theorem or_com (a b : B) : and a b = and b a :=
  match a, b with
    | t, t => rfl
    | t, f => rfl
    | f, t => rfl
    | f, f => rfl

theorem and_ass (a b c : B) : and (and a b) c = and a (and b c) :=
  match a, b with
    | t, t => rfl
    | t, f => rfl
    | f, t => rfl
    | f, f => rfl

theorem or_ass (a b c : B) : or (or a b) c = or a (or b c) :=
  match a, b with
    | t, t => rfl
    | t, f => rfl
    | f, t => rfl
    | f, f => rfl

theorem or_f (a : B) : or a f = a :=
  match a with
  | t => rfl
  | f => rfl

theorem and_t (a : B) : and a t = a :=
  match a with
  | t => rfl
  | f => rfl
