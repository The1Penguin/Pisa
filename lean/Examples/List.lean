namespace Examples.List

inductive L (α : Type u) where
  | Nil : L α
  | Cons : α → L α → L α

open L

def append {α : Type u} (l₁ : L α) (l₂ : L α) : (L α) :=
  match l₁ with
  | Nil => l₂
  | Cons a as => Cons a (append as l₂)


def reverse {α : Type u} (l : L α) : (L α) :=
  match l with
  | Nil => Nil
  | Cons a as => append (reverse as) (Cons a Nil)

def length {α : Type u} (l : L α) : Nat :=
  match l with
  | Nil => 0
  | Cons _ as => 1 + length as

def map {α β : Type u} (f : α → β) : L α → L β
  | .Nil => .Nil
  | .Cons a as => .Cons (f a) (map f as)

def bools := Cons true (Cons false Nil)

def indeterminate {β : Type u} := @append β Nil Nil


-- Example proofs
theorem nil_append (xs : L α) : append Nil xs = xs :=
  rfl

theorem cons_append (x : α) (xs ys : L α) :
  append (Cons x xs) ys = Cons x (append xs ys) :=
  rfl

theorem append_nil (xs : L α) : append xs Nil = xs :=
  L.recOn
  (motive := λ ys => append ys Nil = ys)
  xs
  rfl
  (λ x ys ih => by simp [cons_append, ih])

theorem append_assoc (xs ys zs : L α) :
  append (append xs ys) zs = append xs (append ys zs) :=
    L.recOn
    (motive := λ as => append (append as ys) zs = append as (append ys zs))
    xs
    rfl
    (λ a as ih =>
      show append (append (Cons a as) ys) zs = append (Cons a as) (append ys zs) from
      calc append (append (Cons a as) ys) zs
        _ = append (Cons a (append as ys)) zs := by rw [cons_append]
        _ = Cons a (append (append as ys) zs) := by rw [cons_append]
        _ = Cons a (append as (append ys zs)) := by rw [ih])

theorem reverse_nil : reverse (Nil : L α) = Nil := rfl

theorem reverse_cons (x : α) (xs : L α) :
  reverse (Cons x xs) = append (reverse xs) (Cons x Nil) :=
    rfl

theorem reverse_append (xs ys : L α) :
  reverse (append xs ys) = append (reverse ys) (reverse xs) :=
    L.recOn
    xs
    (by simp [nil_append, reverse_nil, append_nil])
    (λ a as ih => by simp [cons_append,reverse_cons,←append_assoc,←ih])

theorem reverse_reverse (xs : L α) : reverse (reverse xs) = xs :=
  L.recOn
  xs
  rfl
  (λ a as ih => by
  simp [reverse_cons,reverse_append,ih,reverse_nil,nil_append,cons_append])

theorem length_nil {α : Type u} : length (α := α) Nil = 0 := rfl

theorem length_cons {x : α} (xs : L α) : length (Cons x xs) = 1 + length xs := rfl

theorem length_append (xs ys : L α) : length (append xs ys) = length xs + length ys :=
  L.recOn
  xs
  (calc length (append Nil ys)
    _ = length ys := by rw [nil_append]
    _ = 0 + length ys := by simp)
  (λ a as ih => by simp [cons_append,length_cons,ih,Nat.add_assoc])

theorem length_reverse (xs : L α) : length (reverse xs) = length xs :=
  L.recOn
  xs
  rfl
  (λ a as ih => by simp [reverse_cons,length_append,length_cons,length_nil,ih,Nat.add_comm])
