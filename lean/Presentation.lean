import Pisa

namespace Presentation

inductive B where
  | t : B
  | f : B

def not (b : B) : B :=
  match b with
  | .t => .f
  | .f => .t

def and : B → B → B :=
  λ a b => match a with
            | .t => b
            | .f => .f

def or : B → B → B := λ
  | .t, _ => .t
  | .f, b => b

#pisa 7 B B.t B.f not and or

















inductive N where
  | Z : N
  | S : N → N

def add (n m : N) : N :=
  match m with
  | .Z    => n
  | .S m' => .S (add n m')

def mult (n m : N) : N :=
  match m with
  | .Z    => .Z
  | .S m' => add n (mult n m')

#pisa 7 N N.Z N.S add mult















inductive L (α : Type) where
  | Nil : L α
  | Cons : α → L α → L α


def append (l₁ l₂ : L α) : (L α) :=
  match l₁ with
  | .Nil       => l₂
  | .Cons a as => .Cons a (append as l₂)


def reverse (l : L α) : (L α) :=
  match l with
  | .Nil       => .Nil
  | .Cons a as => append (reverse as) (.Cons a .Nil)

#pisa 7 L L.Nil L.Cons append reverse
