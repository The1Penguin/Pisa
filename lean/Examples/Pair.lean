namespace Examples.Pair

inductive P (α β : Type u) where
  | mk : α → β → P α β

def swap : P α β → P β α
  | .mk a b => .mk b a


inductive Tri (α β γ : Type u) where
  | mk : α → β → γ → Tri α β γ

def left : Tri α β γ → Tri β γ α
  | .mk a b c => .mk b c a

def right : Tri α β γ → Tri γ α β
  | .mk a b c => .mk c a b


inductive Q (α β γ δ : Type u) where
  | mk : α → β → γ → δ → Q α β γ δ

def Q.left : Q α β γ δ → Q β γ δ α
  | .mk a b c d => .mk b c d a

def Q.right : Q α β γ δ → Q δ α β γ
  | .mk a b c d => .mk d a b c
