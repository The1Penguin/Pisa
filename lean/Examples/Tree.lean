namespace Examples.Tree

inductive T (α : Type u) where
  | Leaf : α → T α
  | Node : T α → T α → T α

def swap : T α → T α
  | .Leaf x => .Leaf x
  | .Node l r => .Node (swap r) (swap l)

def leftmost : T α → α
  | .Leaf x => x
  | .Node l _ => leftmost l

def rightmost : T α → α
  | .Leaf x => x
  | .Node _ r => rightmost r
