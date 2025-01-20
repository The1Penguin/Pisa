namespace Examples.Vector

inductive Vector (α : Type u) : Nat -> Type u where
    | Nil  : Vector α 0
    | Cons : α -> Vector α n -> Vector α (n+1)

open Vector

def append (v : Vector α n)
           (w : Vector α m) :
           Vector α (m+n) :=
    match v with
    | Nil => w
    | Cons a v' => Cons a (append v' w)

def head {n : Nat} :
         Vector α (.succ n) ->
         α
    | Cons a _ => a



theorem nil_append (xs : Vector α n) : append Nil xs = xs := rfl

theorem cons_append (x : α) (xs : Vector α n) (ys : Vector α m) :
        append (Cons x xs) ys = Cons x (append xs ys) := rfl
