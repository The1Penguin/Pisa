import Pisa
import Examples.Bool
import Examples.Nat
import Examples.List
import Examples.Tree
import Examples.Pair

namespace Examples.Bool

#pisa 7 B B.t B.f not and or

-- Comparing to https://leanprover-community.github.io/mathlib4_docs/Init/SimpLemmas.html#Bool.or_false
-- theorem conjecture0  : not (B.f : B) = (B.t : B) := sorry
-- -- theorem Bool.not_false : (!false) = true
-- theorem conjecture1  : not (B.t : B) = (B.f : B) := sorry
-- -- theorem Bool.not_true  : (!true) = false
-- theorem conjecture2 (x y : B) : and x y = and y x := sorry
-- theorem conjecture3 (x : B) : and x x = x := sorry
-- -- theorem Bool.and_self (b : Bool) : (b && b) = b
-- theorem conjecture4 (x y : B) : or x y = or y x := sorry
-- theorem conjecture5 (x : B) : or x x = x := sorry
-- -- theorem Bool.or_self (b : Bool) : (b || b) = b
-- theorem conjecture6 (x : B) : and x (B.f : B) = (B.f : B) := sorry
-- -- theorem Bool.and_false (b : Bool) : (b && false) = false
-- theorem conjecture7 (x : B) : and x (B.t : B) = x := sorry
-- -- theorem Bool.and_true (b : Bool) : (b && true) = b
-- theorem conjecture8 (x : B) : or x (B.f : B) = x := sorry
-- -- theorem Bool.or_false (b : Bool) : (b || false) = b
-- theorem conjecture9 (x : B) : or x (B.t : B) = (B.t : B) := sorry
-- -- theorem Bool.or_true (b : Bool) : (b || true) = true
-- theorem conjecture10 (x : B) : not (not x) = x := sorry
-- -- theorem Bool.not_not (b : Bool) : (!!b) = b
-- theorem conjecture11 (x : B) : and x (not x) = (B.f : B) := sorry
-- theorem conjecture12 (x : B) : or x (not x) = (B.t : B) := sorry
-- theorem conjecture13 (x y z : B) : and x (and y z) = and y (and x z) := sorry
-- theorem conjecture14 (x y : B) : and x (or x y) = x := sorry
-- theorem conjecture15 (x y : B) : or x (and x y) = x := sorry
-- theorem conjecture16 (x y z : B) : or x (or y z) = or y (or x z) := sorry
-- theorem conjecture17 (x y : B) : and (not x) (not y) = not (or x y) := sorry
-- theorem conjecture18 (x y : B) : and (not x) (or x y) = and y (not x) := sorry
-- theorem conjecture19 (x y z : B) : and (or x y) (or x z) = or x (and y z) := sorry
-- theorem conjecture20 (x : B) : and (B.t : B) x = x := sorry
-- -- theorem Bool.true_and (b : Bool) : (true && b) = b
-- theorem conjecture21 (x : B) : or (B.f : B) x = x := sorry
-- -- theorem Bool.false_or (b : Bool) : (false || b) = b
-- theorem conjecture22 (x y : B) : or (not x) (not y) = not (and x y) := sorry
-- theorem conjecture23 (x y z : B) : and (and x y) z = and x (and y z) := sorry
-- -- theorem Bool.and_assoc (a b c : Bool) : (a && b && c) = (a && (b && c))
-- theorem conjecture24 (x y z : B) : or (or x y) z = or x (or y z) := sorry
-- -- theorem Bool.or_assoc (a b c : Bool) : (a || b || c) = (a || (b || c))

-- Missing are the following
-- --theorem Bool.true_or (b : Bool) : (true || b) = true
-- --theorem Bool.false_and (b : Bool) : (false && b) = false



-- Comparing to https://leanprover-community.github.io/mathlib4_docs/Init/Data/Bool.html#and and https://leanprover-community.github.io/mathlib4_docs/Init/Data/Bool.html#or and https://leanprover-community.github.io/mathlib4_docs/Init/Data/Bool.html#distributivity
-- theorem conjecture0  : not (B.f : B) = (B.t : B) := sorry
-- theorem conjecture1  : not (B.t : B) = (B.f : B) := sorry
-- theorem conjecture2 (x y : B) : and x y = and y x := sorry
-- -- theorem and_comm : ∀ (x y : Bool), (x && y) = (y && x)
-- theorem conjecture3 (x : B) : and x x = x := sorry
-- theorem conjecture4 (x y : B) : or x y = or y x := sorry
-- -- theorem or_comm : ∀ (x y : Bool), (x || y) = (y || x)
-- theorem conjecture5 (x : B) : or x x = x := sorry
-- theorem conjecture6 (x : B) : and x (B.f : B) = (B.f : B) := sorry
-- theorem conjecture7 (x : B) : and x (B.t : B) = x := sorry
-- theorem conjecture8 (x : B) : or x (B.f : B) = x := sorry
-- theorem conjecture9 (x : B) : or x (B.t : B) = (B.t : B) := sorry
-- theorem conjecture10 (x : B) : not (not x) = x := sorry
-- theorem conjecture11 (x : B) : and x (not x) = (B.f : B) := sorry
-- -- theorem and_not_self : ∀ (x : Bool), (x && !x) = false
-- theorem conjecture12 (x : B) : or x (not x) = (B.t : B) := sorry
-- -- theorem or_not_self : ∀ (x : Bool), (x || !x) = true
-- theorem conjecture13 (x y z : B) : and x (and y z) = and y (and x z) := sorry
-- -- theorem and_left_comm : ∀ (x y z : Bool), (x && (y && z)) = (y && (x && z))
-- theorem conjecture14 (x y : B) : and x (or x y) = x := sorry
-- theorem conjecture15 (x y : B) : or x (and x y) = x := sorry
-- theorem conjecture16 (x y z : B) : or x (or y z) = or y (or x z) := sorry
-- -- theorem or_left_comm : ∀ (x y z : Bool), (x || (y || z)) = (y || (x || z))
-- theorem conjecture17 (x y : B) : and (not x) (not y) = not (or x y) := sorry
-- -- theorem not_or : ∀ (x y : Bool), (!(x || y)) = (!x && !y)
-- theorem conjecture18 (x y : B) : and (not x) (or x y) = and y (not x) := sorry
-- theorem conjecture19 (x y z : B) : and (or x y) (or x z) = or x (and y z) := sorry
-- -- theorem or_and_distrib_left  : ∀ (x y z : Bool), (x || y && z) = ((x || y) && (x || z))
-- theorem conjecture20 (x : B) : and (B.t : B) x = x := sorry
-- theorem conjecture21 (x : B) : or (B.f : B) x = x := sorry
-- theorem conjecture22 (x y : B) : or (not x) (not y) = not (and x y) := sorry
-- -- theorem not_and : ∀ (x y : Bool), (!(x && y)) = (!x || !y)
-- theorem conjecture23 (x y z : B) : and (and x y) z = and x (and y z) := sorry
-- theorem conjecture24 (x y z : B) : or (or x y) z = or x (or y z) := sorry

-- Missing are the following
-- -- theorem and_self_left  : ∀ (a b : Bool), (a && (a && b)) = (a && b)
-- -- theorem and_self_right : ∀ (a b : Bool), ((a && b) && b) = (a && b)
-- -- theorem not_and_self : ∀ (x : Bool), (!x && x) = false
-- -- theorem and_right_comm : ∀ (x y z : Bool), ((x && y) && z) = ((x && z) && y)
-- -- theorem or_self_left  : ∀ (a b : Bool), (a || (a || b)) = (a || b)
-- -- theorem or_self_right : ∀ (a b : Bool), ((a || b) || b) = (a || b)
-- -- theorem not_or_self : ∀ (x : Bool), (!x || x) = true
-- -- theorem or_right_comm : ∀ (x y z : Bool), ((x || y) || z) = ((x || z) || y)
-- -- theorem and_or_distrib_left  : ∀ (x y z : Bool), (x && (y || z)) = (x && y || x && z)
-- -- theorem and_or_distrib_right : ∀ (x y z : Bool), ((x || y) && z) = (x && z || y && z)
-- -- theorem or_and_distrib_right : ∀ (x y z : Bool), (x && y || z) = ((x || z) && (y || z))

-- Above combined
-- theorem conjecture0  : not (B.f : B) = (B.t : B) := sorry
-- -- theorem Bool.not_false : (!false) = true
-- theorem conjecture1  : not (B.t : B) = (B.f : B) := sorry
-- -- theorem Bool.not_true  : (!true) = false
-- theorem conjecture2 (x y : B) : and x y = and y x := sorry
-- -- theorem and_comm : ∀ (x y : Bool), (x && y) = (y && x)
-- theorem conjecture3 (x : B) : and x x = x := sorry
-- -- theorem Bool.and_self (b : Bool) : (b && b) = b
-- theorem conjecture4 (x y : B) : or x y = or y x := sorry
-- -- theorem or_comm : ∀ (x y : Bool), (x || y) = (y || x)
-- theorem conjecture5 (x : B) : or x x = x := sorry
-- -- theorem Bool.or_self (b : Bool) : (b || b) = b
-- theorem conjecture6 (x : B) : and x (B.f : B) = (B.f : B) := sorry
-- -- theorem Bool.and_false (b : Bool) : (b && false) = false
-- theorem conjecture7 (x : B) : and x (B.t : B) = x := sorry
-- -- theorem Bool.and_true (b : Bool) : (b && true) = b
-- theorem conjecture8 (x : B) : or x (B.f : B) = x := sorry
-- -- theorem Bool.or_false (b : Bool) : (b || false) = b
-- theorem conjecture9 (x : B) : or x (B.t : B) = (B.t : B) := sorry
-- -- theorem Bool.or_true (b : Bool) : (b || true) = true
-- theorem conjecture10 (x : B) : not (not x) = x := sorry
-- -- theorem Bool.not_not (b : Bool) : (!!b) = b
-- theorem conjecture11 (x : B) : and x (not x) = (B.f : B) := sorry
-- -- theorem and_not_self : ∀ (x : Bool), (x && !x) = false
-- theorem conjecture12 (x : B) : or x (not x) = (B.t : B) := sorry
-- -- theorem or_not_self : ∀ (x : Bool), (x || !x) = true
-- theorem conjecture13 (x y z : B) : and x (and y z) = and y (and x z) := sorry
-- -- theorem and_left_comm : ∀ (x y z : Bool), (x && (y && z)) = (y && (x && z))
-- theorem conjecture14 (x y : B) : and x (or x y) = x := sorry
-- theorem conjecture15 (x y : B) : or x (and x y) = x := sorry
-- theorem conjecture16 (x y z : B) : or x (or y z) = or y (or x z) := sorry
-- -- theorem or_left_comm : ∀ (x y z : Bool), (x || (y || z)) = (y || (x || z))
-- theorem conjecture17 (x y : B) : and (not x) (not y) = not (or x y) := sorry
-- -- theorem not_or : ∀ (x y : Bool), (!(x || y)) = (!x && !y)
-- theorem conjecture18 (x y : B) : and (not x) (or x y) = and y (not x) := sorry
-- theorem conjecture19 (x y z : B) : and (or x y) (or x z) = or x (and y z) := sorry
-- -- theorem or_and_distrib_left  : ∀ (x y z : Bool), (x || y && z) = ((x || y) && (x || z))
-- theorem conjecture20 (x : B) : and (B.t : B) x = x := sorry
-- -- theorem Bool.true_and (b : Bool) : (true && b) = b
-- theorem conjecture21 (x : B) : or (B.f : B) x = x := sorry
-- -- theorem Bool.false_or (b : Bool) : (false || b) = b
-- theorem conjecture22 (x y : B) : or (not x) (not y) = not (and x y) := sorry
-- -- theorem not_and : ∀ (x y : Bool), (!(x && y)) = (!x || !y)
-- theorem conjecture23 (x y z : B) : and (and x y) z = and x (and y z) := sorry
-- -- theorem Bool.and_assoc (a b c : Bool) : (a && b && c) = (a && (b && c))
-- theorem conjecture24 (x y z : B) : or (or x y) z = or x (or y z) := sorry
-- -- theorem Bool.or_assoc (a b c : Bool) : (a || b || c) = (a || (b || c))

-- Missing are the following
-- -- theorem Bool.true_or (b : Bool) : (true || b) = true
-- -- theorem Bool.false_and (b : Bool) : (false && b) = false
-- -- theorem and_self_left  : ∀ (a b : Bool), (a && (a && b)) = (a && b)
-- -- theorem and_self_right : ∀ (a b : Bool), ((a && b) && b) = (a && b)
-- -- theorem not_and_self : ∀ (x : Bool), (!x && x) = false
-- -- theorem and_right_comm : ∀ (x y z : Bool), ((x && y) && z) = ((x && z) && y)
-- -- theorem or_self_left  : ∀ (a b : Bool), (a || (a || b)) = (a || b)
-- -- theorem or_self_right : ∀ (a b : Bool), ((a || b) || b) = (a || b)
-- -- theorem not_or_self : ∀ (x : Bool), (!x || x) = true
-- -- theorem or_right_comm : ∀ (x y z : Bool), ((x || y) || z) = ((x || z) || y)
-- -- theorem and_or_distrib_left  : ∀ (x y z : Bool), (x && (y || z)) = (x && y || x && z)
-- -- theorem and_or_distrib_right : ∀ (x y z : Bool), ((x || y) && z) = (x && z || y && z)
-- -- theorem or_and_distrib_right : ∀ (x y z : Bool), (x && y || z) = ((x || z) && (y || z))

-- TP = 22
-- FP = 3
-- FN = 13
-- recall = TP / (TP + FN) = 0.6285714285714286
-- precision = TP / (TP + FP) = 0.88

end Examples.Bool

namespace Examples.Nat

#pisa 7 N N.Z N.S add mult

-- Comparing to https://leanprover-community.github.io/mathlib4_docs/Init/Data/Nat/Basic.html#Nat-add-theorems and https://leanprover-community.github.io/mathlib4_docs/Init/Data/Nat/Basic.html#Nat-mul-theorems
-- theorem conjecture0 (x y : N) : add x y = add y x := sorry
-- -- theorem add_comm : ∀ (n m : Nat), n + m = m + n
-- theorem conjecture1 (x y : N) : mult x y = mult y x := sorry
-- -- theorem mul_comm : ∀ (n m : Nat), n * m = m * n
-- theorem conjecture2 (x : N) : add x (N.Z : N) = x := sorry
-- theorem conjecture3 (x : N) : mult x (N.Z : N) = (N.Z : N) := sorry
-- -- theorem mul_zero (n : Nat) : n * 0 = 0
-- theorem conjecture4 (x y : N) : add x (N.S y) = N.S (add x y) := sorry
-- -- theorem add_succ (n m : Nat) : n + succ m = succ (n + m)
-- theorem conjecture5 (x : N) : mult x (N.S (N.Z : N)) = x := sorry
-- -- theorem mul_one : ∀ (n : Nat), n * 1 = n
-- theorem conjecture6 (x y z : N) : add x (add y z) = add y (add x z) := sorry
-- -- theorem add_left_comm (n m k : Nat) : n + (m + k) = m + (n + k)
-- theorem conjecture7 (x y : N) : add x (mult x y) = mult x (N.S y) := sorry
-- -- theorem mul_succ (n m : Nat) : n * succ m = n * m + n
-- theorem conjecture8 (x y : N) : mult x (add y y) = mult y (add x x) := sorry
-- theorem conjecture9 (x y z : N) : mult x (mult y z) = mult y (mult x z) := sorry
-- -- theorem mul_left_comm (n m k : Nat) : n * (m * k) = m * (n * k)
-- theorem conjecture10 (x y z : N) : add (mult x y) (mult x z) = mult x (add y z) := sorry
-- -- theorem left_distrib (n m k : Nat) : n * (m + k) = n * m + n * k
-- theorem conjecture11 (x : N) : N.S (mult x (N.S (N.S (N.S x)))) = add x (mult (N.S x) (N.S x)) := sorry
-- theorem conjecture12 (x : N) : add (N.Z : N) x = x := sorry
-- -- theorem zero_add : ∀ (n : Nat), 0 + n = n
-- theorem conjecture13 (x y z : N) : add (add x y) z = add x (add y z) := sorry
-- -- theorem add_assoc : ∀ (n m k : Nat), (n + m) + k = n + (m + k)
-- theorem conjecture14 (x y z : N) : mult (mult x y) z = mult x (mult y z) := sorry
-- -- theorem mul_assoc : ∀ (n m k : Nat), (n * m) * k = n * (m * k)

-- Missing are the following
-- -- theorem succ_add : ∀ (n m : Nat), (succ n) + m = succ (n + m)
-- -- theorem add_one (n : Nat) : n + 1 = succ n
-- -- theorem succ_eq_add_one (n : Nat) : succ n = n + 1
-- -- theorem add_right_comm (n m k : Nat) : (n + m) + k = (n + k) + m
-- -- theorem mul_add_one (n m : Nat) : n * (m + 1) = n * m + n
-- -- theorem zero_mul : ∀ (n : Nat), 0 * n = 0
-- -- theorem succ_mul (n m : Nat) : (succ n) * m = (n * m) + m
-- -- theorem add_one_mul (n m : Nat) : (n + 1) * m = (n * m) + m
-- -- theorem one_mul (n : Nat) : 1 * n = n
-- -- theorem right_distrib (n m k : Nat) : (n + m) * k = n * k + m * k
-- -- theorem mul_add (n m k : Nat) : n * (m + k) = n * m + n * k
-- -- theorem add_mul (n m k : Nat) : (n + m) * k = n * k + m * k
-- -- theorem mul_two (n) : n * 2 = n + n
-- -- theorem two_mul (n) : 2 * n = n + n

-- TP = 12
-- FP = 3
-- FN = 14
-- recall = TP / (TP + FN) = 0.46153846153846156
-- precision = TP / (TP + FP) = 0.8

end Examples.Nat

namespace Examples.List

#pisa 7 L L.Nil L.Cons append reverse

-- Comparing to https://leanprover-community.github.io/mathlib4_docs/Init/Data/List/Lemmas.html#reverse and https://leanprover-community.github.io/mathlib4_docs/Init/Data/List/Basic.html#append and https://leanprover-community.github.io/mathlib4_docs/Init/Data/List/Basic.html#reverse
-- theorem conjecture0  : reverse (L.Nil : L α) = (L.Nil : L α) := sorry
-- -- theorem reverse_nil : reverse ([] : List α) = []
-- theorem conjecture1 (x : L α) : append x (L.Nil : L α) = x := sorry
-- -- theorem append_nil (as : List α) : as ++ [] = as
-- theorem conjecture2 (x : L α) : append (L.Nil : L α) x = x := sorry
-- -- theorem nil_append (as : List α) : [] ++ as = as
-- theorem conjecture3 (x : L α) : reverse (reverse x) = x := sorry
-- -- theorem reverse_reverse (as : List α) : as.reverse.reverse = as
-- theorem conjecture4 (x : α) : reverse (L.Cons x (L.Nil : L α)) = L.Cons x (L.Nil : L α) := sorry
-- theorem conjecture5 (x y z : L α) : append (append x y) z = append x (append y z) := sorry
-- -- theorem append_assoc (as bs cs : List α) : (as ++ bs) ++ cs = as ++ (bs ++ cs)
-- theorem conjecture6 (x : α) (y z : L α) : L.Cons x (append y z) = append (L.Cons x y) z := sorry
-- -- theorem cons_append {a : α} {as bs : List α} : (a::as) ++ bs = a::(as ++ bs)
-- theorem conjecture7 (x y : L α) : append (reverse x) (reverse y) = reverse (append y x) := sorry
-- -- theorem reverse_append {as bs : List α} : (as ++ bs).reverse = bs.reverse ++ as.reverse
-- theorem conjecture8 (x : L α) (y z : α) : append x (L.Cons y (L.Cons z (L.Nil : L α))) = reverse (L.Cons z (L.Cons y (reverse x))) := sorry

-- Missing are the following
-- -- theorem append_cons (as : List α) (b : α) (bs : List α) : as ++ b :: bs = as ++ [b] ++ bs
-- -- theorem reverse_cons {a : α} {as : List α} : reverse (a :: as) = reverse as ++ [a]

-- TP = 7
-- FP = 2
-- FN = 2
-- recall = TP / (TP + FN) = 0.7777777777777778
-- precision = TP / (TP + FP) = 0.7777777777777778

end Examples.List

namespace Examples.Pair

#pisa 7 P P.mk swap

#pisa 7 Tri Tri.mk left right

#pisa 7 Q Q.mk Q.left Q.right

end Examples.Pair

namespace Examples.Tree

#pisa 7 T T.Leaf T.Node swap leftmost rightmost

end Examples.Tree
