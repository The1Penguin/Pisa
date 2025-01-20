import Pisa
import qsExamples.Arith

namespace qsExamples.Lists
open List (reverse append nil length join)
open qsExamples.Arith
#pisa List reverse append nil length join O I add
/-
 : join nil = nil
 : reverse nil = nil
 : length nil = O
(x y : Nat) : add x y = add y x
(x : List α) : append x nil = x
(x : List α) : append nil x = x
(x : Nat) : add x O = x
(x : List α) : reverse (reverse x) = x
(x : List α) : length (reverse x) = length x
(x y : List α) : length (append x y) = length (append y x)
(x y z : List α) : append (append x y) z = append x (append y z)
(x y z : Nat) : add x (add y z) = add y (add x z)
(x y : List α) : append (reverse x) (reverse y) = reverse (append y x)
(x y : List α) : add (length x) (length y) = length (append x y)
(x : Nat) : add O x = x
(x y z : Nat) : add (add x y) z = add x (add y z)
-/
/-
  1. O = length nil
  2. reverse nil = nil
  3. join nil = nil
  4. add x y = add y x
  5. add x O = x
  6. append xs nil = xs
  7. append nil xs = xs
  8. length (reverse xs) = length xs
  9. reverse (reverse xs) = xs
 10. length (append xs ys) = length (append ys xs)
 11. length (join (reverse xss)) = length (join xss)
 12. add (add x y) z = add x (add y z)
 13. append (append xs ys) zs = append xs (append ys zs)
 14. add (length xs) (length ys) = length (append xs ys)
 15. append (reverse xs) (reverse ys) = reverse (append ys xs)
 16. append (join xss) (join yss) = join (append xss yss)
-/
