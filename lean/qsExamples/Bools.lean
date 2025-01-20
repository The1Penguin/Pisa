import Pisa

namespace qsExamples.Bools
#pisa Bool not true false and or
/-
 : not false = true
 : not true = false
(x y : Bool) : and x y = and y x
(x : Bool) : and x x = x
(x y : Bool) : or x y = or y x
(x : Bool) : or x x = x
(x : Bool) : and x false = false
(x : Bool) : and x true = x
(x : Bool) : or x false = x
(x : Bool) : or x true = true
(x : Bool) : not (not x) = x
(x : Bool) : and x (not x) = false
(x : Bool) : or x (not x) = true
(x y z : Bool) : and x (and y z) = and y (and x z)
(x y : Bool) : and x (or x y) = x
(x y : Bool) : or x (and x y) = x
(x y z : Bool) : or x (or y z) = or y (or x z)
(x y : Bool) : and (not x) (not y) = not (or x y)
(x y : Bool) : and (not x) (or x y) = and y (not x)
(x y z : Bool) : and (or x y) (or x z) = or x (and y z)
(x : Bool) : or false x = x
(x : Bool) : and true x = x
(x y : Bool) : or (not x) (not y) = not (and x y)
(x y z : Bool) : or (or x y) z = or x (or y z)
(x y z : Bool) : and (and x y) z = and x (and y z)
-/
/-
== Functions ==
  not :: Bool -> Bool
 True :: Bool
False :: Bool
 (||) :: Bool -> Bool -> Bool
 (&&) :: Bool -> Bool -> Bool

== Laws ==
  1. true = not false
  2. false = not true
  3. and x y = and y x
  4. and x x = x
  5. or x y = or y x
  6. or x x = x
  7. and x false = x
  8. and x true = true
  9. or x false = false
 10. or x true = x
 11. not (not x) = x
 12. and x (not x) = true
 13. or x (not x) = false
 14. and (and x y) z = and x (and y z)
 15. and x (or x y) = x
 16. or x (and x y) = x
 17. or (or x y) z = or x (or y z)
 18. and (not x) (not y) = not (or x y)
 19. and (not x) (or x y) = and y (not x)
 20. and (or x y) (or x z) = or x (and y z)
-/
