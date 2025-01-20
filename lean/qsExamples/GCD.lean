import Pisa
import qsExamples.Arith

namespace qsExamples.GCD
open Nat (gcd)
open qsExamples.Arith
#pisa O I add mul gcd
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
/-
  1. x * y = y * x
  2. x + y = y + x
  3. gcd x y = gcd y x
  4. gcd x x = x
  5. x * 0 = 0
  6. x * 1 = x
  7. x + 0 = x
  8. gcd x 0 = x
  9. gcd x 1 = 1
 10. (x * y) * z = x * (y * z)
 11. x * (y + y) = y * (x + x)
 12. (x + y) + z = x + (y + z)
 13. gcd x (x * y) = x
 14. gcd x (x + y) = gcd x y
 15. gcd (gcd x y) z = gcd x (gcd y z)
 16. x * (y + 1) = x + (x * y)
 17. (x * y) + (x * z) = x * (y + z)
 18. gcd (x * y) (x * z) = x * gcd y z
 19. gcd (x * x) (y * y) = gcd x y * gcd x y
 20. gcd (x * y) (z + y) = gcd (x * z) (z + y)
 21. gcd (x + x) (y + y) = gcd x y + gcd x y
 22. gcd (x + y) (y + y) = gcd (x + x) (x + y)
 23. gcd (x * x) (1 + 1) = gcd x (1 + 1)
-/
