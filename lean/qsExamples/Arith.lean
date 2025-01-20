import Pisa

namespace qsExamples.Arith
open Nat (add mul)
def O := 0
def I := 1
#pisa Nat O I add mul
/-
(x y : Nat) : add x y = add y x
(x y : Nat) : mul x y = mul y x
(x : Nat) : add x O = x
(x : Nat) : mul x I = x
(x : Nat) : mul x O = O
(x y z : Nat) : add x (add y z) = add y (add x z)
(x y : Nat) : mul x (add y y) = mul y (add x x)
(x y z : Nat) : mul x (mul y z) = mul y (mul x z)
(x y : Nat) : mul x (add y I) = add x (mul x y)
(x y z : Nat) : add (mul x y) (mul x z) = mul x (add y z)
(x y : Nat) : mul x (add y (add y y)) = mul y (add x (add x x))
(x : Nat) : add O x = x
(x : Nat) : mul I x = x
(x y z : Nat) : add (add x y) z = add x (add y z)
(x y z : Nat) : mul (mul x y) z = mul x (mul y z)
-/
/-
(x y : Nat) : add x y = add y x
(x y : Nat) : mul x y = mul y x
(x : Nat) : add x O = x
(x : Nat) : mul x I = x
(x : Nat) : mul x O = O
(x y z : Nat) : add x (add y z) = add y (add x z)
(x y : Nat) : mul x (add y y) = mul y (add x x)
(x y z : Nat) : mul x (mul y z) = mul y (mul x z)
(x y : Nat) : mul x (add y I) = add x (mul x y)
(x y z : Nat) : add (mul x y) (mul x z) = mul x (add y z)
(x y : Nat) : mul x (add y (add y y)) = mul y (add x (add x x))
(x : Nat) : add O x = x
(x : Nat) : mul I x = x
(x y z : Nat) : add (add x y) z = add x (add y z)
(x y z : Nat) : mul (mul x y) z = mul x (mul y z)
-/
