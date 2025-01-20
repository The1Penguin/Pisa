import Examples.Bool
namespace Examples.Nat

inductive N where
  | Z : N
  | S : N → N

open N

def one := S Z

def add (n m : N) : N :=
  match m with
  | Z => n
  | S m' => S (add n m')

def mult (n m : N) : N :=
  match m with
  | .Z => .Z
  | .S m' => add n (mult n m')

unsafe def sub : N → N → N
  | .Z, .Z     => .Z
  | .Z, _      => sorry
  | n, .Z      => n
  | .S n, .S m => sub n m

-- Example proofs
theorem add_z (n : N) : add n Z = n := rfl

theorem add_s (n : N) : add n (S Z) = S n := rfl

theorem add_succ (n m : N) : add n (S m) = S (add n m) := rfl
theorem succ_add (n m : N) : add (S n) m = S (add n m) :=
  N.recOn
  m
  rfl
  (λ _ ih => by simp [add_succ, ih])

theorem z_add (n : N) : add Z n = n :=
  N.recOn
  n
  rfl
  (λ n ih => by simp [ih,add_succ])

theorem assoc_add (x y z : N) : add (add x y) z = add x (add y z) :=
  N.recOn
  z
  rfl
  (λ _ ih => by simp [ih,add_succ])

theorem comm_add (n m : N) : add n m = add m n :=
  N.recOn
  m
  (by simp [z_add,add_z])
  (λ _ ih => by simp [add_succ,succ_add,ih])
