import Examples.List
namespace Examples.List

open L

inductive IsPrefixOf {α : Type u} : L α → L α → Prop where
  | base : {ys : L α} → IsPrefixOf Nil ys
  | step : {x y : α} → {xs ys : L α} → x = y → IsPrefixOf xs ys → IsPrefixOf (Cons x xs) (Cons y ys)

open IsPrefixOf

-- Some small proofs

theorem refL (xs : L α) : IsPrefixOf xs xs :=
  match xs with
  | Nil => base
  | Cons _ as => step rfl (refL as)

def transL {xs ys zs : L α} (p₁ : IsPrefixOf xs ys) (p₂ : IsPrefixOf ys zs): IsPrefixOf xs zs :=
  match p₁, p₂ with
  | base, _ => base
  | step a a₁, step b b₁ => step (trans a b) (transL a₁ b₁)
