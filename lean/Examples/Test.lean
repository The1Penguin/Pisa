import Examples.Nat
import Examples.Bool
inductive Dat where
  | a : Dat
  | b : Dat
  | c : Dat
  | d : Dat
  | e : Dat
  | f : Dat
  | g : Dat
  | h : Dat

structure Trip (α : Type u) where
  mk ::
  fst : α
  snd : α
  trd : α

inductive Trip' (α : Type u) where
  | mk : (fst : α) → (snd : α) → (trd : α) → Trip' α

inductive Trips (α : Type) (β : Type) (γ : Type) where
  | mk : (fst : α) → (snd : β) → (trd : γ) → Trips α β γ
  | emp : Trips α β γ

inductive Trips' {t : Sort 𝔏} (α : t) (β : t) (γ : t) where
  | mk : {δ: Sort v} → (nil: δ) → (fst : t) → (snd : t) → (trd : t) → Trips' α β γ
  | emp : Trips' α β γ

inductive U where
  | u : U

inductive V : Prop where
  | v : V

inductive W : Type u where
  | w : W

inductive OneCons (α : Type) where
  | TheCons : OneCons α
  | TheDyn : α → OneCons α

def matchCons : OneCons α → OneCons U
  | .TheCons => .TheCons
  | .TheDyn _ => .TheDyn .u

def matchCons' := @matchCons W

#reduce IO.println "hi"

def typA := W → W

def aliased : W → W
  | .w => match U.u with
    | .u => W.w

def toy := True || False

def bar : True := trivial

theorem bar' : True := trivial
def one_plus_one_eq_two : 1 + 1 = 2 := rfl

theorem t : (True → True) ∧ (False → True) :=
  ⟨fun h : True => h, fun _ => trivial⟩

def monadTest [Monad m] : m U := do
  let a := (← pure W.w)
  pure U.u

def aliasd : typA
  | a => a


def explicit :=
  match (motive := (n : U) → n = U.u → String) U.u, rfl with
  | U.u, rfl => "ok"

def ali (a : Prop) (b : a) : U := U.u
#check ali V V.v
#check False
#check true

inductive Tre (α : Type) where
  | End : Tre α
  | Split (a : Tre α) (b : Tre α) : Tre α

#check
  let m {α} (t : Tre α) := match t with
    | .End => U
    | .Split _ _ => W

  @Tre.rec U m

inductive Siple (σ : Type) where
  | S (s : σ) : Siple σ

inductive Deep where
  | Deep (a : Siple Deep) : Deep
  | Shallow (b : Tre Deep) (a : Siple Deep) : Deep
  | Bot : Deep
  /- | Deep (d : Siple (Siple Deep)) (a: Siple Deep) : Deep -/

def deep (d : Deep) := match d with
  | .Bot => U.u
  | .Deep _ => U.u
  | .Shallow _ _ => U.u

#check @Deep.rec -- (λ_ => U)

def «mås» := 1
def mk {α : Type u} (a : α) (b : α) (c : α) := Trip.mk a b c

def mks := @Trips'.mk Type Nat Bool Bool Unit
def mks' := @Trips'.mk Sort true false false U

/- def mks := @Trips.mk Nat String -/
/- def t0 := mks 1 "hej" 3 -/
/- def t1 := Trips.mk 1 "hej" 3 -/

def a := mk U.u U.u U.u
def b := Trip'.mk U.u a.1 U.u

def projd := a.1

def c := [U.u].map id


def typ (a : Type) (b: a) : a := b

def mtyp := typ _ 3

namespace Testsak
mutual
  def even : Nat → Bool
    | 0   => true
    | n+1 => odd n

  def odd : Nat → Bool
    | 0   => false
    | n+1 => even n
end

namespace Examples.Test

inductive P where
  | fst : Examples.Nat.N → Examples.Nat.N → P
  | snd : Examples.Nat.N → Examples.Nat.N → P

set_option trace.compiler.ir.result true in
def swap : P → P
  | .fst n b => .snd b n
  | .snd b n => .fst n b
