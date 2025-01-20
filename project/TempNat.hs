{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TempNat where

import Data.Map.Lazy (Map, fromList)
import GHC.Generics (Generic)
import Lean.Eval
import Lean.IR
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Poly (OrdA (..), OrdB (..), OrdC (..))

environment :: Map Name Decl
environment = fromList [("Examples.Nat.add", FDecl {f = "Examples.Nat.add", xs = [Param {x = VarId {idx = 1}, borrow = True, typ = Obj}, Param {x = VarId {idx = 2}, borrow = False, typ = Obj}], typ = Obj, bod = Case {tid = "Examples.Nat.N", x = VarId {idx = 2}, xType = Obj, cs = [Ctor {info = CtorInfo {name = "Examples.Nat.N.Z", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Inc {x = VarId {idx = 1}, n = 1, c = True, persistent = False, b = Ret {r = Var {i = VarId {idx = 1}}}}}, Ctor {info = CtorInfo {name = "Examples.Nat.N.S", cidx = 1, size = 1, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = IsShared {x = VarId {idx = 2}}, b = Case {tid = "Bool", x = VarId {idx = 3}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Bool.false", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 4}, typ = Obj, e = Proj {i = 0, x = VarId {idx = 2}}, b = Vdecl {x = VarId {idx = 5}, typ = Obj, e = Fap {c = "Examples.Nat.add", ys = [Var {i = VarId {idx = 1}}, Var {i = VarId {idx = 4}}]}, b = Set {x = VarId {idx = 2}, i = 0, a = Var {i = VarId {idx = 5}}, b = Ret {r = Var {i = VarId {idx = 2}}}}}}}, Ctor {info = CtorInfo {name = "Bool.true", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 6}, typ = Obj, e = Proj {i = 0, x = VarId {idx = 2}}, b = Inc {x = VarId {idx = 6}, n = 1, c = True, persistent = False, b = Dec {x = VarId {idx = 2}, n = 1, c = True, persistent = False, b = Vdecl {x = VarId {idx = 7}, typ = Obj, e = Fap {c = "Examples.Nat.add", ys = [Var {i = VarId {idx = 1}}, Var {i = VarId {idx = 6}}]}, b = Vdecl {x = VarId {idx = 8}, typ = Obj, e = ECtor {info = CtorInfo {name = "Examples.Nat.N.S", cidx = 1, size = 1, usize = 0, ssize = 0}, ys = [Var {i = VarId {idx = 7}}]}, b = Ret {r = Var {i = VarId {idx = 8}}}}}}}}}]}}}]}, info = DeclInfo {sorryDep = Nothing}})]

data T'Examples_Nat_N where
  C'Examples_Nat_N_Z :: T'Examples_Nat_N
  C'Examples_Nat_N_S :: T'Examples_Nat_N -> T'Examples_Nat_N
  deriving (Eq)
  deriving (Generic)
  deriving (Ord)
  deriving (Show)
  deriving (Arbitrary) via (GenericArbitrary T'Examples_Nat_N)

v'toValExamples_Nat_N :: T'Examples_Nat_N -> Val
v'toValExamples_Nat_N =
  \(v'a) ->
    case (v'a) of
      (C'Examples_Nat_N_Z) -> VCtor (Object 1 0) []
      (C'Examples_Nat_N_S v'b) ->
        VCtor (Object 1 1) ((:) (v'toValExamples_Nat_N v'b) [])

v'fromValExamples_Nat_N :: Val -> T'Examples_Nat_N
v'fromValExamples_Nat_N =
  \(v'a :: Val) ->
    case (v'a) of
      (VCtor (Object _ 0) ([])) -> C'Examples_Nat_N_Z
      (VCtor (Object _ 1) ((:) v'b ([]))) ->
         C'Examples_Nat_N_S (v'fromValExamples_Nat_N v'b)

v'Examples_Nat_add = FDecl {f = "Examples.Nat.add", xs = [Param {x = VarId {idx = 1}, borrow = True, typ = Obj}, Param {x = VarId {idx = 2}, borrow = False, typ = Obj}], typ = Obj, bod = Case {tid = "Examples.Nat.N", x = VarId {idx = 2}, xType = Obj, cs = [Ctor {info = CtorInfo {name = "Examples.Nat.N.Z", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Inc {x = VarId {idx = 1}, n = 1, c = True, persistent = False, b = Ret {r = Var {i = VarId {idx = 1}}}}}, Ctor {info = CtorInfo {name = "Examples.Nat.N.S", cidx = 1, size = 1, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = IsShared {x = VarId {idx = 2}}, b = Case {tid = "Bool", x = VarId {idx = 3}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Bool.false", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 4}, typ = Obj, e = Proj {i = 0, x = VarId {idx = 2}}, b = Vdecl {x = VarId {idx = 5}, typ = Obj, e = Fap {c = "Examples.Nat.add", ys = [Var {i = VarId {idx = 1}}, Var {i = VarId {idx = 4}}]}, b = Set {x = VarId {idx = 2}, i = 0, a = Var {i = VarId {idx = 5}}, b = Ret {r = Var {i = VarId {idx = 2}}}}}}}, Ctor {info = CtorInfo {name = "Bool.true", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 6}, typ = Obj, e = Proj {i = 0, x = VarId {idx = 2}}, b = Inc {x = VarId {idx = 6}, n = 1, c = True, persistent = False, b = Dec {x = VarId {idx = 2}, n = 1, c = True, persistent = False, b = Vdecl {x = VarId {idx = 7}, typ = Obj, e = Fap {c = "Examples.Nat.add", ys = [Var {i = VarId {idx = 1}}, Var {i = VarId {idx = 6}}]}, b = Vdecl {x = VarId {idx = 8}, typ = Obj, e = ECtor {info = CtorInfo {name = "Examples.Nat.N.S", cidx = 1, size = 1, usize = 0, ssize = 0}, ys = [Var {i = VarId {idx = 7}}]}, b = Ret {r = Var {i = VarId {idx = 8}}}}}}}}}]}}}]}, info = DeclInfo {sorryDep = Nothing}}

sigs :: [Sig]
sigs =
  (:)
    (monoType ((Proxy :: Proxy T'Examples_Nat_N)))
    ( (:)
        ( con
            "Examples.Nat.add"
            ( \(v'a) ->
                \(v'b) ->
                  v'fromValExamples_Nat_N
                    ( eval
                        v'Examples_Nat_add
                        environment
                        ( (:)
                            (v'toValExamples_Nat_N v'a)
                            ((:) (v'toValExamples_Nat_N v'b) [])
                        )
                    )
            )
        )
        []
    )
