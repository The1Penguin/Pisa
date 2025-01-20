{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TempList where

import Control.DeepSeq (NFData)
import Data.Map.Lazy (Map, fromList)
import GHC.Generics (Generic)
import Lean.Eval
import Lean.IR
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

environment :: Map Name Decl
environment = fromList [("Examples.List.append", FDecl {f = "Examples.List.append", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = Irrelevant}], typ = Obj, bod = Vdecl {x = VarId {idx = 2}, typ = Obj, e = Pap {c = "Examples.List.append._rarg._boxed", ys = []}, b = Ret {r = Var {i = VarId {idx = 2}}}}, info = DeclInfo {sorryDep = Nothing}}), ("Examples.List.append._rarg", FDecl {f = "Examples.List.append._rarg", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = Obj}, Param {x = VarId {idx = 2}, borrow = True, typ = Obj}], typ = Obj, bod = Case {tid = "Examples.List.L", x = VarId {idx = 1}, xType = Obj, cs = [Ctor {info = CtorInfo {name = "Examples.List.L.Nil", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Inc {x = VarId {idx = 2}, n = 1, c = True, persistent = False, b = Ret {r = Var {i = VarId {idx = 2}}}}}, Ctor {info = CtorInfo {name = "Examples.List.L.Cons", cidx = 1, size = 2, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = IsShared {x = VarId {idx = 1}}, b = Case {tid = "Bool", x = VarId {idx = 3}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Bool.false", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 4}, typ = Obj, e = Proj {i = 1, x = VarId {idx = 1}}, b = Vdecl {x = VarId {idx = 5}, typ = Obj, e = Fap {c = "Examples.List.append._rarg", ys = [Var {i = VarId {idx = 4}}, Var {i = VarId {idx = 2}}]}, b = Set {x = VarId {idx = 1}, i = 1, a = Var {i = VarId {idx = 5}}, b = Ret {r = Var {i = VarId {idx = 1}}}}}}}, Ctor {info = CtorInfo {name = "Bool.true", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 6}, typ = Obj, e = Proj {i = 0, x = VarId {idx = 1}}, b = Vdecl {x = VarId {idx = 7}, typ = Obj, e = Proj {i = 1, x = VarId {idx = 1}}, b = Inc {x = VarId {idx = 7}, n = 1, c = True, persistent = False, b = Inc {x = VarId {idx = 6}, n = 1, c = True, persistent = False, b = Dec {x = VarId {idx = 1}, n = 1, c = True, persistent = False, b = Vdecl {x = VarId {idx = 8}, typ = Obj, e = Fap {c = "Examples.List.append._rarg", ys = [Var {i = VarId {idx = 7}}, Var {i = VarId {idx = 2}}]}, b = Vdecl {x = VarId {idx = 9}, typ = Obj, e = ECtor {info = CtorInfo {name = "Examples.List.L.Cons", cidx = 1, size = 2, usize = 0, ssize = 0}, ys = [Var {i = VarId {idx = 6}}, Var {i = VarId {idx = 8}}]}, b = Ret {r = Var {i = VarId {idx = 9}}}}}}}}}}}]}}}]}, info = DeclInfo {sorryDep = Nothing}}), ("Examples.List.append._rarg._boxed", FDecl {f = "Examples.List.append._rarg._boxed", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = Obj}, Param {x = VarId {idx = 2}, borrow = False, typ = Obj}], typ = Obj, bod = Vdecl {x = VarId {idx = 3}, typ = Obj, e = Fap {c = "Examples.List.append._rarg", ys = [Var {i = VarId {idx = 1}}, Var {i = VarId {idx = 2}}]}, b = Dec {x = VarId {idx = 2}, n = 1, c = True, persistent = False, b = Ret {r = Var {i = VarId {idx = 3}}}}}, info = DeclInfo {sorryDep = Nothing}}), ("Examples.List.reverse", FDecl {f = "Examples.List.reverse", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = Irrelevant}], typ = Obj, bod = Vdecl {x = VarId {idx = 2}, typ = Obj, e = Pap {c = "Examples.List.reverse._rarg", ys = []}, b = Ret {r = Var {i = VarId {idx = 2}}}}, info = DeclInfo {sorryDep = Nothing}}), ("Examples.List.reverse._rarg", FDecl {f = "Examples.List.reverse._rarg", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = Obj}], typ = Obj, bod = Case {tid = "Examples.List.L", x = VarId {idx = 1}, xType = Obj, cs = [Ctor {info = CtorInfo {name = "Examples.List.L.Nil", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 2}, typ = Obj, e = ECtor {info = CtorInfo {name = "Examples.List.L.Nil", cidx = 0, size = 0, usize = 0, ssize = 0}, ys = []}, b = Ret {r = Var {i = VarId {idx = 2}}}}}, Ctor {info = CtorInfo {name = "Examples.List.L.Cons", cidx = 1, size = 2, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = IsShared {x = VarId {idx = 1}}, b = Case {tid = "Bool", x = VarId {idx = 3}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Bool.false", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 4}, typ = Obj, e = Proj {i = 1, x = VarId {idx = 1}}, b = Vdecl {x = VarId {idx = 5}, typ = Obj, e = Fap {c = "Examples.List.reverse._rarg", ys = [Var {i = VarId {idx = 4}}]}, b = Vdecl {x = VarId {idx = 6}, typ = Obj, e = ECtor {info = CtorInfo {name = "Examples.List.L.Nil", cidx = 0, size = 0, usize = 0, ssize = 0}, ys = []}, b = Set {x = VarId {idx = 1}, i = 1, a = Var {i = VarId {idx = 6}}, b = Vdecl {x = VarId {idx = 7}, typ = Obj, e = Fap {c = "Examples.List.append._rarg", ys = [Var {i = VarId {idx = 5}}, Var {i = VarId {idx = 1}}]}, b = Dec {x = VarId {idx = 1}, n = 1, c = True, persistent = False, b = Ret {r = Var {i = VarId {idx = 7}}}}}}}}}}, Ctor {info = CtorInfo {name = "Bool.true", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 8}, typ = Obj, e = Proj {i = 0, x = VarId {idx = 1}}, b = Vdecl {x = VarId {idx = 9}, typ = Obj, e = Proj {i = 1, x = VarId {idx = 1}}, b = Inc {x = VarId {idx = 9}, n = 1, c = True, persistent = False, b = Inc {x = VarId {idx = 8}, n = 1, c = True, persistent = False, b = Dec {x = VarId {idx = 1}, n = 1, c = True, persistent = False, b = Vdecl {x = VarId {idx = 10}, typ = Obj, e = Fap {c = "Examples.List.reverse._rarg", ys = [Var {i = VarId {idx = 9}}]}, b = Vdecl {x = VarId {idx = 11}, typ = Obj, e = ECtor {info = CtorInfo {name = "Examples.List.L.Nil", cidx = 0, size = 0, usize = 0, ssize = 0}, ys = []}, b = Vdecl {x = VarId {idx = 12}, typ = Obj, e = ECtor {info = CtorInfo {name = "Examples.List.L.Cons", cidx = 1, size = 2, usize = 0, ssize = 0}, ys = [Var {i = VarId {idx = 8}}, Var {i = VarId {idx = 11}}]}, b = Vdecl {x = VarId {idx = 13}, typ = Obj, e = Fap {c = "Examples.List.append._rarg", ys = [Var {i = VarId {idx = 10}}, Var {i = VarId {idx = 12}}]}, b = Dec {x = VarId {idx = 12}, n = 1, c = True, persistent = False, b = Ret {r = Var {i = VarId {idx = 13}}}}}}}}}}}}}}]}}}]}, info = DeclInfo {sorryDep = Nothing}})]

data T0'Examples_List_L where
  C0'Examples_List_L_Nil :: T0'Examples_List_L
  C0'Examples_List_L_Cons ::
    Poly ->
    T0'Examples_List_L ->
    T0'Examples_List_L
  deriving (Eq)
  deriving (Generic)
  deriving (Ord)
  deriving (Show)
  deriving anyclass (NFData)
  deriving (Arbitrary) via (GenericArbitrary T0'Examples_List_L)

v'toValExamples_List_L :: T0'Examples_List_L -> Val
v'toValExamples_List_L =
  \(v'a) ->
    case (v'a) of
      (C0'Examples_List_L_Nil) -> VCtor (Object 1 0) []
      (C0'Examples_List_L_Cons v'b v'c) ->
        VCtor
          (Object 1 1)
          ((:) (v'toValPoly v'b) ((:) (v'toValExamples_List_L v'c) []))

v'fromValExamples_List_L :: Val -> T0'Examples_List_L
v'fromValExamples_List_L =
  \(v'a :: Val) ->
    case (v'a) of
      (VCtor (Object _ 0) ([])) -> C0'Examples_List_L_Nil
      (VCtor (Object _ 1) ((:) v'b ((:) v'c ([])))) ->
        C0'Examples_List_L_Cons
          (v'fromValPoly v'b)
          (v'fromValExamples_List_L v'c)

v'Examples_List_append = FDecl {f = "Examples.List.append", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = Irrelevant}], typ = Obj, bod = Vdecl {x = VarId {idx = 2}, typ = Obj, e = Pap {c = "Examples.List.append._rarg._boxed", ys = []}, b = Ret {r = Var {i = VarId {idx = 2}}}}, info = DeclInfo {sorryDep = Nothing}}

v'Examples_List_reverse = FDecl {f = "Examples.List.reverse", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = Irrelevant}], typ = Obj, bod = Vdecl {x = VarId {idx = 2}, typ = Obj, e = Pap {c = "Examples.List.reverse._rarg", ys = []}, b = Ret {r = Var {i = VarId {idx = 2}}}}, info = DeclInfo {sorryDep = Nothing}}

sigs :: [Sig]
sigs =
  (:)
    (monoType ((Proxy :: Proxy T0'Examples_List_L)))
    ( (:)
        ( con
            "Examples.List.append"
            ( \(v'a) ->
                \(v'b) ->
                  v'fromValExamples_List_L
                    ( eval
                        v'Examples_List_append
                        environment
                        ( (:)
                            (v'toValExamples_List_L v'a)
                            ((:) (v'toValExamples_List_L v'b) [])
                        )
                    )
            )
        )
        ( (:)
            ( con
                "Examples.List.reverse"
                ( \(v'a) ->
                    v'fromValExamples_List_L
                      ( eval
                          v'Examples_List_reverse
                          environment
                          ((:) (v'toValExamples_List_L v'a) [])
                      )
                )
            )
            []
        )
    )
