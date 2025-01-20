{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TempBool where

import Data.Map.Lazy (Map, fromList)
import GHC.Generics (Generic)
import Lean.Eval
import Lean.IR
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Poly (OrdA (..), OrdB (..), OrdC (..))

environment :: Map Name Decl
environment = fromList [("Examples.Bool.and", FDecl {f = "Examples.Bool.and", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = U8}, Param {x = VarId {idx = 2}, borrow = False, typ = U8}], typ = U8, bod = Case {tid = "Examples.Bool.B", x = VarId {idx = 1}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Examples.Bool.B.t", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Ret {r = Var {i = VarId {idx = 2}}}}, Ctor {info = CtorInfo {name = "Examples.Bool.B.f", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = Lit {v = N 1}, b = Ret {r = Var {i = VarId {idx = 3}}}}}]}, info = DeclInfo {sorryDep = Nothing}}), ("Examples.Bool.not", FDecl {f = "Examples.Bool.not", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = U8}], typ = U8, bod = Case {tid = "Examples.Bool.B", x = VarId {idx = 1}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Examples.Bool.B.t", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 2}, typ = U8, e = Lit {v = N 1}, b = Ret {r = Var {i = VarId {idx = 2}}}}}, Ctor {info = CtorInfo {name = "Examples.Bool.B.f", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = Lit {v = N 0}, b = Ret {r = Var {i = VarId {idx = 3}}}}}]}, info = DeclInfo {sorryDep = Nothing}}), ("Examples.Bool.or", FDecl {f = "Examples.Bool.or", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = U8}, Param {x = VarId {idx = 2}, borrow = False, typ = U8}], typ = U8, bod = Case {tid = "Examples.Bool.B", x = VarId {idx = 1}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Examples.Bool.B.t", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = Lit {v = N 0}, b = Ret {r = Var {i = VarId {idx = 3}}}}}, Ctor {info = CtorInfo {name = "Examples.Bool.B.f", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Ret {r = Var {i = VarId {idx = 2}}}}]}, info = DeclInfo {sorryDep = Nothing}})]

data T'Examples_Bool_B where
  C'Examples_Bool_B_t :: T'Examples_Bool_B
  C'Examples_Bool_B_f :: T'Examples_Bool_B
  deriving (Eq)
  deriving (Generic)
  deriving (Ord)
  deriving (Show)
  deriving (Arbitrary) via (GenericArbitrary T'Examples_Bool_B)

v'toValExamples_Bool_B :: T'Examples_Bool_B -> Val
v'toValExamples_Bool_B =
  \(v'a) ->
    case (v'a) of
      (C'Examples_Bool_B_t) -> Unsigned 0
      (C'Examples_Bool_B_f) -> Unsigned 1

v'fromValExamples_Bool_B :: Val -> T'Examples_Bool_B
v'fromValExamples_Bool_B =
  \(v'a :: Val) ->
    case (v'a) of
      (Unsigned 0) -> C'Examples_Bool_B_t
      (Unsigned 1) -> C'Examples_Bool_B_f

v'Examples_Bool_not = FDecl {f = "Examples.Bool.not", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = U8}], typ = U8, bod = Case {tid = "Examples.Bool.B", x = VarId {idx = 1}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Examples.Bool.B.t", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 2}, typ = U8, e = Lit {v = N 1}, b = Ret {r = Var {i = VarId {idx = 2}}}}}, Ctor {info = CtorInfo {name = "Examples.Bool.B.f", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = Lit {v = N 0}, b = Ret {r = Var {i = VarId {idx = 3}}}}}]}, info = DeclInfo {sorryDep = Nothing}}

v'Examples_Bool_and = FDecl {f = "Examples.Bool.and", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = U8}, Param {x = VarId {idx = 2}, borrow = False, typ = U8}], typ = U8, bod = Case {tid = "Examples.Bool.B", x = VarId {idx = 1}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Examples.Bool.B.t", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Ret {r = Var {i = VarId {idx = 2}}}}, Ctor {info = CtorInfo {name = "Examples.Bool.B.f", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = Lit {v = N 1}, b = Ret {r = Var {i = VarId {idx = 3}}}}}]}, info = DeclInfo {sorryDep = Nothing}}

v'Examples_Bool_or = FDecl {f = "Examples.Bool.or", xs = [Param {x = VarId {idx = 1}, borrow = False, typ = U8}, Param {x = VarId {idx = 2}, borrow = False, typ = U8}], typ = U8, bod = Case {tid = "Examples.Bool.B", x = VarId {idx = 1}, xType = U8, cs = [Ctor {info = CtorInfo {name = "Examples.Bool.B.t", cidx = 0, size = 0, usize = 0, ssize = 0}, b = Vdecl {x = VarId {idx = 3}, typ = U8, e = Lit {v = N 0}, b = Ret {r = Var {i = VarId {idx = 3}}}}}, Ctor {info = CtorInfo {name = "Examples.Bool.B.f", cidx = 1, size = 0, usize = 0, ssize = 0}, b = Ret {r = Var {i = VarId {idx = 2}}}}]}, info = DeclInfo {sorryDep = Nothing}}

sigs :: [Sig]
sigs =
  (:)
    (monoType ((Proxy :: Proxy T'Examples_Bool_B)))
    ( (:)
        ( con
            "Examples.Bool.not"
            ( \(v'a) ->
                v'fromValExamples_Bool_B
                  ( eval
                      v'Examples_Bool_not
                      environment
                      ((:) (v'toValExamples_Bool_B v'a) [])
                  )
            )
        )
        ( (:)
            ( con
                "Examples.Bool.and"
                ( \(v'a) ->
                    \(v'b) ->
                      v'fromValExamples_Bool_B
                        ( eval
                            v'Examples_Bool_and
                            environment
                            ( (:)
                                (v'toValExamples_Bool_B v'a)
                                ((:) (v'toValExamples_Bool_B v'b) [])
                            )
                        )
                )
            )
            ( (:)
                ( con
                    "Examples.Bool.or"
                    ( \(v'a) ->
                        \(v'b) ->
                          v'fromValExamples_Bool_B
                            ( eval
                                v'Examples_Bool_or
                                environment
                                ( (:)
                                    (v'toValExamples_Bool_B v'a)
                                    ((:) (v'toValExamples_Bool_B v'b) [])
                                )
                            )
                    )
                )
                []
            )
        )
    )
