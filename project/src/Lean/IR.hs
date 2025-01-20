{-# OPTIONS_GHC -fdefer-typed-holes #-} -- TODO: DURING DEV ONLY
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lean.IR where

import Control.Monad (forM)
import Data.Aeson hiding (Object)
import Data.Aeson qualified as A
import Data.Aeson.Types hiding (Object)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (toList)
import GHC.Natural (Natural)
import Lean.Util
import Test.QuickCheck (Arbitrary(..), genericShrink)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Instances.Natural ()
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type Name = Text

data Decl
  = FDecl  {f :: FunId, xs :: [Param], typ :: Typ, bod :: FnBody, info :: DeclInfo}
  | Extern {f :: FunId, xs :: [Param], typ :: Typ, ext :: ExternAttrData }
  deriving (Show, Eq)

data ExternAttrData = ExternAttrData { arity :: Maybe Int, entries :: [ExternEntry] }
  deriving (Show, Eq)

data ExternEntry
  = Adhoc { backend :: Name }
  | Inline { backend :: Name, pattern :: String }
  | Standard { backend :: Name, fn :: Text }
  | Foreign { backend :: Name, fn :: Text }
  deriving (Show, Eq)

data Param = Param {x :: VarId, borrow :: Bool, typ :: Typ}
  deriving (Show, Eq)

type FunId = Name

data Typ
  = Fl
  | U8
  | U16
  | U32
  | U64
  | US
  | Irrelevant
  | Obj
  | TObj
  | Fl32
  | Struct { leanTypeName  :: Maybe Name, types :: [Typ] }
  | Union  { leanTypeName' :: Name,       types :: [Typ] }
  deriving (Show, Eq)

newtype DeclInfo = DeclInfo { sorryDep :: Maybe Name}
  deriving (Show, Eq)

newtype VarId =  VarId { idx :: Int }
  deriving (Show,Eq,Ord)

data FnBody
  = Vdecl  { x   :: VarId,       typ  :: Typ,     e      :: Expr,     b          :: FnBody }
  | Jdecl  { j   :: JoinPointId, xs   :: [Param], v      :: FnBody,   b          :: FnBody }
  | Set    { x   :: VarId,       i    :: Int,     a      :: Argument, b          :: FnBody }
  | SetTag { x   :: VarId,       cidx :: Natural, b      :: FnBody }
  | Uset   { x   :: VarId,       i    :: Int,     y      :: VarId,    b          :: FnBody }
  | Sset   { x   :: VarId,       i    :: Int,     offset :: Int,      y          :: VarId, ty :: Typ,   b :: FnBody }
  | Inc    { x   :: VarId,       n    :: Int,     c      :: Bool,     persistent :: Bool,  b  :: FnBody }
  | Dec    { x   :: VarId,       n    :: Int,     c      :: Bool,     persistent :: Bool,  b  :: FnBody }
  | Del    { x   :: VarId,       b    :: FnBody }
  | Mdata  { d   :: MData,       b    :: FnBody }
  | Case   { tid :: Name,        x    :: VarId,   xType  :: Typ,      cs         :: [AltCore FnBody] }
  | Ret    { r   :: Argument }
  | Jmp    { j   :: JoinPointId, ys   :: [Argument] }
  | Unreachable
  deriving (Show, Eq)

newtype JoinPointId = JoinPointId { idx :: Int }
  deriving (Show, Eq, Ord)

data Argument = Var {i :: VarId} | Irr
  deriving (Show, Eq)

data AltCore a = Ctor {info :: CtorInfo, b :: a} | Default {b :: a}
  deriving (Show, Eq)

data CtorInfo = CtorInfo { name :: Name, cidx :: Natural, size :: Int, usize :: Int, ssize :: Int }
  deriving (Show, Eq)

data Expr
  = ECtor    { info :: CtorInfo, ys     :: [Argument] }
  | Reset    { n    :: Int,      x      :: VarId }
  | Reuse    { x    :: VarId,    info   :: CtorInfo, updtHeader :: Bool, ys :: [Argument] }
  | Proj     { i    :: Int,      x      :: VarId }
  | Uproj    { i    :: Int,      x      :: VarId }
  | Sproj    { n    :: Int,      offset :: Int,  x :: VarId }
  | Fap      { c    :: FunId,    ys     :: [Argument] }
  | Pap      { c    :: FunId,    ys     :: [Argument] }
  | Ap       { x    :: VarId,    ys     :: [Argument] }
  | Box      { ty   :: Typ,      x      :: VarId }
  | Unbox    { x    :: VarId }
  | Lit      { v    :: Literal }
  | IsShared { x    :: VarId }
  deriving (Show, Eq)

data Literal = S Text | N Natural
  deriving (Show, Eq)

newtype MData = MData [(Text, DataVal)] -- TODO: How do we manage values?
  deriving (Show, Eq)

data DataVal = DString Text | DBool Bool | DName Text | DNat Int | DInt Int | DSyntax
  deriving (Show, Eq)

data Object = Object { rc :: Int, tag :: Natural }
  deriving (Show)

instance Eq Object where
  (==) = \cases
    Object {tag} Object {tag = tag2} -> tag == tag2

data Val
  = Float Float
  | Unsigned Natural
  | Str Text
  | Irre
  | Obje { o :: Object }
  | VCtor { o :: Object, vs :: [Val] }
  | VBox { v :: Val }
  | Partial { b :: FnBody, ps :: [Param], vs :: [Val], ctx :: Map VarId Val }
  -- TODO: structs and unions
  deriving (Show, Eq)


newtype Poly = Poly Natural
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

instance Arbitrary Poly where
  arbitrary = genericArbitrary
  shrink = genericShrink

v'fromValPoly :: Val -> Poly
v'fromValPoly = \case
  Unsigned a -> Poly a
  _ -> undefined

v'toValPoly :: Poly -> Val
v'toValPoly (Poly a) = Unsigned a

-------------------------------------------------------------------------------
--- Deserialization -----------------------------------------------------------
-------------------------------------------------------------------------------

instance FromJSON Decl where
  parseJSON = keyedObject "Decl"
    [ ("fdecl", cQuin FDecl "f" "xs" "type" "body" "info")
    , ("extern", cQuad Extern "f" "xs" "type" "ext")
    ]

instance FromJSON ExternAttrData where
  parseJSON = \case
    A.Object v -> ExternAttrData <$> v .:? "arity" <*> v .: "entries"
    invalid  ->
      prependFailure "parsing ExternAttrData failed, "
          (typeMismatch "Object" invalid)

instance FromJSON ExternEntry where
  parseJSON = keyedObject "ExternEntry"
    [ ("adhoc",    cSing Adhoc    "backend")
    , ("inline",   cDubl Inline   "backend" "pattern")
    , ("standard", cDubl Standard "backend" "fn")
    , ("foreign",  cDubl Foreign  "backend" "fn")
    ]

instance FromJSON Param where
  parseJSON = \case
    A.Object v -> cTrip Param "x" "borrow" "ty" v
    invalid  ->
      prependFailure "parsing IRParam failed, "
          (typeMismatch "Object" invalid)

instance FromJSON VarId where
  parseJSON = \case
    A.Object v -> cSing VarId "idx" v
    invalid  ->
      prependFailure "parsing VarId failed, "
          (typeMismatch "Object" invalid)

instance FromJSON Typ where
  parseJSON = \case
    String "float"      -> pure Fl
    String "uint8"      -> pure U8
    String "uint16"     -> pure U16
    String "uint32"     -> pure U32
    String "uint64"     -> pure U64
    String "usize"      -> pure US
    String "irrelevant" -> pure Irrelevant
    String "object"     -> pure Obj
    String "tobject"    -> pure TObj
    String "float32"    -> pure Fl32
    A.Object v            -> alt
                             (cDubl Struct "leanTypeName" "types" )
                             (cDubl Union  "leanTypeName" "types")
                             v
    invalid  ->
      prependFailure "parsing IRType failed, "
          (typeMismatch "Object" invalid)

instance FromJSON DeclInfo where
  parseJSON = \case
    String s -> pure . DeclInfo $ Just s
    A.Object _ -> pure $ DeclInfo Nothing
    invalid  ->
      prependFailure "parsing DeclInfo failed, "
          (typeMismatch "Object" invalid)

instance FromJSON FnBody where
  parseJSON = keyedObject "FnBody"
    [ ("vdecl"       , cQuad Vdecl  "x"   "ty"   "e"      "b")
    , ("jdecl"       , cQuad Jdecl  "j"   "xs"   "v"      "b")
    , ("set"         , cQuad Set    "x"   "i"    "y"      "b")
    , ("setTag"      , cTrip SetTag "x"   "cidx" "b")
    , ("uset"        , cQuad Uset   "x"   "i"    "y"      "b")
    , ("sset"        , cSext Sset   "x"   "i"    "offset" "y"          "ty" "b")
    , ("inc"         , cQuin Inc    "x"   "n"    "c"      "persistent" "b")
    , ("dec"         , cQuin Dec    "x"   "n"    "c"      "persistent" "b")
    , ("del"         , cDubl Del    "x"   "b")
    , ("mdata"       , cDubl Mdata "d"   "b")
    , ("case"        , cQuad Case   "tid" "x"    "xType"  "cs")
    , ("ret"         , cSing Ret    "x")
    , ("jmp"         , cDubl Jmp    "j"   "ys")
    , ("unreachable" , const (pure Unreachable))
    ]

instance FromJSON JoinPointId where
  parseJSON = \case
    A.Object v -> cSing JoinPointId "idx" v
    invalid  ->
      prependFailure "parsing VarId failed, "
          (typeMismatch "Object" invalid)

instance FromJSON MData where
  parseJSON = withArray "MData" $ \kva ->
    fmap (MData . toList) $ forM kva $ parseJSON

instance FromJSON DataVal where
  parseJSON = keyedVal "DataVal"
    [ ("string" , mk DString)
    , ("bool"   , mk DBool)
    , ("name"   , mk DName)
    , ("nat"    , mk DNat)
    , ("int"    , mk DInt)
    , ("syntax" , pure . const DSyntax)
    ]

instance FromJSON Argument where
  parseJSON = \case
    String "irrelevant" -> pure Irr
    A.Object v -> v .: "var" >>=
      \case
      A.Object v'         -> cSing Var "id" v'
      invalid  ->
        prependFailure "parsing Var failed, "
            (typeMismatch "Object" invalid)
    invalid  ->
      prependFailure ("parsing Argument failed, ")
          (typeMismatch "Object" invalid)

instance FromJSON a => FromJSON (AltCore a) where
  parseJSON =
    keyedObject "AltCore"
    [ ("ctor"    , cDubl Ctor    "info" "b")
    , ("default" , cSing Default "b")
    ]

instance FromJSON CtorInfo where
  parseJSON = \case
    A.Object v -> cQuin CtorInfo "name" "cidx" "size" "usize" "ssize" v
    invalid  ->
      prependFailure "parsing CtorInfo failed, "
          (typeMismatch "Object" invalid)

instance FromJSON Expr where
  parseJSON = keyedObject "IRExpr"
    [ ("ctor"     , cDubl ECtor    "i"  "ys")
    , ("reset"    , cDubl Reset    "n"  "x")
    , ("reuse"    , cQuad Reuse    "x"  "info"   "updtHeader" "ys")
    , ("proj"     , cDubl Proj     "i"  "x")
    , ("uproj"    , cDubl Uproj    "i"  "x")
    , ("sproj"    , cTrip Sproj    "n"  "offset" "x")
    , ("fap"      , cDubl Fap      "c"  "ys")
    , ("pap"      , cDubl Pap      "c"  "ys")
    , ("ap"       , cDubl Ap       "x"  "ys")
    , ("box"      , cDubl Box      "ty" "x")
    , ("unbox"    , cSing Unbox    "x")
    , ("lit"      , cSing Lit      "v") -- TODO: Verify that this works
    , ("isShared" , cSing IsShared "x")
    ]

instance FromJSON Literal where
  parseJSON =  keyedObject "IRLiteral"
    [ ("num", cSing N "v")
    , ("str", cSing S "v")
    ]
