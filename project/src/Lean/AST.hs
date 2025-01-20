{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lean.AST where

import Control.Monad (forM, join, (>=>))
import Data.Aeson
import Data.Aeson.Types
import Data.List ((!?))
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Vector (toList)
import Lean.IR qualified as IR
import Lean.Util hiding (fail)
import Prettyprinter
  ( Doc,
    Pretty,
    align,
    comma,
    enclose,
    encloseSep,
    group,
    hcat,
    line,
    line',
    nest,
    pretty,
    vsep,
    (<+>),
  )

-------------------------------------------------------------------------------
--- ADT definitions -----------------------------------------------------------
-------------------------------------------------------------------------------
-- Doc comments are sourced from the lean4 sources

type Name = Text

data Out = DeclT Expr [Name] IR.Decl | Decl IR.Decl | ConInfo Def
  deriving Show


data Def = Def { name :: Name, levelParams :: [Name], typ :: Expr, kind :: DefKind }
  deriving Show

data DefKind
  = Axiom {isUnsafe :: Bool}
  | Definitional {val :: Expr, safety :: DefSafety}
  | Theorem {val :: Expr}
  | Opaque {val :: Expr, isUnsafe :: Bool}
  | Quot {kind :: QuotKind}
  | -- | The kernel compiles (mutual) inductive declarations (see `inductiveDecls`) into a set of
    -- - `Declaration.inductDecl` (for each inductive datatype in the mutual Declaration),
    -- - `Declaration.ctorDecl` (for each Constructor in the mutual Declaration),
    -- - `Declaration.recDecl` (automatically generated recursors).
    --
    -- This data is used to implement iota-reduction efficiently and compile nested inductive
    -- declarations.
    --
    -- A series of checks are performed by the kernel to check whether a `inductiveDecls`
    -- is valid or not.
    Inductive
      { -- | Number of parameters. A parameter is an argument to the defined type that is fixed over constructors.
        -- An example of this is the `α : Type` argument in the vector constructors
        -- `nil : Vector α 0` and `cons : α → Vector α n → Vector α (n+1)`.
        --
        -- The intuition is that the inductive type must exhibit _parametric polymorphism_ over the inductive
        -- parameter, as opposed to _ad-hoc polymorphism_.
        numParams :: Int,
        -- | Number of indices. An index is an argument that varies over constructors.
        --
        -- An example of this is the `n : Nat` argument in the vector constructor `cons : α → Vector α n → Vector α (n+1)`.
        numIndices :: Int,
        -- | List of the names of the constructors for this inductive datatype.
        ctors :: [Name],
        -- | Number of auxiliary data types produced from nested occurrences.
        -- An inductive definition `T` is nested when there is a constructor with an argument `x : F T`,
        --  where `F : Type → Type` is some suitably behaved (ie strictly positive) function (Eg `Array T`, `List T`, `T × T`, ...).
        numNested :: Int,
        -- | `true` when recursive (that is, the inductive type appears as an argument in a constructor).
        isRec :: Bool,
        -- | Whether the definition is flagged as unsafe.
        isUnsafe :: Bool,
        -- | An inductive type is called reflexive if it has at least one constructor that takes as an argument a function returning the
        -- same type we are defining.
        -- Consider the type:
        -- ```
        -- inductive WideTree where
        -- | branch: (Nat -> WideTree) -> WideTree
        -- | leaf: WideTree
        -- ```
        -- this is reflexive due to the presence of the `branch : (Nat -> WideTree) -> WideTree` constructor.
        --
        -- See also: 'Inductive Definitions in the system Coq Rules and Properties' by Christine Paulin-Mohring
        -- Section 2.2, Definition 3
        isReflexive :: Bool
      }
  | Constructor
      { -- | Inductive type this constructor is a member of
        induct :: Name,
        -- | Constructor index (i.e., Position in the inductive declaration)
        cidx :: Int,
        -- | Number of parameters in inductive datatype.
        numParams :: Int,
        -- | Number of fields (i.e., arity - nparams)
        arity :: Int,
        isUnsafe :: Bool
      }
  | Recursor
      { -- | Number of parameters
        numParams :: Int,
        -- | Number of indices
        numIndices :: Int,
        -- | Number of motives
        numMotives :: Int,
        -- | Number of minor premises
        numMinors :: Int,
        -- | A reduction for each Constructor
        rules :: [RecursorRule],
        -- | It supports K-like reduction.
        -- A recursor is said to support K-like reduction if one can assume it behaves
        -- like `Eq` under axiom `K` --- that is, it has one constructor, the constructor has 0 arguments,
        -- and it is an inductive predicate (ie, it lives in Prop).
        --
        -- Examples of inductives with K-like reduction is `Eq`, `Acc`, and `And.intro`.
        -- Non-examples are `exists` (where the constructor has arguments) and
        --   `Or.intro` (which has multiple constructors).
        k :: Bool,
        isUnsafe :: Bool
      }
  deriving (Show)

data DefSafety = Unsafe | Safe | Partial
  deriving (Show)

data QuotKind
  = -- | `Quot`
    QT
  | -- | `Quot.mk`
    QC
  | -- | `Quot.lift`
    QL
  | -- | `Quot.ind`
    QI
  deriving (Show)

-- | Information for reducing a recursor
data RecursorRule = RecRule
  { -- | Reduction rule for this Constructor
    ctor :: Name,
    -- | Number of fields (i.e., without counting inductive datatype parameters)
    nfields :: Int,
    -- | Right hand side of the reduction rule
    def :: Expr
  }
  deriving (Show)

-- | Lean expressions. This data structure is used in the kernel and
-- elaborator. However, expressions sent to the kernel should not
-- contain metavariables.
--
-- Remark: we use the `E` suffix (short for `Expr`) to avoid collision with keywords.
-- We considered using «...», but it is too inconvenient to use.
data Expr
  = -- | The `bvar` constructor represents bound variables, i.e. occurrences
    -- of a variable in the expression where there is a variable binder
    -- above it (i.e. introduced by a `lam`, `forallE`, or `letE`).
    --
    -- The `deBruijnIndex` parameter is the *de-Bruijn* index for the bound
    -- variable. See [the Wikipedia page on de-Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index)
    -- for additional information.
    --
    -- For example, consider the expression `fun x : Nat => forall y : Nat, x = y`.
    -- The `x` and `y` variables in the equality expression are constructed
    -- using `bvar` and bound to the binders introduced by the earlier
    -- `lam` and `forallE` constructors. Here is the corresponding `Expr` representation
    -- for the same expression:
    -- ```lean
    -- .lam `x (.const `Nat [])
    --   (.forallE `y (.const `Nat [])
    --     (.app (.app (.app (.const `Eq [.succ .zero]) (.const `Nat [])) (.bvar 1)) (.bvar 0))
    --     .default)
    --   .default
    -- ```
    --
    Bvar {index :: Int}
  | -- | The `fvar` constructor represent free variables. These *free* variable
    -- occurrences are not bound by an earlier `lam`, `forallE`, or `letE`
    -- constructor and its binder exists in a local context only.
    --
    -- Note that Lean uses the *locally nameless approach*. See [McBride and McKinna](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.365.2479&rep=rep1&type=pdf)
    -- for additional details.
    --
    -- When "visiting" the body of a binding expression (i.e. `lam`, `forallE`, or `letE`),
    -- bound variables are converted into free variables using a unique identifier,
    -- and their user-facing name, type, value (for `LetE`), and binder annotation
    -- are stored in the `LocalContext`.
    Fvar {ident :: Text}
  | -- | Metavariables are used to represent "holes" in expressions, and goals in the
    -- tactic framework. Metavariable declarations are stored in the `MetavarContext`.
    -- Metavariables are used during elaboration, and are not allowed in the kernel,
    -- or in the code generator.
    Mvar {ident :: Text}
  | -- | Used for `Type u`, `Sort u`, and `Prop`:
    -- - `Prop` is represented as `.sort .zero`,
    -- - `Sort u` as ``.sort (.param `u)``, and
    -- - `Type u` as ``.sort (.succ (.param `u))``
    Sort {level :: Level}
  | -- | A (universe polymorphic) constant that has been defined earlier in the module or
    -- by another imported module. For example, `@Eq.{1}` is represented
    -- as ``Expr.const `Eq [.succ .zero]``, and `@Array.map.{0, 0}` is represented
    -- as ``Expr.const `Array.map [.zero, .zero]``.
    Const {name :: Text, levels :: [Level]}
  | -- | A function application.
    --
    -- For example, the natural number one, i.e. `Nat.succ Nat.zero` is represented as
    -- ``Expr.app (.const `Nat.succ []) (.const .zero [])``.
    -- Note that multiple arguments are represented using partial application.
    --
    -- For example, the two argument application `f x y` is represented as
    -- `Expr.app (.app f x) y`.
    App {function :: Expr, arg :: Expr}
  | -- | A lambda abstraction (aka anonymous functions). It introduces a new binder for
    -- variable `x` in scope for the lambda body.
    --
    -- For example, the expression `fun x : Nat => x` is represented as
    -- ```
    -- Expr.lam `x (.const `Nat []) (.bvar 0) .default
    -- ```
    Lam {binder :: Name, typ :: Expr, body :: Expr, info :: BinderInfo}
  | -- | A dependent arrow `(a : α) → β)` (aka forall-expression) where `β` may dependent
    -- on `a`. Note that this constructor is also used to represent non-dependent arrows
    -- where `β` does not depend on `a`.
    --
    -- For example:
    -- - `forall x : Prop, x ∧ x`:
    --   ```lean
    --   Expr.forallE `x (.sort .zero)
    --     (.app (.app (.const `And []) (.bvar 0)) (.bvar 0)) .default
    --   ```
    -- - `Nat → Bool`:
    --   ```lean
    --   Expr.forallE `a (.const `Nat [])
    --     (.const `Bool []) .default
    --   ```
    ForAllE {binder :: Name, typ :: Expr, body :: Expr, info :: BinderInfo}
  | -- | Let-expressions.
    --
    -- **IMPORTANT**: The `nonDep` flag is for "local" use only. That is, a module should not "trust" its value for any purpose.
    -- In the intended use-case, the compiler will set this flag, and be responsible for maintaining it.
    -- Other modules may not preserve its value while applying transformations.
    --
    -- Given an environment, a metavariable context, and a local context,
    -- we say a let-expression `let x : t := v; e` is non-dependent when it is equivalent
    -- to `(fun x : t => e) v`. In contrast, the dependent let-expression
    -- `let n : Nat := 2; fun (a : Array Nat n) (b : Array Nat 2) => a = b` is type correct,
    -- but `(fun (n : Nat) (a : Array Nat n) (b : Array Nat 2) => a = b) 2` is not.
    --
    -- The let-expression `let x : Nat := 2; Nat.succ x` is represented as
    -- ```
    -- Expr.letE `x (.const `Nat []) (.lit (.natVal 2)) (.app (.const `Nat.succ []) (.bvar 0)) true
    -- ```
    -- --------------
    -- It seems nonDep signifies that a the variable has been zetaDelta-expanded
    -- Basically representing `let x := v; e` as `(fun x => e) v`
    -- But I have no clue what it would mean in the elaborated context
    LetE {name :: Name, typ :: Expr, val :: Expr, body :: Expr, nonDep :: Bool}
  | -- | Natural number and string literal values.
    --
    -- They are not really needed, but provide a more compact representation in memory
    -- for these two kinds of literals, and are used to implement efficient reduction
    -- in the elaborator and kernel. The "raw" natural number `2` can be represented
    -- as `Expr.lit (.natVal 2)`. Note that, it is definitionally equal to:
    -- ```lean
    -- Expr.app (.const `Nat.succ []) (.app (.const `Nat.succ []) (.const `Nat.zero []))
    -- ```
    Lit {litVal :: Literal}
  | -- | Metadata (aka annotations).
    --
    -- We use annotations to provide hints to the pretty-printer,
    -- store references to `Syntax` nodes, position information, and save information for
    -- elaboration procedures (e.g., we use the `inaccessible` annotation during elaboration to
    -- mark `Expr`s that correspond to inaccessible patterns).
    --
    -- Note that `Expr.mdata data e` is definitionally equal to `e`.
    Mdata {dat :: MData, body :: Expr}
  | -- | Projection-expressions. They are redundant, but are used to create more compact
    -- terms, speedup reduction, and implement eta for structures.
    -- The type of `struct` must be an structure-like inductive type. That is, it has only one
    -- constructor, is not recursive, and it is not an inductive predicate. The kernel and elaborators
    -- check whether the `typeName` matches the type of `struct`, and whether the (zero-based) index
    -- is valid (i.e., it is smaller than the number of constructor fields).
    -- When exporting Lean developments to other systems, `proj` can be replaced with `typeName`.`rec`
    -- applications.
    --
    -- Example, given `a : Nat × Bool`, `a.1` is represented as
    -- ```
    -- .proj `Prod 0 a
    -- ```
    Proj {name :: Name, idx :: Int, struct :: Expr}
  deriving (Show)

-- | If two Expressions are syntactically equal
instance Eq Expr where
  (==) = \cases
    (Bvar a)          (Bvar b)             -> a == b
    (Fvar a)          (Fvar b)             -> a == b
    (Mvar a)          (Mvar b)             -> a == b
    (Sort a)          (Sort b)             -> a == b
    (Const n l)       (Const n' l')        -> n' == n && l == l'
    (App f a)         (App f' a')          -> f' == f && a == a'
    -- Names should be irrelevant as actual references use Bvars
    (Lam _ t b i)     (Lam _ t' b' i')     -> t' == t && b == b' && i == i'
    (ForAllE _ t b i) (ForAllE _ t' b' i') -> t' == t && b == b' && i == i'
    (LetE _ t v b d)  (LetE _ t' v' b' d') -> t' == t && v == v' && b == b' && d == d'
    (Lit a)           (Lit b)              -> a == b
    (Mdata d e)       (Mdata d' e')        -> d == d' && e == e'
    (Proj n i s)      (Proj n' i' s')      -> n == n' && i == i' && s == s'
    _                  _                   -> False


-- | Arguments in forallE binders can be labelled as implicit or explicit.
--
-- Each `lam` or `forallE` binder comes with a `binderInfo` argument (stored in ExprData).
-- This can be set to
-- - `default` -- `(x : α)`
-- - `implicit` --  `{x : α}`
-- - `strict_implicit` -- `⦃x : α⦄`
-- - `inst_implicit` -- `[x : α]`.
-- - `aux_decl` -- Auxiliary definitions are helper methods that
--   Lean generates. `aux_decl` is used for `_match`, `_fun_match`,
--   `_let_match` and the self reference that appears in recursive pattern matching.
--
-- The difference between implicit `{}` and strict-implicit `⦃⦄` is how
-- implicit arguments are treated that are *not* followed by explicit arguments.
-- `{}` arguments are applied eagerly, while `⦃⦄` arguments are left partially applied:
-- ```
-- def foo {x : Nat} : Nat := x
-- def bar ⦃x : Nat⦄ : Nat := x
-- #check foo -- foo : Nat
-- #check bar -- bar : ⦃x : Nat⦄ → Nat
-- ```
--
-- See also [the Lean manual](https://lean-lang.org/lean4/doc/expressions.html#implicit-arguments).
data BinderInfo
  = -- | Default binder annotation, e.g. `(x : α)`
    BDefault
  | -- | Implicit binder annotation, e.g., `{x : α}`
    BImplicit
  | -- | Strict implicit binder annotation, e.g., `{{ x : α }}`
    BStrictImplicit
  | -- | Local instance binder annotataion, e.g., `[Decidable α]`
    BInstance
  deriving (Show, Eq)

data Literal = S Text | N Integer
  deriving (Show, Eq)

data Level
  = Zero
  | Succ Level
  | Max Level Level
  | IMax Level Level
  | Param Text
  | LMVarId Text
  deriving (Show, Eq)

newtype MData = MData [(Text, DataVal)] -- TODO: How do we manage values?
  deriving (Show, Eq)

data DataVal = DString Text | DBool Bool | DName Text | DNat Int | DInt Int | DSyntax
  deriving (Show, Eq)

-------------------------------------------------------------------------------
--- Pretty printing -----------------------------------------------------------
-------------------------------------------------------------------------------

block :: Doc a -> Doc a
block = group . nest 2

lvls :: Pretty l => [l] -> Doc a
lvls [] = mempty
lvls ls = encloseSep ".{" "}" comma (map pretty ls)

decl :: Name -> [Name] -> Expr -> Doc a
decl n lp typ =
  group
    ( pretty n <> lvls lp </>
      ":" <+> block (prettyExpr (Just []) Nothing typ)
    )

instance Pretty Def where
  pretty Def { name, levelParams, typ, kind } = case kind of
    Definitional { val, safety } ->
      (case safety of
        Safe -> mempty
        Unsafe -> "unsafe "
        Partial -> "partial ") <>
      decl name levelParams typ </>
      "=" <+> block (prettyExpr (Just []) (Just typ) val)
    Theorem { val } ->
      "theorem" <+> decl name levelParams typ </>
      "=" <+> block (prettyExpr (Just []) (Just typ) val)
    Recursor { rules } ->
      decl name levelParams typ </>
      vsep (flip map rules (\RecRule { ctor, nfields, def } ->
        pretty ctor <> hcat (replicate nfields " _") </>
        "→" <+> block (prettyExpr (Just []) (Just typ) def)))
    Inductive { ctors } ->
      decl name levelParams typ </>
      vsep (map (("|" <+>) . pretty) ctors)
    Constructor { arity } ->
      pretty name <> lvls levelParams <+> "□" <> "(" <> pretty arity <> ")" </>
      ":" <+> block (prettyExpr (Just []) Nothing typ)
    Axiom { isUnsafe } ->
      if isUnsafe then "unsafe " else "" <> "⊢" <+> decl name levelParams typ
    _ -> error "Undefined Pretty Def"

bindParen :: BinderInfo -> Doc a -> Doc a
bindParen i d = align $ wrap $ align d <> line'
  where
    wrap = case i of
      BDefault -> enclose "(" ")"
      BImplicit -> enclose "{" "}"
      BStrictImplicit -> enclose "⦃" "⦄"
      BInstance -> enclose "[" "]"

toSup :: Char -> Char
toSup = \case
  '0' -> '⁰'
  '1' -> '¹'
  '2' -> '²'
  '3' -> '³'
  '4' -> '⁴'
  '5' -> '⁵'
  '6' -> '⁶'
  '7' -> '⁷'
  '8' -> '⁸'
  '9' -> '⁹'
  a -> a

instance Pretty Expr where pretty = prettyExpr Nothing Nothing

prettyExpr' :: Text -> Maybe [Text] -> Maybe Expr -> Expr -> Doc a
prettyExpr' name bStack ctx = prettyExpr ((name :) <$> bStack) ctx

-- | Pretty print an expr, optionally with a type signature as context
prettyExpr :: Maybe [Text] -> Maybe Expr -> Expr -> Doc a
prettyExpr bStack ctx = \case
    Bvar { index } -> case join $ fmap (!? index) bStack of
      Just nam -> pretty (map toSup (show index)) <> pretty nam
      Nothing -> pretty index <> "ᵝ"
    Fvar { {- ident -} } -> error "Fvar"
    Mvar { {- ident -} } -> error "Mvar"
    Sort { level } -> "Sort" <+> pretty level
    Const { name, levels } -> "`" <> pretty name <> lvls levels
    d@App {} ->
      let (fn, args) = go (function d) [arg d]
          go f@App {} as = go (function f) (arg f : as)
          go f@Lam {}     as = ("(" <> prettyExpr bStack Nothing f <> ")", as)
          go f@ForAllE {} as = ("(" <> prettyExpr bStack Nothing f <> ")", as)
          go f              as = (prettyExpr bStack ctx f, as)

          pArg arg = group ("∘(" <> align (prettyExpr bStack Nothing arg) <> ")")
      in fn </> vsep (map pArg args)

    Lam { binder, typ, body, info } ->
      let (sameTyp, ctx') = case ctx of
            Just (ForAllE { typ=typ', body=body' }) -> (typ == typ', Just body')
            _ -> (False, Nothing)
          showTyp t = if sameTyp then mempty else t
      in group $ group (
          "λ" <> bindParen info (pretty binder <> showTyp (line <>
          ": " <> block (prettyExpr bStack Nothing typ)))
        ) </>
        "↦ " <> block (prettyExpr' binder bStack ctx' body)
    ForAllE { binder, typ, body, info} ->
      group (
        "∀" <> bindParen info (pretty binder </>
        ": " <> block (prettyExpr bStack Nothing typ))
      ) </>
      "→ " <> block (prettyExpr' binder bStack Nothing body)
    LetE { name, typ, val, body } ->
      block ("let" <+> pretty name </> ":" <+> pretty typ) </>
      "=" <+> block (prettyExpr bStack ctx val) </>
      "in" <+> block (prettyExpr' name bStack ctx body)
    Lit { litVal } -> pretty litVal
    Mdata { dat=MData dat, body } ->
      let kv (k, v) = pretty k <> ":" <+> pretty v
      in enclose "(" ")" $ group (vsep (map kv dat)) </>
         "⊢" <+> block (pretty body)
    Proj { name, idx, struct } ->
      pretty name <> "." <> pretty idx <> "(" <>
        block (line <> prettyExpr bStack Nothing struct) </>
      ")"

instance Pretty Level where
  pretty = \case
    Zero -> "Ø"
    Succ l0 ->
      let go i (Succ l) = go (i+1) l
          go i l        = (i, l)
      in case go (1 :: Int) l0 of
        (i, Zero) -> pretty i
        (i, l) -> "(" <> pretty l <+> "+" <+> pretty i <> ")"
    Max a b -> "max(" <> pretty a <> "," <+> pretty b <> ")"
    IMax a b -> "imax(" <> pretty a <> "," <+> pretty b <> ")"
    Param n -> pretty n
    LMVarId _ -> error "LMVarId"

instance Pretty Literal where
  pretty (S t) = enclose "“" "”" $ pretty t
  pretty (N n) = pretty n

instance Pretty DataVal where
  pretty = \case
    DString s -> enclose "“" "”" $ pretty s
    DBool b -> pretty b
    DName n -> "`" <> pretty n
    DNat n -> pretty n
    DInt i -> "i" <> pretty i
    DSyntax -> "⟪SYNTAX⟫"

-------------------------------------------------------------------------------
--- Deserialization -----------------------------------------------------------
-------------------------------------------------------------------------------

instance FromJSON Out where
  parseJSON :: Value -> Parser Out
  parseJSON = keyedVal "Out"
    [ ("dec"    , withObject "dec" $ cSing Decl "def")
    , ("decT"   , withObject "decT" $ cTrip DeclT "typ" "level" "def" )
    , ("conInfo", mk ConInfo)
    ]

instance FromJSON Def where
  parseJSON = keyedObject "ConstantInfo"
    [ ("axiomInfo"  , (.: "val") >=> (shared $ cSing Axiom        "isUnsafe"))
    , ("defnInfo"   , (.: "val") >=> (shared $ cDubl Definitional "value"    "safety"))
    , ("thmInfo"    , (.: "val") >=> (shared $ cSing Theorem      "value"))
    , ("opaqueInfo" , (.: "val") >=> (shared $ cDubl Opaque       "value"    "isUnsafe"))
    , ("quotInfo"   , (.: "val") >=> (shared $ cSing Quot         "kind"))
    , ("inductInfo" , (.: "val") >=> (shared $ cSept Inductive    "numParams" "numIndices" "ctors" "numNested" "isRec" "isUnsafe" "isReflexive"))
    , ("ctorInfo"   , (.: "val") >=> (shared $ cQuin Constructor  "induct"   "cidx" "numParams" "numFields" "isUnsafe"))
    , ("recInfo"    , (.: "val") >=> (shared $ cSept Recursor     "numParams" "numIndices" "numMotives" "numMinors" "rules" "k" "isUnsafe"))
    ]
    where
      shared :: (Object -> Parser DefKind) -> Object -> Parser Def
      shared f o = cTrip Def "name" "levelParams" "type" o <*> f o

instance FromJSON DefSafety where
  parseJSON = \case
    String "«unsafe»" -> pure Unsafe
    String "safe" -> pure Safe
    String "«partial»" -> pure Partial
    _ -> fail "Definitional safety is not of valid enum"

instance FromJSON RecursorRule where
  parseJSON = withObject "RecursorRule" $ cTrip RecRule "ctor" "nfields" "rhs"

instance FromJSON Expr where
  parseJSON = withObject "Expr" (cSing Lit "lit") `alt` keyedObject "Expr"
    [ ("bvar"    , cSing Bvar    "deBruijnIndex")
    , ("fvar"    , cSing Fvar    "id")
    , ("mvar"    , cSing Mvar    "id")
    , ("sort"    , cSing Sort    "u")
    , ("const"   , cDubl Const   "declName"   "us")
    , ("app"     , cDubl App     "fn"         "arg")
    , ("lam"     , cQuad Lam     "binderName" "binderType" "body"  "binderInfo")
    , ("forallE" , cQuad ForAllE "binderName" "binderType" "body"  "binderInfo")
    , ("letE"    , cQuin LetE    "declName"   "type" "value" "body" "nonDep")
    , ("mdata"   , cDubl Mdata   "data"       "expr")
    , ("proj"    , cTrip Proj    "typeName"   "idx" "struct")
    ]

instance FromJSON Literal where
  parseJSON (Number n) = case floatingOrInteger n of
    Left (_ :: Double) -> fail "Literal is unexpected floating"
    Right i -> pure $ N i
  parseJSON (String s) = pure $ S s
  parseJSON invalid = prependFailure "Literal is " (unexpected invalid)

instance FromJSON Level where
  parseJSON (String "zero") = pure Zero
  parseJSON v = flip (withObject "Level") v
    $     cSing Succ "succ"
    `alt` cSing (uncurry Max) "max"
    `alt` cSing (uncurry IMax) "imax"
    `alt` cSing Param "param"
    `alt` cSing LMVarId "lmVarId"
    `alt` const (fail "unknown level")

instance FromJSON BinderInfo where
  parseJSON = \case
    String "default" -> pure BDefault
    String "implicit" -> pure BImplicit
    String "strictImplicit" -> pure BStrictImplicit
    String "instImplicit" -> pure BInstance
    _ -> fail "BinderInfo is not of valid enum"

instance FromJSON QuotKind where
  parseJSON = \case
    String "type" -> pure QT
    String "ctor" -> pure QC
    String "lift" -> pure QL
    String "ind"  -> pure QI
    _ -> fail "QuotKind is not valid enum"

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
