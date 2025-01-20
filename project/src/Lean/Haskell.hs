{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language RecordWildCards #-}
{-# Language DuplicateRecordFields #-}
module Lean.Haskell where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor
import Control.Monad.Writer
import Control.Monad.Reader
import Lean.Util (indnt, (</>))
import Prettyprinter
  ( Doc,
    Pretty,
    enclose,
    group,
    hcat,
    line,
    nest,
    parens,
    pretty,
    vsep,
    (<+>),
    tupled,
    defaultLayoutOptions,
    layoutPretty, list,
  )
import Prettyprinter.Render.Text (renderLazy)

-------------------------------------------------------------------------------
--- ADT definitions -----------------------------------------------------------
-------------------------------------------------------------------------------

data    Vv = Vd [Int] Text -- ^ Declaration in Expression
           | Vv Text       -- ^ Var in Expression
           | Vvh Text      -- ^ Raw in Expression
  deriving (Eq,Show)
data    VC = VC [Int] Text -- ^ Constructor definition
           | VCh Text      -- ^ Raw Constructor
  deriving (Eq,Show)
data    VT = VT { enum :: Bool, levels :: [Int], name :: Text} -- ^ Declaration in Type Level
           | VTh Text      -- ^ Raw in Type Level
  deriving (Eq,Show)
newtype Vt = Vt       Text -- ^ Var in Type level
  deriving (Eq,Show)

type Tagged d = d Vv VC VT Vt
type HCode = Tagged Code
type HExpr = Tagged Expr
type HTypC = TypComponent VT Vt
type HTyp = Typ VT Vt
type HCtor = Ctor VC VT Vt
type HMatch = Match Vv VC VT
type HCaseOf = Tagged CaseOf
type HDeriv = Deriv VT Vt
type HSegment = Tagged Segment

data E a b c = Le a | Mi b | Ri c

-- TODO: replace with Maybe?
data Typ t tv
  = T (TypComponent t tv) -- ^ The type
  | TNone                 -- ^ No type is specified
  deriving (Eq, Show)

data TypComponent t tv
  = TNam t                                       -- ^ Named type
  | TVar tv                                      -- ^ Type variable
  | TApp (TypComponent t tv) (TypComponent t tv) -- ^ Modus ponens of types
  | TArr (TypComponent t tv) (TypComponent t tv) -- ^ Lambda type
  | TAbs tv (TypComponent t tv)                  -- ^ forall a. ...
  deriving (Eq, Show)

data Ctor c t tv = Ctor { name :: c, typ :: TypComponent t tv }

data Match v c t = Match { ctor :: c, args :: [E v (Match v c t) t] }

data CaseOf v c t tv
  = CaseOf { match :: Tuples (Match v c t), body :: Expr v c t tv}

data Tuples a = Tup a [a]

data Expr v c t tv
  = Var (Either v c)
  | Typ {body :: Expr v c t tv, typ :: Typ t tv}
  | App {fn :: Expr v c t tv, arg :: Expr v c t tv}
  | Lst [Expr v c t tv] -- ^ Hard coded special case for lists
  | Lam {binding :: v, typ :: Typ t tv, body :: Expr v c t tv}
  | Let
      { binding :: v,
        typ :: Typ t tv,
        val :: Expr v c t tv,
        body :: Expr v c t tv
      }
  | Cas {args :: Tuples (Expr v c t tv), cas :: [CaseOf v c t tv]}
  | Int Integer
  | Str Text

data Deriv t tv = Deriv {cls :: t, via :: Typ t tv}

data Segment v c t tv
  = Data
      { tName :: t,
        tArgs :: [tv],
        constructors :: [Ctor c t tv],
        deriv :: [Deriv t tv]
      }
  | Alias {tName :: t, tArgs :: [tv], typ :: Typ t tv}
  | Decl {name :: v, typ :: Typ t tv, body :: Expr v c t tv}
  | Comment TL.Text
  | Raw TL.Text

data Code v c t tv = EOF | M (Code v c t tv) (Segment v c t tv)

-------------------------------------------------------------------------------
--- Util functions ------------------------------------------------------------
-------------------------------------------------------------------------------

instance Semigroup (Code v c t tv) where
  c <>  EOF     = c
  c <> (M c' s) = M (c <> c') s

instance Monoid (Code v c t tv) where
  mempty = EOF

typSubsts :: Eq tv => [(tv, TypComponent t tv)] -> TypComponent t tv -> TypComponent t tv
typSubsts maps = \case
  t0@TNam {} -> t0
  t0@(TVar v) -> fromMaybe t0 $ lookup v maps
  TApp f a -> TApp (typSubsts maps f) (typSubsts maps a)
  TArr a r -> TArr (typSubsts maps a) (typSubsts maps r)
  t0@(TAbs v b) -> if any ((v ==) . fst) maps then t0 else TAbs v (typSubsts maps b)

typSubst :: Eq tv => TypComponent t tv -> tv -> TypComponent t tv -> TypComponent t tv
typSubst t tv = typSubsts [(tv, t)]

unsnocT :: TypComponent t v -> ([TypComponent t v], TypComponent t v)
unsnocT v = let (ts,t) = go [] v in
  (reverse ts, t)
  where go xs (TArr a b) = go (a:xs) b
        go xs a = (xs,a)

getRoot :: TypComponent t v -> TypComponent t v
getRoot = \case
  TApp t _ -> getRoot t
  t -> t

foldCode :: (a -> Segment v c t tv -> a) -> a -> Code v c t tv -> a
foldCode _ i EOF = i
foldCode f i (M c s) = f (foldCode f i c) s

filterComments :: Code v c t tv -> Code v c t tv
filterComments = foldCode f EOF
  where
    f c Comment {} = c
    f c s = M c s

instance Functor Tuples where
  fmap f (Tup a as) = Tup (f a) (fmap f as)

isList :: Doc ann -> Bool
isList d = case renderLazy . layoutPretty defaultLayoutOptions $ d of
  ":" -> True
  _ -> False

-------------------------------------------------------------------------------
--- Pretty printing -----------------------------------------------------------
-------------------------------------------------------------------------------

type W = ReaderT (Map Text Text) (Writer (Map Text Text))


renameCode :: HCode -> W ()
renameCode = \case
  EOF -> pure ()
  M c s -> renameSeg s >> renameCode c

renameSeg :: HSegment -> W ()
renameSeg = \case
  Data {..} -> renameVT tName >>
               mapM_ renameVt tArgs >>
               mapM_ renameCtor constructors >>
               mapM_ renameDeriv deriv
  Alias {..} -> renameVT tName >> mapM_ renameVt tArgs >> renameTyp typ
  Decl {..} -> renameVv name >> renameTyp typ >> renameExpr body
  _ -> pure ()

renameExpr :: HExpr -> W ()
renameExpr = \case
  Var (Left  v) -> renameVv v
  Var (Right c) -> renameVC c
  Typ {..}      -> renameExpr body >> renameTyp typ
  App {..}      -> renameExpr fn >> renameExpr arg
  Lam {..}      -> renameVv binding >> renameTyp typ >> renameExpr body
  Let {..}      -> renameVv binding >> renameTyp typ >> renameExpr val >> renameExpr body
  Cas {args = Tup a as, ..}      -> mapM_ renameExpr (a:as) >> mapM_ renameCase cas
  _             -> pure ()

renameCase :: HCaseOf -> W ()
renameCase = \case
  CaseOf {match = Tup a as, ..} -> mapM_ renameMatch (a:as) >> renameExpr body

renameMatch :: HMatch -> W ()
renameMatch = \case
  Match {..} -> renameVC ctor >> mapM_ (\case
                                           Le v -> renameVv v
                                           Mi c -> renameMatch c
                                           Ri t -> renameVT t) args

renameTyp :: HTyp -> W ()
renameTyp = \case
  T t -> renameTypC t
  TNone -> pure ()

renameTypC :: HTypC -> W ()
renameTypC = \case
  TNam t -> renameVT t
  TVar tv -> renameVt tv
  TApp t1 t2 -> renameTypC t1 >> renameTypC t2
  TArr t1 t2 -> renameTypC t1 >> renameTypC t2
  TAbs tv tc -> renameVt tv >> renameTypC tc

renameCtor :: HCtor -> W ()
renameCtor = \case
  Ctor {..} -> renameVC name >> renameTypC typ

renameDeriv :: HDeriv -> W ()
renameDeriv = \case
  Deriv {..} -> renameVT cls >> renameTyp via

renameVv :: Vv -> W ()
renameVv vv =
  let (t,t') = rVv vv in
    tell =<< (getNonGlobalName t' <&> Map.singleton t . \case
      Just t'' -> t''
      Nothing -> t')

renameVC :: VC -> W ()
renameVC vc =
  let (t,t') = rVC vc in
    tell =<< (getNonGlobalName t' <&> Map.singleton t . \case
      Just t'' -> t''
      Nothing -> t')

renameVT :: VT -> W ()
renameVT vt =
  let (t,t') = rVT vt in
    tell =<< (getNonGlobalName t' <&> Map.singleton t . \case
      Just t'' -> t''
      Nothing -> t')

renameVt :: Vt -> W ()
renameVt vt =
  let (t,t') = rVt vt in
    tell =<< (getNonGlobalName t' <&> Map.singleton t . \case
      Just t'' -> t''
      Nothing -> t')

getNonGlobalName :: Text -> W (Maybe Text)
getNonGlobalName t = asks (Map.lookup t)

-- | By using a known prefixes we ensure variable hygiene
prettyVar :: Text -> [Int] -> Text -> Text
prettyVar prefix us = T.append prefix' . toIdent
  where
    us' = map (T.pack . show) us
    prefix' = T.concat $ prefix : intersperse "-" us' ++ ["'"]
    toIdent = T.map $ \case
      '.' -> '_'
      '@' -> '_'
      c -> c

rVv :: Vv -> (Text,Text)
rVv (Vd us t) = (prettyVar "v" us t,t)
rVv (Vv t)    = (prettyVar "v" [] t,t)
rVv (Vvh t)   = (t,t)

rVC :: VC -> (Text,Text)
rVC (VC _ t@"Nat.zero") = ("zero", t)
rVC (VC _ t@"Nat.succ") = ("succ", t)
rVC (VC us t) = (prettyVar "C" us t,t)
rVC (VCh t)   = (t,t)

rVT :: VT -> (Text,Text)
rVT (VT _ _  t@"Nat") = ("Natural", t)
rVT (VT _ us t) = (prettyVar "T" us t,t)
rVT (VTh t)   = (t,t)

rVt :: Vt -> (Text,Text)
rVt (Vt t)    = ("Poly",t)

instance Pretty Vv where
  pretty = pretty . fst . rVv
instance Pretty VC where
  pretty = pretty . fst . rVC
instance Pretty VT where
  pretty = pretty . fst . rVT
instance Pretty Vt where
  pretty = pretty . fst . rVt


instance (Pretty t, Pretty tv) => Pretty (Typ t tv) where
  pretty TNone = mempty
  pretty (T t) = " ::" <+> pretty t

instance (Pretty t, Pretty tv) => Pretty (TypComponent t tv) where
  pretty = \case
    TNam n -> pretty n
    TVar v -> pretty v
    TApp tf ta -> flatten tf [ta]
      where
        opParen :: TypComponent t tv -> Doc a
        opParen = \case
          t@(TNam {}) -> pretty t
          t@(TVar {}) -> pretty t
          t -> parens (pretty t)
        flatten fn args = case fn of
          TApp tf' ta' -> flatten tf' (ta' : args)
          tf' -> group (
              opParen tf'
              <> indnt (vsep (map opParen args))
            )
    TArr t1 t2 -> group (addParen (pretty t1) <+> "->" <> indnt (pretty t2))
      where
        addParen = case t1 of
          TArr _ _ -> parens
          _        -> id
    TAbs v t -> "forall" <+> pretty v <+> "." <+> pretty t

instance (Pretty c, Pretty t, Pretty tv) => Pretty (Ctor c t tv) where
  pretty Ctor {..} = pretty name <+> "::" <+> pretty typ

instance Pretty a => Pretty (Tuples a) where
  pretty (Tup a as) = tupled (map pretty (a:as))

instance
  (Pretty v, Pretty c, Pretty t, Pretty tv) =>
  Pretty (Expr v c t tv)
  where
  pretty (Var (Left v)) = pretty v
  pretty (Var (Right c)) = pretty c
  pretty (Typ {..}) = parens (pretty body <> pretty typ)
  pretty (Lst lst) = list (map pretty lst)
  pretty App {fn = fn0, arg = arg0} = flatten fn0 [arg0]
    where
      opParen :: Expr v c t tv -> Doc a
      opParen = \case
        t@Var {} -> pretty t
        t@Int {} -> pretty t
        t@Str {} -> pretty t
        t@Lst {} -> pretty t
        t -> parens (pretty t)
      flatten fn' args = case fn' of
        App {..} -> flatten fn (arg : args)
        fn -> group (
            opParen fn <> indnt (vsep (map opParen args))
          )
  pretty Lam {..} =
    group ("\\" <> parens (pretty binding <> pretty typ) <+> "->" <>
      indnt (pretty body))
  pretty Let {..} =
    group (
      group ("let" <+> pretty binding <>
        indnt (pretty typ) <>
        indnt ("=" <+> pretty val)) </>
      "in" <+> nest 2 (pretty body))
  pretty Cas {..} = "case" <+> pretty args <+> "of"
    <> indnt (vsep (map pretty cas))
  pretty (Int n) = pretty n
  pretty (Str s) = enclose "\"" "\"" $ pretty s

instance (Pretty v, Pretty c, Pretty t) => Pretty (Match v c t) where
  pretty Match {..} =
    group (
    (if isList (pretty ctor)
    then
      parens (pretty ctor)
    else pretty ctor)
    <> hcat (map pArg args))
    where
      pArg = (" " <>) . \case
        Le t -> pretty t
        Mi c -> parens $ pretty c
        Ri tv -> pretty tv
instance
  (Pretty v, Pretty c, Pretty t, Pretty tv) =>
  Pretty (CaseOf v c t tv)
  where
  pretty CaseOf {..} =
    group (pretty match <+> "->" <>
      indnt (pretty body))

instance (Pretty t, Pretty tv) => Pretty (Deriv t tv) where
  pretty Deriv {..} = "deriving" <+> pretty cls <> case via of
    TNone -> mempty
    T tc -> " via" <+> parens (pretty tc)
instance
  (Pretty v, Pretty c, Pretty t, Pretty tv) =>
  Pretty (Segment v c t tv)
  where
  pretty = \case
    s@Data {..} ->
      "data" <+> prettyTypeDeclName s
        <> whenMembers constructors
          (" where" <> indnt (vsep (map pretty constructors)))
        <> whenMembers deriv (indnt $ vsep $ map pretty deriv)

    s@Alias {..} ->
      "type" <+> prettyTypeDeclName s <+> "=" <+> case typ of
        TNone -> "_"
        T tc -> pretty tc
    Decl {..} ->
      typeDecl <>
      group (pretty name <+> "=" <> indnt (pretty body))
      where
        typeDecl = case typ of
          TNone -> mempty
          _ -> pretty name <> pretty typ <> line
    Comment c -> "{-" <+> pretty c <+> "-}"
    Raw r -> pretty r
    where
      prettyTypeDeclName :: (Pretty t, Pretty tv) => Segment v c t tv -> Doc a
      prettyTypeDeclName s =
        pretty (tName s) <> hcat (map ((" " <>) . pretty) (tArgs s))

      whenMembers :: [m] -> Doc a -> Doc a
      whenMembers [] _ = mempty
      whenMembers _  d = d

instance
  (Pretty v, Pretty c, Pretty t, Pretty tv) =>
  Pretty (Code v c t tv) where
  pretty EOF = mempty
  pretty (M EOF s) = pretty s
  pretty (M c@(M _ Comment {}) s) = pretty c </> pretty s
  pretty (M c s) = pretty c </> line <> pretty s
