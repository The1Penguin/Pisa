{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Lean.Translation
  ( leanAsHaskell,
    TranslationResult,
  )
where

import Control.Monad (zipWithM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.RWS (RWS, asks, gets, local, modify, runRWS, tell)
import Data.Functor ((<&>))
import Data.List ((!?))
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace
import GHC.Stack (CallStack, HasCallStack, callStack)
import Lean.AST
import Lean.AST qualified as E (Expr (..))
import Lean.Haskell
  ( HCode,
    HExpr,
    HTypC,
    HCtor,
    VC (VC, VCh),
    VT (VT, VTh),
    Vt (Vt),
    Vv (Vd, Vv, Vvh),
    E (..),
    prettyVar
  )
import Lean.Haskell qualified as H
import Lean.Util
import Prettyprinter
  ( Doc,
    Pretty,
    comma,
    defaultLayoutOptions,
    encloseSep,
    group,
    layoutPretty,
    pretty,
    space,
    (<+>), viaShow,
  )
import Prettyprinter.Render.Text (renderLazy)
import Prelude hiding (fail)
import Control.Monad.Fix (MonadFix (mfix))

-------------------------------------------------------------------------------
--- Translation computations `T Generated` ------------------------------------
-------------------------------------------------------------------------------

{-

The expression `0ᵝ` cannot be further evaluated unless provided a context where
the variable is bound

Similarly `λα → α` require a context to mean much, however without it we may
still decide to transform it into Haskell code

The example `let α = 1 in α` is also complete

Notice how the first example `0ᵝ` could be the body of either of the two
previous examples.

-}

-- | Represent the result of generating a piece of lean code as haskell
data Generated
  = -- | An evaluation that has not finished, should only be transient
    Incomplete
  | -- | Haskell representation of code with an associated type signature
    Code (HExpr, HTypC)
  | -- | Meta variables prevented construction of a concrete value
    -- TODO: Is there always a single unique defer where code don't reslove?
    -- We probably want a more fine grained way to request information if this
    -- is our approach
    -- TODO: Also, this control flow really should be part of the monad stack
    -- to make the code easier to work with. Something like:
    --   arg <- request ?Condition
    -- Maybe that's algebraic effects, or just CPS
    Defered (Bound -> T Generated)
  | -- | A type that likely will be consumed by a Defered,
    -- or must have a runtime representation
    Typ HTypC
  | -- |
    SortOf Int

-- | Lean Definitions
type Globals = Map Name Out

-- | Translation state
data St = St
  { -- | Keep track of already generated instances to prevent loops and duplicates
    instantiations :: Map (Name, [Int]) Generated
  }

type Expt = ExceptT (PisaErr, CallStack)

type DebugStack = [T.Text]

-- | A computation that can fail while still retaining partial output
type T = Expt (RWS (Globals, DebugStack) HCode St)

-- | Include a stack trace in failed computations
instance {-# OVERLAPS #-} Fail T where
  fail err = throwError (err, callStack)


-------------------------------------------------------------------------------
--- Bound Lean Expressions ----------------------------------------------------
-------------------------------------------------------------------------------

data ScopeVar
  = -- | A typed var is expected to be available in the scope of the generated
    -- code via the provided name
    -- Notably these references may not exist if the scope is transposed
    VarRef {varIdent :: Name, varType :: Bound}
  | -- | A resolved variable may not exist at runtime, and thus lacks a name
    -- The value and type may however reference runtime variables.
    ResolvedVar {varType :: Bound, varVal :: Bound, origName :: Name}
  | ResolvedTyp {varType :: Bound, origName :: Name}

-- | Get the name used for a scope var in the AST
varName :: ScopeVar -> Name
varName = \case
  VarRef {varIdent} -> varIdent
  ResolvedVar {origName} -> origName
  ResolvedTyp {origName} -> origName

type DeBruijnIndex = [ScopeVar]

type UnivLookup = [(Name, Int)]

-- TODO: How do we roboustly prevent bound expressions from being evaluated in
-- contexts where the references are lost?
--
-- I can only think of multi-pass approaches. Eg. one pass where variables are
-- first uniquely identified. Then adding back-references to traverse the tree
-- in both directions is a substitute to binding the variables each time.
--
-- Although, universe variables still need to be bound, so this needs to be
-- thought through further.
--
-- We likely need a way to additionally keep track of what's available in the
-- current context of code being generated.

-- | An Expr in a context. This binds all de-Bruijn indices to their values,
-- but no guarantees of those refences existing in other contexts are given.
-- If a bound expression is pushed down into a sub-expression variables will
-- be defined. However they may be shadowed
data Bound = Bound {univs :: UnivLookup, scope :: DeBruijnIndex, expr :: Expr}

pushTypVar :: UnivLookup -> Name -> Expr -> DeBruijnIndex -> DeBruijnIndex
pushTypVar us n t scope = VarRef n (Bound us scope t) : scope

pushValVar :: UnivLookup -> Name -> Expr -> Expr -> DeBruijnIndex -> DeBruijnIndex
pushValVar us n t v scope = ResolvedVar (expr t) (expr v) n : scope
  where expr = Bound us scope

-- | Inherit the context of a bound variable to an expression
-- This is intended for subexpressions that don't extend the scope
bindSub :: Bound -> Expr -> Bound
bindSub Bound {..} e = Bound {expr = e, ..}

-- | Inherit the context of a bound variable and push a variable of given name
-- and type to the scope.
-- This is intended for subexpressions that are abstractions
bindSubTyp :: Bound -> Name -> Expr -> Expr -> Bound
bindSubTyp b n t e = Bound (univs b) (pushTypVar (univs b) n t (scope b)) e

-- | Inherit the context of a bound variable and push a variable of given name,
-- type, and value to the scope.
-- This is intended for subexpressions like LetE
bindSubVal :: Bound -> Name -> Expr -> Expr -> Expr -> Bound
bindSubVal b n t v e =
  Bound (univs b) (pushValVar (univs b) n t v (scope b)) e

expectVar :: (HasCallStack, Applicative m, Fail m) => DeBruijnIndex -> Int -> m ScopeVar
expectVar scope i = maybe (fail "Expected scoped var missing") pure $ scope !? i

-- | Extract the boudy of a bound type and ensure it's bound correctly
bindBodyTyp :: HasCallStack => Bound -> T Bound
bindBodyTyp b@Bound {expr = e} = case e of
  Lam {..} -> pure $ bindSubTyp b binder typ body
  ForAllE {..} -> pure $ bindSubTyp b binder typ body
  LetE {..} -> pure $ bindSubTyp b name typ body
  _ -> expectationFail "Binding body"


-------------------------------------------------------------------------------
--- Debugging helpers ---------------------------------------------------------
-------------------------------------------------------------------------------

instance Pretty ScopeVar where
  pretty = \case
    VarRef {varIdent = n, varType = t} -> "\"" <> pretty n <> "\" =" <+> pretty t
    ResolvedVar {varType = t, varVal = v, origName = n} ->
      group (pretty n <> ": " <+> pretty t </> "=" <+> pretty v)
    ResolvedTyp {varType = t, origName = n} ->
      pretty n <> ": " <+> pretty t


instance Pretty Bound where
  pretty :: Bound -> Doc a
  pretty Bound {expr, univs, scope} =
    group
      ( "@" <> encloseSep ".{" "}" (comma <> space) us <+>
        "|" <> pretty (length scope) <> "|" </>
        prettyExpr (Just $ map varName scope) Nothing expr
      )
    where us = map (\(n, i) -> pretty n <> ":" <+> pretty i) univs

eKind :: Expr -> T.Text
eKind = \case
  Bvar {} -> "ᵝ"
  Fvar {} -> "Fvar"
  Mvar {} -> "Mvar"
  Sort {} -> "Sort"
  Const {} -> "ℭ"
  App {} -> "∘"
  Lam {} -> "λ"
  ForAllE {} -> "∀"
  LetE {} -> "="
  Lit {} -> "Lit"
  Mdata {} -> "Mdata"
  Proj {} -> "⇒"

annotate :: Pretty a => T.Text -> a -> T ()
annotate desc d = write $ doc
  where
    doc = group $ group (pretty desc) <> indnt (pretty d)
    write = writeSeg . H.Comment . renderLazy . layoutPretty defaultLayoutOptions

addTrace :: T.Text -> T a -> T a
addTrace t = local (fmap (t:))

noteTrace :: T ()
noteTrace = do
  traces <- asks snd
  traceM $ T.unpack $ T.concat $ reverse traces

todoPrint :: (HasCallStack, Pretty p) => T.Text -> p -> T a
todoPrint desc d = do
  annotate desc d
  todo $ T.unpack desc

-------------------------------------------------------------------------------
--- Level utilities -----------------------------------------------------------
-------------------------------------------------------------------------------

-- | Given a scope, try to calculate a final Sort level
sortWith :: [(Name, Int)] -> Level -> Maybe Int
sortWith scope = \case
  Param p -> lookup p scope
  Zero -> Just 0
  Succ l -> (1 +) <$> sortWith scope l
  Max l0 l1 -> max <$> sortWith scope l0 <*> sortWith scope l1
  IMax l0 l1 -> do
    l1' <- sortWith scope l1
    if l1' == 0 then Just 0
    else max l1' <$> sortWith scope l0
  LMVarId _ -> Nothing -- TODO: How does this work?

getLevel :: (HasCallStack, Applicative m, Fail m) => UnivLookup -> Level -> m Int
getLevel uScope l =
  maybe (fail "Encountered free variables in level") pure $ sortWith uScope l


-------------------------------------------------------------------------------
--- Translation utils ---------------------------------------------------------
-------------------------------------------------------------------------------

getGlobal :: Name -> T (Maybe Out)
getGlobal n = asks $ Map.lookup n . fst

expectGlobal :: HasCallStack => Name -> T Out
expectGlobal n = getGlobal n >>= \case
  Just d -> pure d
  Nothing -> fail $ "Expected identifier `" ++ T.unpack n ++ "` to be in global scope"

writeSeg :: H.Tagged H.Segment -> T ()
writeSeg = tell . H.M H.EOF

defer :: (Bound -> T Generated) -> T Generated
defer f = do
  traces <- asks snd
  pure $ Defered $ \g -> do
    addTrace "↯" noteTrace
    local (fmap (const ("↦":traces))) $ f g

-- | Find the root of the resulting value of a type
resultConst :: Expr -> T (Name, [Level])
resultConst = \case
  Const {..} -> pure (name, levels)
  ForAllE {..} -> resultConst body
  App {..} -> resultConst function
  _ -> expectationFail "Root result type"


-------------------------------------------------------------------------------
--- Translation entrypoint ----------------------------------------------------
-------------------------------------------------------------------------------

type TranslationResult = Either (PisaErr, CallStack, HCode) HCode

leanAsHaskell :: [(Name, Name)] -> Map Name Out -> TranslationResult
leanAsHaskell roots globals =
  case runRWS (runExceptT translation) (globals,[]) initState of
    (Left (e,s), _, code) -> Left (e, s, code)
    (_, _, code) -> Right code
  where
    initState = St Map.empty

    header =
        "{-# LANGUAGE DeriveGeneric #-}\n\
        \{-# LANGUAGE DerivingVia #-}\n\
        \{-# Language OverloadedStrings #-}\n\
        \{-# Language LambdaCase #-}\n\
        \{-# Language RecordWildCards #-}\n\
        \{-# Language DuplicateRecordFields #-}\n\
        \{-# Language DeriveAnyClass #-}\n\
        \module A where\n\
        \import GHC.Generics (Generic)\n\
        \import GHC.Natural (Natural)\n\
        \import Lean.Eval\n\
        \import Lean.IR\n\
        \import Data.Map.Lazy (Map, fromList)\n\
        \import Control.DeepSeq (NFData)\n\
        \import Test.QuickCheck\n\
        \import Test.QuickCheck.Arbitrary.Generic\n\
        \import QuickSpec"

    env m =
      "environment :: Map Name Decl\n\
      \environment = " <> TL.pack (show m)

    translateRoots :: (Name,Name) -> T [HExpr]
    translateRoots (root,rootSimpler) =
      expectGlobal root >>= \case
        ConInfo d@Def {levelParams} ->
          l2hDef (map (const 0) levelParams) d >>= \case
            Typ (H.TNam t) -> pure . pure $ H.App
              { fn = H.Var $ Left $ Vvh "monoType"
              , arg = H.Typ
                { body = H.Var $ Left $ Vvh "Proxy"
                , typ = H.T $ H.TApp (H.TNam (VTh "Proxy")) $ H.TNam t
                }
              }
            Code (H.Var v, _) -> pure . pure $ H.App
              { fn = H.App {fn = H.Var $ Left $ Vvh "con", arg = H.Str rootSimpler}
              , arg = H.Var v
              }
            _ -> fail "Unexpected root"
        DeclT t level decl -> do
          let intern = (prettyVar "v" [] root)
          (ts, resultType) <- l2hTyp (Bound (("Poly",0):map (,0) level) [] t) >>=
            mfix (\cases
                  f g (Defered x) -> g x >>= f <&> \(n,m) -> (n,m+1)
                  _ _ x -> pure (x,0)
                ) ($ Bound [("Poly",0)] [] $ Const {name = "Poly", levels = [ Zero ]}) >>= \case
            (Typ t,n) -> pure (H.unsnocT t) <&> \(ts,r) -> (drop n ts,r)
            _ -> fail ""

          writeSeg . H.Raw . renderLazy . layoutPretty defaultLayoutOptions $
            pretty intern <+> "=" <+> viaShow decl

          let ts' = zip (map (Vv . T.pack . (: [])) ['a' .. 'z']) ts

          pure . pure $ H.App
            { fn = H.App {fn = H.Var $ Left $ Vvh "con", arg = H.Str rootSimpler}
            , arg =
              foldr (\(n,_) b -> H.Lam n H.TNone b)
                H.App {
                  fn = H.Var . Left . Vv $ "fromVal" <> (case H.getRoot resultType of H.TNam (VT _ _ v) -> v; _ -> error ""),
                  arg =
                  H.App {
                    fn = H.App {
                           fn = H.App {
                             fn = H.Var $ Left $ Vvh "eval",
                             arg = H.Var $ Left $ Vvh intern
                           },
                           arg = H.Var $ Left $ Vvh "environment"
                         },
                    arg = H.Lst $ map (\(n,m) ->
                      let (H.TNam (VT _ _ v)) = H.getRoot m in
                        H.App (H.Var . Left . Vv $ "toVal" <> v) (H.Var . Left $ n))
                      ts'
                  }
                }
              ts'
            }
        _ -> pure []

    translation :: T ()
    translation = do
      writeSeg $ H.Raw header

      writeSeg . H.Raw . env .
        Map.mapMaybe (\case Decl f -> Just f; DeclT _ _ f -> Just f; _ -> Nothing) $
        globals

      roots' <- mapM translateRoots roots <&>
                  ((H.Var $ Left $ Vvh "monoType (Proxy :: Proxy Poly)") :) .
                  concat

      writeSeg (H.Decl
        { name = Vvh "sigs"
        , typ = H.T $ H.TNam $ VTh "[Sig]"
        , body = H.Lst roots'
        })

l2hDef :: [Int] -> Def -> T Generated
l2hDef lvlArgs d0@Def {name = defName, typ = defTyp, ..} = withMemo
  where
  uScope = zip levelParams lvlArgs
  ident = (defName, lvlArgs)

  set v = modify $ \st ->
    st { instantiations = Map.insert ident v (instantiations st) }

  withMemo :: T Generated
  withMemo = do
    gets (Map.lookup ident . instantiations) >>= \case
      Just r -> do
        traceM $ "cached def:" ++ T.unpack defName
        pure r
      Nothing -> local (fmap (const [defName])) $ case defName of
        "Nat" -> do
          let t = H.TNam $ VT False lvlArgs defName
          writeSeg $ H.Decl (Vv $ "toVal" <> defName) (H.T $ H.TArr t (H.TNam  $ VTh "Val")) $
            H.Lam (Vv "n") (H.TNone) $ H.App (H.Var $ Right $ VCh "Unsigned") (H.Var $ Left $ Vv "n")
          writeSeg . H.Decl (Vv $ "fromVal" <> defName) (H.T $ H.TArr (H.TNam $ VTh "Val") t) $
            H.Lam (Vv "a") (H.T $ H.TNam $ VTh "Val") $ H.Cas
              (flip H.Tup [] $ H.Var $ Left $ Vv "a") $
                [ H.CaseOf (flip H.Tup [] $ H.Match (VCh "Unsigned") [Le $ Vv "n"]) $ H.Var $ Left $ Vv "n" ]
          set $ Typ t
          pure $ Typ t
        _ -> do
          traceM $ "translating def: " ++ T.unpack defName
          set Incomplete -- Mark as visited, to prevent loops
          l2hDef'
          gets (Map.lookup ident . instantiations) >>= \case
            Nothing -> fail "Forgot generated result"
            Just Incomplete -> fail "Failed to generate a translation"
            Just a -> pure $ a

  l2hDef' :: T ()
  l2hDef' = case kind of
    Inductive { ctors, isUnsafe = False, isReflexive = False } ->
      let
        collect :: Expr -> [(Int,Expr)] -> Either String (Expr, [Name], [(Int,Bound)])
        collect te ces = collect' (uncurry (zip3 (repeat [])) $ unzip ces) id te
          where
            collect' :: [(DeBruijnIndex, Int, Expr)] -> ([Name] -> [Name]) -> Expr -> Either String (Expr, [Name], [(Int,Bound)])
            collect' cs ns = \case
              t@ForAllE { info = BDefault } ->
                let withCs f = mapM f cs >>= \cs' ->
                      collect' cs' (\l -> ns (E.binder t:l)) (E.body t)
                in withCs $ \case
                  (scope, s, ForAllE { info = BImplicit, binder, typ, body }) -> do
                    -- Expecting initial arguments implicit reflecting type arguments
                    expectation "Binder name" (binder == E.binder t && typ == E.typ t)
                    pure (pushTypVar uScope binder typ scope, s, body)
                  _ -> throwError "Unknown constructor type"
              other -> pure (other, ns [], map (\(b,s,e) -> (s, Bound uScope b e)) cs)

        cDef n = expectGlobal n >>= \case
          ConInfo Def { kind = Constructor { isUnsafe = True } } ->
            todo "Unsafe constructor"
          ConInfo Def { kind = Constructor {cidx}, typ } -> pure (cidx,typ)
          _ -> fail "Non-constructor constructor"

        regCtor :: T.Text -> Int -> Bound -> T (Int,HCtor)
        regCtor n s ctorTyp = do
          (typN, levels) <- resultConst (expr ctorTyp)
          levels' <- mapM (getLevel uScope) levels
          expectation "Type universe" (levels' == lvlArgs)
          expectation "Type name" (typN == defName)
          addTrace (T.append ":" n) (l2hTyp ctorTyp) >>= \case
            Typ t -> do
              let (ts,t') = H.unsnocT t in
                pure . (s,) $ H.Ctor (VC lvlArgs n) (foldr (H.TArr . H.getRoot) (H.getRoot t') ts)
            _ -> todoPrint "Complex constructor type" ctorTyp

      in mapM cDef ctors >>= catchTo . collect defTyp >>= \case
        (Sort l, ns, cts) -> getLevel uScope l >>= \case
          1 -> do
            let tIdent = VT False lvlArgs defName
            set $ Typ $ H.TNam tIdent

            ctors' <- zipWithM (uncurry . regCtor) ctors cts

            writeSeg $ H.Data tIdent [] (map snd ctors')
              [ H.Deriv (VTh "Eq") H.TNone
              , H.Deriv (VTh "Generic") H.TNone
              , H.Deriv (VTh "Ord") H.TNone
              , H.Deriv (VTh "Show") H.TNone
              , H.Deriv (VTh "anyclass NFData") H.TNone
              , H.Deriv (VTh "Arbitrary")
                $ H.T
                $ H.TApp
                  (H.TNam (VTh "GenericArbitrary"))
                $ H.TNam tIdent
              ]

            let isEnum = all (\case (_,H.Ctor _ (H.TArr _ _)) -> False; _ -> True) ctors'
            let tIdent = VT isEnum lvlArgs defName
            set $ Typ $ H.TNam tIdent
            let enumOrdering =
                  let go acc (x@(_,t):xs) = case H.getRoot t of
                                    (H.TNam (VT True _ _)) -> x : go acc xs
                                    _                      -> go (x:acc) xs
                      go acc [] = reverse acc in
                  go []

            writeSeg . H.Decl (Vv $ "toVal" <> defName) (H.T $ H.TArr (H.TNam tIdent) (H.TNam  $ VTh "Val")) $
              H.Lam (Vv "a") (H.TNone) $ H.Cas
                (flip H.Tup [] . H.Var $ Left $ Vv "a") $
                  map (\(s,H.Ctor c args) ->
                        let ts = zip (map (Vv . T.pack . (: [])) ['b' .. 'z']) . fst $ H.unsnocT args in
                        H.CaseOf (flip H.Tup [] $ H.Match c (map (Le . fst) ts)) $
                          if isEnum then
                            H.App (H.Var . Right $ VCh "Unsigned" ) (H.Int $ toInteger s)
                          else
                            H.App (H.App (H.Var . Right $ VCh "VCtor") (H.App (H.App (H.Var . Right $ VCh "Object") (H.Int 1)) (H.Int $ toInteger s))) $
                            H.Lst $
                            map (\case
                                      (n,(H.TNam (VT _ _ v))) -> (H.App (H.Var . Left . Vv $ "toVal" <> v) (H.Var . Left $ n))
                                      (n,(H.TVar (Vt   v  ))) -> (H.App (H.Var . Left . Vv $ "toValPoly") (H.Var . Left $ n))
                                      (n,a@(H.TApp _ _     )) -> let H.TNam (VT _ _ v) = H.getRoot a in (H.App (H.Var . Left . Vv $ "toVal" <> v) (H.Var . Left $ n))
                                      (_,m) -> error (show m))
                            (enumOrdering ts)
                      ) ctors'

            writeSeg . H.Decl (Vv $ "fromVal" <> defName) (H.T $ H.TArr (H.TNam $ VTh "Val") (H.TNam tIdent)) $
              H.Lam (Vv "a") (H.T $ H.TNam $ VTh "Val") $ H.Cas
                (flip H.Tup [] . H.Var $ Left $ Vv "a") $
                  map (\(s,H.Ctor c args) ->
                        let ts = zip (map (Vv . T.pack . (: [])) ['b' .. 'z']) . fst $ H.unsnocT args
                            g a b c = Mi $ H.Match a [b,c]
                        in
                         H.CaseOf
                           (if isEnum then
                              flip H.Tup [] $ H.Match (VCh "Unsigned") [Le . Vvh . T.pack $ show s]
                            else
                              flip H.Tup [] $ H.Match (VCh "VCtor") $
                              [ (Mi (H.Match (VCh "Object") [Le $ Vvh "_", Le . Vvh . T.pack $ show s]))
                              , foldr (\(n,_) -> g (VCh ":") (Le $ n)) (Mi $ H.Match (VCh "[]") []) (enumOrdering ts)
                              ]
                           )
                           (foldr
                            (\case
                                (n,(H.TNam (VT _ _ v))) -> flip H.App (H.App (H.Var . Left . Vv $ "fromVal" <> v) (H.Var . Left $ n))
                                (n,(H.TVar (Vt _    ))) -> flip H.App (H.App (H.Var . Left . Vv $ "fromValPoly") (H.Var . Left $ n))
                                (n,a@(H.TApp _ _     )) -> let H.TNam (VT _ _ v) = H.getRoot a in flip H.App (H.App (H.Var . Left . Vv $ "fromVal" <> v) (H.Var . Left $ n))
                                (_,m) -> error (show m))
                            (H.Var $ Right c)
                            (reverse ts))
                      ) ctors'

          _ -> todo "Non-type Inductive"
        _ -> todo "Other inductive"

    Constructor { induct, isUnsafe = False } -> do
      expectGlobal induct >>= \case
        ConInfo d -> l2hDef lvlArgs d >>= \case
          Typ (H.TNam (VT _ la n)) -> do
            expectation "Correct induct resolution" (induct == n && lvlArgs == la)
            annotate "Constructor" defTyp
            gt <- l2hTyp (Bound (("Poly",0):uScope) [] defTyp) >>=
              mfix (\cases
                    f g (Defered x) -> g x >>= f <&> \(n,m) -> (n,m+1)
                    _ _ x -> pure (x,0)
                  ) ($ Bound [("Poly",0)] [] $ Const {name = "Poly", levels = [ Zero ]}) >>= \case
              (Typ t,n) -> pure (H.unsnocT t) <&> \(ts,r) -> Typ $ foldr H.TArr r (drop n ts)
              _ -> fail ""
            set =<< collect pure gt
            where
              collect :: (HTypC -> T HTypC) -> Generated -> T Generated
              collect tf = \case
                Typ t -> do
                  t' <- tf t 
                  annotate "Final layer" (t, t')
                  pure $ Code (H.Var $ Right $ VC lvlArgs defName, t')
                Defered f -> defer $ \t -> do
                  annotate "Another layer" t
                  let next = collect $ \case
                        H.TArr _ b -> pure b
                        ct -> todoPrint "Unexpected constructor type" ct
                  f t >>= next
                _ -> todoPrint "Advanced constructor" defTyp
          _ -> fail "Constructor of non-type?"
        _ -> fail "Non def constructor"

    d -> do
      trace (show d) todoPrint "Unknown Definition" d0


l2hTyp :: HasCallStack => Bound -> T Generated
l2hTyp e = addTrace (T.append "ᵗ" $ eKind (expr e)) $ do
  noteTrace
  annotate "typ" e
  l2hTyp' e
  where
    l2hTyp' :: HasCallStack => Bound -> T Generated
    l2hTyp' b@Bound {univs = u, scope = scp, expr = e} = case e of
      -- TODO: What is a sound way to signal this.
      -- I'm not sure if this defer is sound, it may cause havoc in some places?
      Sort { level } -> SortOf <$> getLevel u level
      Bvar {..} -> do
        expectVar scp index >>= \case
          VarRef {varIdent, varType} -> case expr varType of
            Sort {level} -> do
              expectation "Type variable level" =<< (1 ==) <$> getLevel u level
              pure $ Typ $ H.TVar $ Vt $ varIdent
            t -> todoPrint "Unknown de Bruijn type reference" t
          ResolvedVar {varType, varVal} ->
            todoPrint "Type reference resolved var" (varType, varVal)
          ResolvedTyp {varType} -> l2hTyp varType
      Const {name = "Poly"} -> pure . Typ . H.TNam $ VT False [0] "Poly"
      Const {name, levels} -> do
        ls <- mapM (getLevel u) levels
        expectGlobal name >>= \case
          ConInfo d -> l2hDef ls d >>= \case
            Typ (H.TNam t) -> pure $ Typ $ H.TNam t
            _ -> expectationFail "Type const reference"
          _ -> fail "Found non Coninfo for Const typ"
      ForAllE {info = BDefault, binder, typ, body } ->
        l2hTyp (Bound u scp typ) >>= \case
          Typ typ' -> l2hTyp (Bound u (pushTypVar u binder typ scp) body) >>= \case
            Typ body' -> pure $ Typ $ H.TArr typ' body'
            SortOf _ -> todo "Manage SortOf"
            _ -> todoPrint "Complex body type" b
          _ -> todoPrint "Complex parameter type" b
      ForAllE {info = BImplicit, binder, typ, body } ->
        -- TODO: Check if provided arg is equivalent to typ
        let g typ' = \case
              Typ body' -> pure $ Typ $ H.TArr typ' $ H.typSubst typ' (Vt binder) body'
              Defered f -> defer $ \t -> f t >>= g typ'
              _ -> todoPrint "Complex body type" b
        in defer $ \a -> l2hTyp a >>= \case
                Typ typ' ->
                  l2hTyp (Bound u (pushTypVar u binder typ scp) body) >>= g typ'
                _ -> todoPrint "Complex parameter provided" b
      -- TODO: push down arg in scope of function
      App {function, arg} -> l2hTyp (Bound u scp function) >>= \case
        Typ fn' -> l2hTyp (Bound u scp arg) >>= \case
          Typ arg' -> pure $ Typ $ H.TApp fn' arg'
          _ -> todoPrint "Complex argument type" b
        _ -> todoPrint "Complex function type" b
      Mdata {body} -> do
        annotate "Mdata for" b
        l2hTyp (Bound u scp body)
      t -> todoPrint "Unknown type" t
