{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Lean.Prop where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor
import Data.List (groupBy)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Prettyprinter
import Prettyprinter.Render.Text (renderLazy)
import QuickSpec.Internal.Haskell (Constant (..))
import QuickSpec.Internal.Prop (Equation (..), Prop (..))
import QuickSpec.Internal.Term (MetaVar (..), Term (..), Var (..))
import QuickSpec.Internal.Type (TyCon (..), Type)
import Twee.Term qualified as TW
import Data.Function (on)
import Control.Monad.RWS (RWS, runRWS, get, modify)

type Arg = (Text, (Bool, Type))
type S = RWS (Map Text Text) [Arg] (Map Int Text)
type R = Reader (Map Text Text)

prop2Lean :: Map Text Text -> Prop (Term Constant) -> Doc ann
prop2Lean m v =
  let
    (eq, _, vars) = runRWS (prop2Lean' v) m Map.empty
    args = flip runReader m $ mapM termConst $ groupBy ((==) `on` snd) vars
  in
  hsep args <+> ":" <+> eq

prop2Lean' :: Prop (Term Constant) -> S (Doc ann)
prop2Lean' ([] :=>: rhs) = eqToLean rhs
prop2Lean' (lhs :=>: rhs) =
  mapM eqToLean lhs >>= \lhs' ->
  eqToLean rhs <&> ((hsep (punctuate "∧" lhs') <+> "→") <+>)

eqToLean :: Equation (Term Constant) -> S (Doc ann)
eqToLean (x :=: y) =
  termToLean x >>= \x' ->
  termToLean y >>= \y' ->
  let xt = renderLazy . layoutPretty defaultLayoutOptions $ x'
      yt = renderLazy . layoutPretty defaultLayoutOptions $ y'
  in if | xt == "True" -> pure y'
        | yt == "True" -> pure x'
        | otherwise -> pure $ x' <+> "=" <+> y'

tVarId :: Int -> Text
tVarId i = "X" <> T.pack (show i)

termToLean :: Term Constant -> S (Doc ann)
termToLean (Var V {..}) = do
  m <- get
  pretty <$> case Map.lookup var_id m of
    Just n -> pure n
    Nothing -> do
      let s = V.unsafeIndex varNames $ Map.size m
      modify (Map.insert var_id s)
      tell [(s, (True, var_ty))]
      pure s
termToLean (Hole MV{..}) = do
  tell [(T.pack hole_id, (False, hole_ty))]
  pure (pretty hole_id)
termToLean (Fun f) = constantToLean f
termToLean (a :$: b) =
  termToLean a >>= \a' ->
  termToLean b <&> (case b of (_ :$: _) -> parens; _ -> id;) >>= \b' ->
  pure $ a' <+> b'

constantToLean :: Constant -> S (Doc ann)
constantToLean Constant {..} =
  ask >>= \env ->
  let t = runReader (typeToLean con_type) env in
  pure $ if containsArrow con_type then pretty con_name else parens $ pretty con_name <+> ":" <+> t

varNames :: V.Vector Text
varNames = V.fromList $ map T.singleton $ "xyzw" ++ ['a'..'v']

termConst :: [Arg] -> R (Doc ann)
termConst as@((_, (_, t)):_) = do
  a <- ask
  let t' = runReader (typeToLean t) a
  pure . parens $ hsep (map (pretty . fst) as) <+> ":" <+> t'
termConst _ = error "assumption fail"

typeToLean :: Type -> R (Doc ann)
typeToLean (TW.Var (TW.V i)) = pure $ pretty $ tVarId i
typeToLean (TW.App (TW.F _ t) TW.Empty) = tyconToLean t
typeToLean (TW.App (TW.F _ t) as@(TW.Cons _ _)) =
  case (t,as) of
    (Arrow,(TW.Cons t1 (TW.Cons t2 TW.Empty))) ->
      typeToLean t1 <&> (if containsArrow t1 then parens else id) >>= \t1' ->
      typeToLean t2 >>= \t2' ->
      pure $ t1' <+> "→" <+> t2'
    _ ->
      let as' = TW.unpack as in
        tyconToLean t >>= \t' ->
        mapM typeToLean as' >>= \as'' ->
        pure $ t' <+> hsep as''

tyconToLean :: TyCon -> R (Doc ann)
tyconToLean Arrow = pure "→"
tyconToLean (String s) = ask >>= \m -> case m Map.!? (T.pack s) of
  Just s' -> pure $ pretty s'
  Nothing -> pure $ pretty s
tyconToLean (TyCon x)  =
  let s = T.pack (show x) in
  ask >>= \m -> case m Map.!? s of
    Just s' -> pure $ pretty s'
    Nothing -> pure $ pretty s

containsArrow :: Type -> Bool
containsArrow (TW.App (TW.F _ Arrow) _) = True
containsArrow (TW.App (TW.F _ _) as@(TW.Cons _ _)) =
  any containsArrow $ TW.unpack as
containsArrow _ = False
