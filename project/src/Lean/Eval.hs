{-# OPTIONS_GHC -fdefer-typed-holes #-} -- TODO: DURING DEV ONLY
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language RecordWildCards #-}
{-# Language DuplicateRecordFields #-}
{-# Language OverloadedRecordDot #-}
module Lean.Eval where

import Control.Monad.RWS (RWS, MonadState, evalRWS, modify, get, put, asks, gets, lift)
import Control.Monad.Except
import Data.Functor
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Lean.IR
import Control.Monad (zipWithM_)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

todo :: String -> ρ
todo = error . ("TODO! " <>)

data St = St {
  vars :: Map VarId Val,
  jmps :: Map JoinPointId ([Param], FnBody)
  }

type Funcs = Map Name Decl

type Exec = ExceptT String (RWS Funcs () St)

builtins :: Funcs
builtins = undefined

eval :: Decl -> Map Name Decl -> [Val] -> Val
eval FDecl  {..} env vals =
  let args = flip St mempty . Map.fromList $ zipWith ((,) . VarId ) [1..] vals
  in
  either error id $ if all (\Param {typ} -> typ == Irrelevant) xs then
    case ev bod (flip St mempty . Map.fromList $ map (\Param {x} -> (x,Irre)) xs) of
      Right (Partial {..}) -> ev b args
      x -> x
  else
    ev bod (if length vals == length (vars args) then args else error "Args wrong")
    where ev b s = fst $ evalRWS (runExceptT $ evalBody b) env s
eval Extern {ext} env vals = case runExcept $ evalExtern ext vals of
  Right v -> v
  Left e -> error e

evalBody :: FnBody -> Exec Val
evalBody = \case
  Vdecl   {..} -> evalExpr e >>= updateToState x >> evalBody b
  Jdecl   {..} -> addJoinPoint j (xs,v) >> evalBody b
  Set     {..} -> getVal (case a of Var i -> i; Irr -> error "Trying to getVal from Irr") >>= \v -> getVal x >>= \case
    VCtor o vs -> updateToState x (VCtor o $ setAt i v vs) >> evalBody b
    _ -> error "Tring to Set non VCtor"
  SetTag  {..} -> getVal x >>= \case
    VCtor o vs -> updateToState x (VCtor (o { tag = cidx }) vs) >> evalBody b
    Obje o -> updateToState x (Obje o { tag = cidx }) >> evalBody b
    Unsigned _ -> updateToState x (Unsigned cidx) >> evalBody b
    _ -> error "Trying to SetTag on non VCtor, Obje, or Unsigned"
  Uset    {..} -> getVal y >>= \v -> getVal x >>= \case
    VCtor o vs -> updateToState x (VCtor o $ setAt i v vs) >> evalBody b
    _ -> error "Trying to Uset on non VCtor"
  Sset    {..} -> getVal y >>= \v -> getVal x >>= \case
    VCtor o vs -> updateToState x (VCtor o $ setAt (i+offset) v vs) >> evalBody b
    _ -> error "Trying to Sset on non VCtor"
  Inc     {..} -> evalBody b --TODO: This is a ease of code, should really handle RC
  Dec     {..} -> evalBody b --TODO: This is a ease of code, should really handle RC
  Del     {..} -> modify (\s@St{..} -> s {vars = Map.delete x vars}) >> evalBody b
  Mdata   {..} -> evalBody b
  Case    {..} -> getVal x >>= \case
    VCtor Object {..} _ ->
      evalBody $ foldr (\case
                          Default b -> const b
                          Ctor CtorInfo {..} b -> if cidx == tag then const b else id)
                 ((\Default {..} -> b) $ last cs)
                 cs
    Unsigned tag ->
      evalBody $ foldr (\case
                          Default b -> const b
                          Ctor CtorInfo {..} b -> if cidx == tag then const b else id)
                 ((\Default {..} -> b) $ last cs)
                 cs
    _ -> undefined
  Ret Var {..} -> getVal i
  Ret Irr      -> error "Cannot return Irr"
  Jmp     {..} -> getJoinPoint j >>= \(ps,b) -> zipWithM_ (\Param {x} -> \case
                                                             Var {i} -> getVal i >>= updateToState x
                                                             Irr -> undefined)
                                                  ps ys >> evalBody b
  Unreachable  -> pure Irre

evalExtern :: ExternAttrData -> [Val] -> Except String Val
evalExtern ExternAttrData {..} =
  fromMaybe (const $ throwError $ "todo: " ++ show entries)
  $ listToMaybe $ flip mapMaybe entries $ \case
    Standard {fn="lean_nat_dec_eq"} -> Just $ \case
        [Unsigned a, Unsigned b] ->
          pure $ Unsigned $ fromIntegral $ fromEnum $ a == b
        _ -> throwError "Bad dec eq"
    Standard {fn="lean_nat_add"} -> Just $ \case
        [Unsigned a, Unsigned b] -> pure $ Unsigned $ a + b
        _ -> throwError "Bad add"
    Standard {fn="lean_nat_mul"} -> Just $ \case
        [Unsigned a, Unsigned b] -> pure $ Unsigned $ a * b
        _ -> throwError "Bad mul"
    Standard {fn="lean_nat_sub"} -> Just $ \case
        [Unsigned a, Unsigned b] -> pure $ Unsigned $ if a < b then 0 else a - b
        _ -> throwError "Bad sub"
    _ -> Nothing

getArgs :: [Argument] -> Exec [Val]
getArgs = mapM $ \case
  Var i -> getVal i
  Irr -> error "Trying to getVal from Irr"

preserve :: MonadState s m => m a -> m a
preserve act = get >>= \old -> act <* put old

setArgs :: MonadState St m => [Param] -> [Val] -> m ()
setArgs ps args =
  put $ flip St mempty $ Map.fromList $
  zipWith (\Param {x} -> (x,)) ps args


evalExpr :: Expr -> Exec Val
evalExpr = \case
  ECtor {info = CtorInfo {..}, ys} -> getArgs ys <&> VCtor (Object 1 cidx)
  Reset {} -> undefined
  Reuse {} -> undefined
  Proj  {..} -> getVal x <&> (\VCtor {..} -> vs !! i)
  Uproj {..} -> getVal x <&> (\VCtor {..} -> vs !! i)
  Sproj {..} -> getVal x <&> (\VCtor {..} -> vs !! (n+offset)) -- TODO: Is this right?

  Fap {..} -> asks (Map.lookup c) >>= \case
    Just FDecl {xs=ps,bod=b} -> preserve $ do
      setArgs ps =<< getArgs ys
      evalBody b
    Just Extern {xs=ps,ext=b@ExternAttrData {entries=[Standard {fn=n}]}} -> do
      as <- getArgs ys
      mapExceptT lift . evalExtern b =<< getArgs ys
    _ -> throwError $ show c

  Pap {..} -> asks (Map.lookup c) >>= \case
    Just FDecl {xs=ps,bod=b} -> Partial b ps <$> getArgs ys <*> gets vars
    _ -> undefined

  Ap {..} -> getVal x >>= \case
    Partial{..} -> do
      args0 <- getArgs ys
      let args = vs <> args0
      if length args == length ps then preserve $ do
        setArgs ps args
        evalBody b
      else
        pure $ Partial b (drop (length args0) ps) args ctx
    _ -> undefined

  Box   {..} -> getVal x <&> VBox
  Unbox {..} -> getVal x >>= \VBox {..} -> pure v
  Lit   {..} -> pure $ case v of
    S t -> Str t
    N n -> Unsigned n
  IsShared {..} -> getVal x <&> \case
    Obje (Object {..}) -> if rc > 1 then Unsigned 1 else Unsigned 0
    VCtor (Object {..}) _ -> if rc > 1 then Unsigned 1 else Unsigned 0
    _ -> Unsigned 0

getVal :: VarId -> Exec Val
getVal = (maybe (error "Couldn't find i") id <$>) . gets . (. vars) . Map.lookup

updateToState :: VarId -> Val -> Exec ()
updateToState i v = modify (\s@St{..} -> s { vars = Map.insert i v vars })

getJoinPoint :: JoinPointId -> Exec ([Param], FnBody)
getJoinPoint = (maybe (error "Couldn't find i") id <$>) . gets . (. jmps) . Map.lookup

addJoinPoint :: JoinPointId -> ([Param],FnBody) -> Exec ()
addJoinPoint i v = modify (\s@St{..} -> s { jmps = Map.insert i v jmps })

setAt :: Int -> a -> [a] -> [a]
setAt _ _   [] = undefined
setAt 0 a' (_:as) = a':as
setAt i a' (a:as) = a:(setAt (i-1) a' as)
