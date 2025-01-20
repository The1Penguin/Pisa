{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Aeson (pairs, (.=))
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.Map.Lazy as Map
import Pisa
import Lean.AST
import Control.Exception (catch, SomeException, throwIO)
import GHC.Stack (prettyCallStack)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import Data.Map.Lazy (Map)

translation :: [(Name,Name)] -> Map Name Out -> Int ->  IO ()
translation roots defs size =
        case translate roots defs of
          Left (e, stack, c) -> do
            err <- (const Nothing <$> printTranslation stderr False c)
              `catch` \(e' :: SomeException) -> return $ Just e'
            hPutStrLn stderr "======================================"
            -- printGlobals defs
            hPutStrLn stderr e
            hPutStrLn stderr $ prettyCallStack stack
            case err of
              Nothing -> exitFailure
              Just e' -> throwIO e'
          Right c -> do
            -- printTranslation stderr True c
            -- hPutStrLn stderr "======================================"
            hPutStrLn stderr =<< translationString c
            explore c size (renamings (Map.fromList roots) c)
              `catch` \(e :: SomeException) -> do
                BS.putStr . encodingToLazyByteString $ pairs  ("success" .= False <> "data" .= show e)
                throwIO e

main :: IO ()
main = do
  raw <- BS.getContents
  case parseAstEnv raw of
    Left err -> do
      hPutStrLn stderr err
      BS.putStrLn raw
    Right (roots, defs) -> getArgs >>= \case
      ["syntax"] -> printGlobals $ removeDecl defs
      ["eval"] -> do
        print defs
        pure () -- TODO evaluate defs (head roots)
      ["--size", k] ->
        translation roots defs (read k)
      [] ->
        translation roots defs 7
      _ -> pure ()

removeDecl :: Map Name Out -> Map Name Def
removeDecl = Map.map (\case
                        Decl _      -> error ""
                        DeclT _ _ _ -> error ""
                        ConInfo n   -> n) . Map.filter (\case
                          Decl _      -> False
                          DeclT _ _ _ -> False
                          ConInfo _   -> True)
