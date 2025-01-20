{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Pisa where

import Data.Aeson (FromJSON, JSONPath, pairs, (.=))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Language.Haskell.Interpreter hiding (name)
import Language.Haskell.Interpreter.Unsafe
import Lean.AST (Out(..), Def(..), Name)
import Lean.IR (Decl(..))
import Lean.Translation (leanAsHaskell, TranslationResult)
import QuickSpec (Sig, withMaxTermSize, defaultTemplates)
import QuickSpec.Internal (addBackground, makeConfig, quickSpecResult)
import QuickSpec.Internal.RoughSpec qualified as RoughSpec
import System.FilePath ((</>), (<.>), takeBaseName)
import System.IO (stdout, hPutStrLn, stderr, Handle, hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Environment (lookupEnv)

import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.ByteString.Lazy (lbsToTokens)
import Data.Aeson.Types (ifromJSON, IResult (ISuccess, IError), formatError, JSONPathElement (Index))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isSpace)
import Data.Map (Map, empty, insert, toList)
import Lean.Haskell (HCode, filterComments, renameCode)
import Text.Pretty.Simple (pPrint)
import Control.Monad (forM_, when)
import qualified Data.Text.IO as TIO
import Prettyprinter (pretty, group, LayoutOptions (..), layoutPretty, unAnnotate, PageWidth (..), Pretty, Doc, defaultLayoutOptions)
import Control.Exception (SomeException, catch, bracket)
import Control.Monad.Writer
import Control.Monad.Reader
import Ormolu (ormolu, defaultConfig)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter.Render.Text (renderIO, renderStrict)
import Lean.Prop
import QuickSpec.Internal.Haskell (Constant)
import QuickSpec.Internal.Term (Term)
import QuickSpec.Internal.Prop (Prop)
import GHC.IO.Handle (hDuplicateTo, hDuplicate)


layoutOptions :: LayoutOptions
layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine maxWidth ribbon }
  where
    maxLen = 70 -- ^ How many characters (excluding indent) the text may be
    maxWidth = 120 -- ^ Width to not exceed
    ribbon = maxLen / (fromIntegral maxWidth)

mkPretty :: Pretty p => p -> Text
mkPretty p = renderStrict $ layoutPretty layoutOptions $ unAnnotate $ pretty p
putPretty :: Pretty p => (Doc a -> Doc a) -> p ->  IO ()
putPretty f p = do
  renderIO stdout $ layoutPretty layoutOptions $ unAnnotate $ f $ pretty p
  putStr "\n"

printGlobals :: Map Name Def -> IO ()
printGlobals g = forM_ (toList g) $ \(k, d) -> do
  prettyDef d `catch` \(e :: SomeException) -> do
    hPutStrLn stderr $ show e
    TIO.putStrLn k
    pPrint d
  putStr "\n"
  where
    prettyDef = putPretty group

printTranslation :: Handle -> Bool -> HCode -> IO ()
printTranslation hdl parse c = do
  putPretty id c
  when parse $ do
    putStrLn "----"
    TIO.hPutStrLn hdl =<< ormolu defaultConfig "" (mkPretty $ filterComments c)

translationString :: HCode -> IO String
translationString c = ormolu defaultConfig "" (mkPretty $ filterComments c) <&> T.unpack

parseAstEnv :: ByteString -> Either String ([(Name,Name)], Map Name Out)
parseAstEnv bs = do
  (roots :: [(Name,Name)], docs) <- parsePrefix [Index (-1)] bs
  scope <- parseMany (\m -> \case
                              v@(Decl      (FDecl  {f})) -> insert f v m
                              v@(Decl      (Extern {f})) -> insert f v m
                              v@(DeclT _ _ (FDecl  {f})) -> insert f v m
                              v@(DeclT _ _ (Extern {f})) -> insert f v m
                              v@(ConInfo   (Def {name})) -> insert name v m)
           empty docs
  pure (roots, scope)

-- | Read a ByteString until a parse of a is complete, return a and suffix or error
parsePrefix :: (FromJSON a) => JSONPath -> ByteString -> Either String (a, ByteString)
parsePrefix path bs = case toEitherValue $ lbsToTokens bs of
    Right (v, rest) -> case ifromJSON v of
      ISuccess a -> Right $ (a, rest)
      IError p m -> bad (path ++ p) m
    Left m -> bad path m
  where bad pth msg = Left $ formatError pth msg

-- | Parse a stream of multiple JSON documents
parseMany :: (FromJSON a) => (b -> a -> b) -> b ->  ByteString -> Either String b
parseMany f = go 0
  where
    go i past bs = parsePrefix [Index i] bs >>= \(a, rest) ->
      if BS.all isSpace rest
        then Right (f past a)
        else go (i+1) (f past a) rest

translate :: [(Name,Name)] -> Map Name Out -> TranslationResult
translate roots defs = leanAsHaskell roots defs

renamings :: Map Name Name -> HCode -> Map Text Text
renamings m = snd . runWriter . flip runReaderT m . renameCode

interp :: FilePath -> IO [Sig]
interp f =
  let act = (set [languageExtensions := [DeriveGeneric, OverloadedStrings, LambdaCase, RecordWildCards, DuplicateRecordFields, UnknownExtension "DerivingVia"]] >>
                  loadModules [f] >>
                  setImportsF [ ModuleImport (takeBaseName f) NotQualified NoImportList
                              , ModuleImport "GHC.Generics" NotQualified (ImportList ["Generic"])
                              , ModuleImport "Lean.Eval" NotQualified NoImportList
                              , ModuleImport "Lean.IR" NotQualified NoImportList
                              , ModuleImport "Data.Map.Lazy" NotQualified (ImportList ["Map", "fromList"])
                              , ModuleImport "Control.DeepSeq" NotQualified (ImportList ["NFData"])
                              , ModuleImport "Test.QuickCheck" NotQualified NoImportList
                              , ModuleImport "Test.QuickCheck.Arbitrary.Generic" NotQualified NoImportList
                              , ModuleImport "QuickSpec" NotQualified NoImportList
                              ] >>
                  interpret "sigs" infer)
  in (liftIO (lookupEnv "GHC_LIB_DIR") >>= \case
        Just dir -> putStrLn dir >> unsafeRunInterpreterWithArgsLibdir [] dir act
        Nothing -> runInterpreter act
     ) >>= \case
              Left s -> error $ show s
              Right s -> pure s

runQSRS :: [Sig] -> Int -> IO [Prop (Term Constant)]
runQSRS sigs size =
  bracket
  (hDuplicate stdout <* hDuplicateTo stderr stdout)
  (\og -> hDuplicateTo og stdout >> hClose og)
  $ const $
    quickSpecResult (withMaxTermSize size:sigs) >>=
    (`RoughSpec.roughSpec` Nothing) .
    makeConfig .
    ((defaultTemplates:sigs) <>) .
    pure . addBackground .
    reverse -- It seems RoughSpec flips the order of generated conjectures

printPropsAsJson ::  Map Text Name -> [Prop (Term Constant)] -> IO ()
printPropsAsJson backmap =
  BS.putStr .
  encodingToLazyByteString .
  pairs .
  ("success" .= True <>) . ("data" .=) .
  map (renderStrict . layoutPretty defaultLayoutOptions . prop2Lean backmap)

explore :: HCode -> Int -> Map Text Name -> IO ()
explore code size backmap =
  withSystemTempDirectory "pisa" $ \dir ->
    let file = dir </> "A" <.> "hs"
        doc = T.unpack . mkPretty $ filterComments code in
    writeFile file doc >>
    interp file >>=
    (`runQSRS` size) >>=
    printPropsAsJson backmap

