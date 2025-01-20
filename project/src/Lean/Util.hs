module Lean.Util where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (JSONPathElement (..), Parser)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Prettyprinter (Doc, line, nest)
import Prelude hiding (fail)
import Prelude qualified as P

infixr 6 </>
(</>) :: Doc a -> Doc a -> Doc a
a </> b = a <> line <> b

indnt :: Doc a -> Doc a
indnt d = nest 2 (line <> d)


-------------------------------------------------------------------------------
--- Generic failures ----------------------------------------------------------
-------------------------------------------------------------------------------

-- | Our representation of failures, may be made more precise in the future
type PisaErr = String

-- | Anything that can fail with our kind of errors
-- This enables our helper functions to work outside of the T monad
class Fail m where fail :: HasCallStack => PisaErr -> m a

-- | Inherit all MonadError instances
instance (MonadError PisaErr m) => Fail m where
  fail = throwError

class TransferFail m0 where catchTo :: (Applicative m, Fail m) => m0 a -> m a

instance TransferFail (Either PisaErr) where
  catchTo :: (HasCallStack, Applicative m, Fail m) => Either PisaErr a -> m a
  catchTo (Right a) = pure a
  catchTo (Left e) = fail e

todo :: (HasCallStack, Fail m) => String -> m a
todo n = fail $ "TODO: " ++ n

expectationFail :: (HasCallStack, Fail m) => String -> m a
expectationFail s = fail $ s ++ " expectation missmatch"

expectation :: (HasCallStack, Applicative m, Fail m) => String -> Bool -> m ()
expectation s b = unless b $ expectationFail s

-------------------------------------------------------------------------------
--- Deserialization helpers ---------------------------------------------------
-------------------------------------------------------------------------------

-- | A faux Alternative instance for functions to Alternatives
alt :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
alt f0 f1 a = f0 a <|> f1 a

-- | Parse a value whose constructor is identified by assigning it to a key
keyedVal :: String -> [(Key, Value -> Parser a)] -> Value -> Parser a
keyedVal n m = withObject n $ \o ->
  fromMaybe missing $ foldr (<|>) Nothing $ m <&> parseKey o
  where
    missing = P.fail $ "No matching key for " ++ n
    parseKey :: Object -> (Key, Value -> Parser a) -> Maybe (Parser a)
    parseKey o (k, f) = ((<?> Key k) . f) <$> KM.lookup k o

-- | Parse a value whose constructor is identified by assigning it to a key
keyedObject :: String -> [(Key, Object -> Parser a)] -> Value -> Parser a
keyedObject n m = keyedVal n $ m <&> \(k, f) -> (k, withObject (n ++ '.' : show k) f)

mk :: FromJSON a => (a -> b) -> Value -> Parser b
mk ctr v = ctr <$> parseJSON v

-- | @cSing Ctr key@ is an alias for @\o -> Ctr '<$>' o '.:' key@
cSing :: (FromJSON a) => (a -> x) -> Key -> Object -> Parser x
cSing ctr k0 o = ctr <$> o .: k0

-- | Like 'cSing' but with two keys
cDubl :: (FromJSON a, FromJSON b) =>
  (a -> b -> x) -> Key -> Key -> Object -> Parser x
cDubl ctr k0 k1 o = ctr <$> o .: k0 <*> o .: k1

-- | Like 'cSing' but with three keys
cTrip :: (FromJSON a, FromJSON b, FromJSON c) =>
  (a -> b -> c -> x) -> Key -> Key -> Key -> Object -> Parser x
cTrip ctr k0 k1 k2 o = ctr <$> o .: k0 <*> o .: k1 <*> o .: k2

-- | Like 'cSing' but with four keys
cQuad :: (FromJSON a, FromJSON b, FromJSON c, FromJSON d) =>
  (a -> b -> c -> d -> x) -> Key -> Key -> Key -> Key -> Object -> Parser x
cQuad ctr k0 k1 k2 k3 o = ctr <$> o .: k0 <*> o .: k1 <*> o .: k2 <*> o .: k3

-- | Like 'cSing' but with five keys
cQuin :: (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) =>
  (a -> b -> c -> d -> e -> x) -> Key -> Key -> Key -> Key -> Key -> Object -> Parser x
cQuin ctr k0 k1 k2 k3 k4 o = ctr <$> o .: k0 <*> o .: k1 <*> o .: k2 <*> o .: k3 <*> o .: k4

-- | Like 'cSing' but with six keys
cSext :: (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) =>
  (a -> b -> c -> d -> e -> f -> x) -> Key -> Key -> Key -> Key -> Key -> Key -> Object -> Parser x
cSext ctr k0 k1 k2 k3 k4 k5 o = ctr <$> o .: k0 <*> o .: k1 <*> o .: k2 <*> o .: k3 <*> o .: k4 <*> o .: k5

-- | Like 'cSing' but with seven keys
cSept :: (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) =>
  (a -> b -> c -> d -> e -> f -> g -> x) -> Key -> Key -> Key -> Key -> Key -> Key -> Key -> Object -> Parser x
cSept ctr k0 k1 k2 k3 k4 k5 k6 o = ctr <$> o .: k0 <*> o .: k1 <*> o .: k2 <*> o .: k3 <*> o .: k4 <*> o .: k5 <*> o .: k6

