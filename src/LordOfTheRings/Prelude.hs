module LordOfTheRings.Prelude (
  module X,
  module LordOfTheRings.Prelude,
) where

import ClassyPrelude as X hiding (foldlM, on, orElse, (\\))
import Data.Type.Equality as X (type (~))

import Control.Lens as X (
  Lens',
  Traversal',
  at,
  ix,
  lens,
  preview,
  to,
  traverseOf,
  traverseOf_,
  view,
  views,
  (%~),
  (&),
  (+~),
  (-~),
  (.~),
  (<>~),
  (?~),
  (^.),
  (^..),
  (^?),
 )
import Control.Lens.TH as X
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans.Class as X (MonadTrans (..))
import Control.Monad.Extra as X (
  allM,
  andM,
  anyM,
  concatForM,
  concatMapM,
  fromMaybeM,
  mapMaybeM,
  orM,
  partitionM,
 )
import Control.Monad.Random as X (MonadRandom, uniform)
import Control.Monad.Random.Class as X (getRandom, getRandomR, getRandoms)
import Control.Monad.Random.Strict as X (Random)
import Control.Monad.Trans.Maybe as X (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Aeson as X hiding (Result (..))
import Data.Coerce as X (Coercible, coerce)
import Data.Kind as X (Type)
import Data.List as X (findIndex, nub, (\\))
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.UUID as X (UUID)
import GHC.Records as X (HasField (..))
import GHC.Stack as X
import Safe as X (atMay, fromJustNote)
import System.Random.Shuffle as X (shuffleM)

import Data.Char qualified as C
import Language.Haskell.TH (mkName, nameBase)

class Not a where
  not_ :: a -> a

suffixedNamer :: FieldNamer
suffixedNamer _ _ n = case dropWhile C.isLower (nameBase n) of
  x : xs -> [TopName (mkName ((C.toLower x : xs) ++ "L"))]
  _ -> []

suffixedFields :: LensRules
suffixedFields = defaultFieldRules & lensField .~ suffixedNamer

guardM :: (Alternative m, Monad m) => m Bool -> m ()
guardM p = p >>= guard

notNull :: (MonoFoldable mono) => mono -> Bool
notNull = not . null

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

unlessNull :: (Applicative m) => [a] -> ([a] -> m ()) -> m ()
unlessNull [] _ = pure ()
unlessNull xs f = f xs

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mm f = mm >>= traverse_ f

forMaybeM :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

data With a b = With a b
  deriving stock (Show, Eq, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (a `With` b) where
  toJSON (a `With` b) = case (toJSON a, toJSON b) of
    (Object o, Object m) -> Object (o <> m)
    (a', _) -> a'
