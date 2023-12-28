module LordOfTheRings.Ally.Type (module LordOfTheRings.Ally.Type) where

import Data.Aeson
import Data.Coerce
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Run

class (Run a, Show a, ToJSON a) => IsAlly a where
  heroAttrs :: a -> AllyAttrs
  default heroAttrs :: (Coercible a AllyAttrs) => a -> AllyAttrs
  heroAttrs = coerce

data Ally where
  Ally :: (IsAlly a) => a -> Ally

data AllyAttrs = AllyAttrs
  { id :: AllyId
  , card :: Card 'AllyKind
  , sphere :: Sphere
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a
