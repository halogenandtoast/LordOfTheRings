module LordOfTheRings.Ally.Type (module LordOfTheRings.Ally.Type) where

import Data.Aeson
import Data.Coerce
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Run

class (Run a, Show a, ToJSON a) => IsAlly a where
  allyAttrs :: a -> AllyAttrs
  default allyAttrs :: (Coercible a AllyAttrs) => a -> AllyAttrs
  allyAttrs = coerce
  overAllyAttrs :: (AllyAttrs -> AllyAttrs) -> a -> a
  default overAllyAttrs :: (Coercible a AllyAttrs) => (AllyAttrs -> AllyAttrs) -> a -> a
  overAllyAttrs f = coerce f

data Ally where
  Ally :: (IsAlly a) => a -> Ally

data AllyAttrs = AllyAttrs
  { id :: AllyId
  , card :: Card 'AllyKind
  , sphere :: Sphere
  , placement :: Placement
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a
