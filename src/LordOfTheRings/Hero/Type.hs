module LordOfTheRings.Hero.Type (module LordOfTheRings.Hero.Type) where

import Data.Aeson
import Data.Coerce
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Run

class (Run a, Show a, ToJSON a) => IsHero a where
  heroAttrs :: a -> HeroAttrs
  default heroAttrs :: (Coercible a HeroAttrs) => a -> HeroAttrs
  heroAttrs = coerce

data Hero where
  Hero :: (IsHero a) => a -> Hero

data HeroAttrs = HeroAttrs
  { id :: HeroId
  , card :: Card 'HeroKind
  , resources :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Hero where
  show (Hero a) = show a

instance ToJSON Hero where
  toJSON (Hero a) = toJSON a
