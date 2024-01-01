module LordOfTheRings.Hero.Type (module LordOfTheRings.Hero.Type) where

import Data.Aeson
import Data.Coerce
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Name
import LordOfTheRings.Prelude
import LordOfTheRings.Run

class (Run a, Show a, ToJSON a) => IsHero a where
  heroAttrs :: a -> HeroAttrs
  default heroAttrs :: (Coercible a HeroAttrs) => a -> HeroAttrs
  heroAttrs = coerce

data Hero where
  Hero :: (IsHero a) => a -> Hero

instance Reference Hero where
  toRef (Hero a) = toRef (heroAttrs a)

instance Reference HeroAttrs where
  toRef a = HeroRef a.id

data HeroAttrs = HeroAttrs
  { id :: HeroId
  , card :: PlayerCard 'HeroKind
  , resources :: Int
  , sphere :: Sphere
  , player :: PlayerId
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Hero where
  show (Hero a) = show a

instance ToJSON Hero where
  toJSON (Hero a) = toJSON a

instance HasField "name" Hero Name where
  getField (Hero a) = (heroAttrs a).name

instance HasField "name" HeroAttrs Name where
  getField a = a.card.def.name

instance HasField "threat" HeroAttrs Int where
  getField a = a.card.def.details.threat

instance HasField "player" Hero PlayerId where
  getField (Hero a) = (heroAttrs a).player

instance HasField "id" Hero HeroId where
  getField (Hero a) = (heroAttrs a).id

instance HasField "threat" Hero Int where
  getField (Hero a) = (heroAttrs a).threat

instance HasField "sphere" Hero Sphere where
  getField (Hero a) = (heroAttrs a).sphere

instance HasField "resources" Hero Int where
  getField (Hero a) = (heroAttrs a).resources
