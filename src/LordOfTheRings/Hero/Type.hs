module LordOfTheRings.Hero.Type (module LordOfTheRings.Hero.Type) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Name
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

class
  ( Typeable a
  , Show a
  , ToJSON a
  , RunMessage a
  , Entity a
  , EntityId a ~ HeroId
  , EntityAttrs a ~ HeroAttrs
  )
  => IsHero a

data Hero = forall a. (IsHero a) => Hero a

instance Entity Hero where
  type EntityId Hero = HeroId
  type EntityAttrs Hero = HeroAttrs
  toId = toId . toAttrs
  toAttrs (Hero a) = toAttrs a
  overAttrs f (Hero a) = Hero $ overAttrs f a

instance Entity HeroAttrs where
  type EntityId HeroAttrs = HeroId
  type EntityAttrs HeroAttrs = HeroAttrs
  toId = (.id)
  toAttrs a = a
  overAttrs f a = f a


instance Targetable Hero where
  toTarget (Hero a) = toTarget (toAttrs a)

instance Sourceable Hero where
  toSource (Hero a) = toSource (toAttrs a)

instance Targetable HeroAttrs where
  toTarget a = HeroTarget a.id

instance Sourceable HeroAttrs where
  toSource a = HeroSource a.id

data HeroAttrs = HeroAttrs
  { id :: HeroId
  , card :: PlayerCard
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
  getField (Hero a) = (toAttrs a).name

instance HasField "name" HeroAttrs Name where
  getField a = cdName (toCardDef a.card)

instance HasField "threat" HeroAttrs Int where
  getField a = fromMaybe 0 $ cdThreatCost (toCardDef a.card)

instance HasField "player" Hero PlayerId where
  getField (Hero a) = (toAttrs a).player

instance HasField "id" Hero HeroId where
  getField (Hero a) = (toAttrs a).id

instance HasField "threat" Hero Int where
  getField (Hero a) = (toAttrs a).threat

instance HasField "sphere" Hero Sphere where
  getField (Hero a) = (toAttrs a).sphere

instance HasField "resources" Hero Int where
  getField (Hero a) = (toAttrs a).resources
