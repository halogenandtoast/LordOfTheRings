{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Location.Type (module LordOfTheRings.Location.Type) where

import Control.Lens
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

class
  ( Typeable a
  , Show a
  , ToJSON a
  , RunMessage a
  , Entity a
  , EntityId a ~ LocationId
  , EntityAttrs a ~ LocationAttrs
  )
  => IsLocation a

data Location = forall a. (IsLocation a) => Location a

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs
  toId = toId . toAttrs
  toAttrs (Location a) = toAttrs a
  overAttrs f (Location a) = Location $ overAttrs f a

instance Entity LocationAttrs where
  type EntityId LocationAttrs = LocationId
  type EntityAttrs LocationAttrs = LocationAttrs
  toId = (.id)
  toAttrs a = a
  overAttrs f a = f a

instance Targetable Location where
  toTarget (Location a) = toTarget (toAttrs a)

instance Sourceable Location where
  toSource (Location a) = toSource (toAttrs a)

instance Targetable LocationAttrs where
  toTarget a = LocationTarget a.id

instance Sourceable LocationAttrs where
  toSource a = LocationSource a.id

instance HasField "id" Location LocationId where
  getField (Location a) = (toAttrs a).id

instance HasPlacement Location where
  placementL =
    lens
      (\(Location a) -> (toAttrs a) ^. placementL)
      (\(Location a) p -> Location $ overAttrs (placementL .~ p) a)

instance HasPlacement LocationAttrs where
  placementL = lens (.placement) (\x y -> x {placement = y})

data Side = A | B
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data LocationAttrs = LocationAttrs
  { id :: LocationId
  , card :: EncounterCard 'LocationKind
  , placement :: Placement
  , progress :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Location where
  show (Location a) = show a

instance ToJSON Location where
  toJSON (Location a) = toJSON a
