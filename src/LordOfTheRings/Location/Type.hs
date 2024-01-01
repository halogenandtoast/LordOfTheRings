{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Location.Type (module LordOfTheRings.Location.Type) where

import Control.Lens
import Data.Aeson
import Data.Coerce
import GHC.Generics
import GHC.Records
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Run

class (Show a, ToJSON a, Run a) => IsLocation a where
  locationAttrs :: a -> LocationAttrs
  default locationAttrs :: (Coercible a LocationAttrs) => a -> LocationAttrs
  locationAttrs = coerce
  overLocationAttrs :: (LocationAttrs -> LocationAttrs) -> a -> a
  default overLocationAttrs
    :: (Coercible a LocationAttrs) => (LocationAttrs -> LocationAttrs) -> a -> a
  overLocationAttrs = coerce

data Location where
  Location :: (IsLocation a) => a -> Location

instance HasField "id" Location LocationId where
  getField (Location a) = (locationAttrs a).id

instance HasPlacement Location where
  placementL =
    lens
      (\(Location a) -> (locationAttrs a) ^. placementL)
      (\(Location a) p -> Location $ overLocationAttrs (placementL .~ p) a)

instance HasPlacement LocationAttrs where
  placementL = lens (.placement) (\x y -> x {placement = y})

data Side = A | B
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data LocationAttrs = LocationAttrs
  { id :: LocationId
  , card :: EncounterCard 'LocationKind
  , placement :: Placement
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Location where
  show (Location a) = show a

instance ToJSON Location where
  toJSON (Location a) = toJSON a
