{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location (module LordOfTheRings.Location, module LordOfTheRings.Location.Type) where

import Control.Monad.Random
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Location.Type
import LordOfTheRings.Message
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Run

instance Run Location where
  run msg (Location a) = Location <$> run msg a

newtype OldForestRoad = OldForestRoad LocationAttrs
  deriving newtype (Show, ToJSON)

instance IsLocation OldForestRoad
instance Run OldForestRoad where
  run msg (OldForestRoad a) = case msg of
    _ -> OldForestRoad <$> run msg a

instance Run LocationAttrs where
  run msg attrs = case msg of
    PlaceProgress (isTarget attrs -> True) n -> do
      pure $ attrs {progress = attrs.progress + n}
    _ -> pure attrs

lookupLocation :: CardCode -> LocationAttrs -> Location
lookupLocation cCode = fromMaybe (error "No such quest") $ Map.lookup cCode allData
 where
  allData =
    Map.fromList
      [ ("01099", Location . OldForestRoad)
      ]

newLocation :: (MonadRandom m) => EncounterCard 'LocationKind -> m Location
newLocation card = do
  locationId <- getRandom
  let attrs = LocationAttrs {id = locationId, card, placement = Unplaced, progress = 0}
  pure $ lookupLocation card.def.code attrs
