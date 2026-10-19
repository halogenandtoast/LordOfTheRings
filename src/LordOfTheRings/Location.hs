{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location (module LordOfTheRings.Location, module LordOfTheRings.Location.Type) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Location.Type
import LordOfTheRings.Message
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

instance RunMessage Location where
  runMessage msg (Location a) = Location <$> runMessage msg a

newtype OldForestRoad = OldForestRoad LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage OldForestRoad where
  runMessage msg (OldForestRoad a) = case msg of
    _ -> OldForestRoad <$> runMessage msg a

instance RunMessage LocationAttrs where
  runMessage msg attrs = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      pure $ attrs {progress = attrs.progress + n}
    _ -> pure attrs

lookupLocation :: CardCode -> LocationAttrs -> Location
lookupLocation cCode = fromMaybe (error "No such quest") $ Map.lookup cCode allData
 where
  allData =
    Map.fromList
      [ ("01099", Location . OldForestRoad)
      ]

newLocation :: (MonadRandom m) => EncounterCard -> m Location
newLocation card = do
  locationId <- getRandom
  let attrs = LocationAttrs {id = locationId, card, placement = Unplaced, progress = 0}
  pure $ lookupLocation (toCardCode card) attrs
