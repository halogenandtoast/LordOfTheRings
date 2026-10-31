module LordOfTheRings.Location.Cards.PalaceRemains (palaceRemains) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea, StagingArea))

newtype PalaceRemains = PalaceRemains LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

palaceRemains :: LocationCard PalaceRemains
palaceRemains = location PalaceRemains Cards.palaceRemains

-- | Immune to player card effects. Palace Remains gets +1 threat for each resource token
-- on it. The players cannot travel here unless there are at least 5 resource tokens on
-- Palace Remains.
instance HasModifiersFor PalaceRemains where
  getModifiersFor (PalaceRemains a) = do
    let n = countTokens Resource a.locationTokens
    modified a a $ ImmuneToPlayerCardEffects : [ThreatModifier n | n > 0] <> [CannotBeTraveledTo | n < 5]

-- | While Palace Remains is in the staging area, it gains "Forced: After a player takes
-- control of a location, place 1 resource token here." Read off the location as it goes
-- to his play area.
instance RunMessage PalaceRemains where
  runMessage msg (PalaceRemains a) = case msg of
    PlaceLocation lid (InPlayArea pid) | a.placement == StagingArea -> do
      already <- (== InPlayArea pid) . (.placement) <$> getLocation lid
      unless already $ placeResources a a 1
      PalaceRemains <$> runMessage msg a
    _ -> PalaceRemains <$> runMessage msg a
