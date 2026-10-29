module LordOfTheRings.Location.Cards.CollapsedTower (collapsedTower) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea, Unplaced))
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (isScour)

newtype CollapsedTower = CollapsedTower LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

collapsedTower :: LocationCard CollapsedTower
collapsedTower = location CollapsedTower Cards.collapsedTower

-- | Surge.
-- Scour: Return this location to the staging area and place 2 damage here. Triggered from
-- the encounter discard pile, it comes back from there to the staging area.
-- Shadow: Excess damage from this attack must be placed on the active location, if able.
instance RunMessage CollapsedTower where
  runMessage msg l@(CollapsedTower a) = case msg of
    _ | isScour a msg -> do
      if a.placement == Unplaced
        then do
          addToStagingFromDiscard $ MkEncounterCard a.cardId (toCardCode a)
          push $ ScenarioSpecific "collapsedTower.returned" (toJSON a.cardId)
        else do
          placeLocation a.id StagingArea
          placeDamage a a 2
      pure l
    ScenarioSpecific "collapsedTower.returned" (parseMaybe parseJSON -> Just cid) | cid == a.cardId -> do
      placeDamage a a 2
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      attackModifier (CardIdSource cid) eid ExcessDamageToActiveLocation
      pure l
    _ -> CollapsedTower <$> runMessage msg a
