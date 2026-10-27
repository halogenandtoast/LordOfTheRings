module LordOfTheRings.Quest.Cards.TheDrippingWalls (theDrippingWalls) where

import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (attachCaveTorch)

newtype TheDrippingWalls = TheDrippingWalls QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDrippingWalls :: QuestCard TheDrippingWalls
theDrippingWalls = quest TheDrippingWalls Cards.theDrippingWalls

-- | Setup: Place the Foundations of Stone encounter set aside, out of play (the scenario
-- does, as it gathers the encounter deck). The first player attaches Cave Torch to a hero
-- of his choice.
-- When Revealed: Reveal 1 card from the encounter deck per player, and add it to the
-- staging area.
instance RunMessage TheDrippingWalls where
  runMessage msg q@(TheDrippingWalls attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      attachCaveTorch attrs
      eachPlayer $ const revealEncounterCard
      pure $ TheDrippingWalls $ attrs {questFlipped = True}
    _ -> TheDrippingWalls <$> runMessage msg attrs
