module LordOfTheRings.Quest.Cards.TheMorgulRider (theMorgulRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (captainCannotLeavePlay, isCaptain)

newtype TheMorgulRider = TheMorgulRider QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMorgulRider :: QuestCard TheMorgulRider
theMorgulRider = quest TheMorgulRider Cards.theMorgulRider

-- | Nazgûl of Minas Morgul cannot leave play unless destroyed.
instance HasModifiersFor TheMorgulRider where
  getModifiersFor (TheMorgulRider a) = captainCannotLeavePlay a Enemies.nazgulOfMinasMorgul

-- | When Revealed: Add Nazgûl of Minas Morgul and The White Bridge to the staging area.
-- When Revealed: Reveal 1 card per player from the encounter deck and add it to the
-- staging area.
-- If Nazgûl of Minas Morgul is defeated, the players have won the game.
instance RunMessage TheMorgulRider where
  runMessage msg q@(TheMorgulRider attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheMorgulRider <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.nazgulOfMinasMorgul)
      addSetAsideToStaging (cardIs Locations.theWhiteBridge)
      eachPlayer $ const revealEncounterCard
      pure q
    Destroy (EnemyTarget eid) -> do
      whenM (isCaptain Enemies.nazgulOfMinasMorgul eid) winGame
      TheMorgulRider <$> runMessage msg attrs
    _ -> TheMorgulRider <$> runMessage msg attrs
