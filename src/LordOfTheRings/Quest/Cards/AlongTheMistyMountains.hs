module LordOfTheRings.Quest.Cards.AlongTheMistyMountains (alongTheMistyMountains) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AlongTheMistyMountains = AlongTheMistyMountains QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

alongTheMistyMountains :: QuestCard AlongTheMistyMountains
alongTheMistyMountains = quest AlongTheMistyMountains Cards.alongTheMistyMountains

-- | Setup: Put Arwen Undómiel into play under the control of the first player. Shuffle
-- the encounter deck. Reveal 1 card from the encounter deck per player, and add them to
-- the staging area.
instance RunMessage AlongTheMistyMountains where
  runMessage msg q@(AlongTheMistyMountains attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Objectives.arwenUndomielRoadToRivendell)
      eachPlayer $ const revealEncounterCard
      AlongTheMistyMountains <$> runMessage msg attrs
    _ -> AlongTheMistyMountains <$> runMessage msg attrs
