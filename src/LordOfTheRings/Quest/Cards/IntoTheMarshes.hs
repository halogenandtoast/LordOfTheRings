module LordOfTheRings.Quest.Cards.IntoTheMarshes (intoTheMarshes) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype IntoTheMarshes = IntoTheMarshes QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoTheMarshes :: QuestCard IntoTheMarshes
intoTheMarshes = quest IntoTheMarshes Cards.intoTheMarshes

-- | Setup: Search the encounter deck for Gollum, and add it to the staging area. Shuffle
-- the encounter deck, then reveal 1 card per player from the encounter deck and add it to
-- the staging area.
instance RunMessage IntoTheMarshes where
  runMessage msg q@(IntoTheMarshes attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    -- reset to this stage later, it only turns over: the setup is the game's first
    PerformSetup (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Objectives.gollumTheDeadMarshes)
      eachPlayer $ const revealEncounterCard
      turnOver attrs
      pure q
    _ -> IntoTheMarshes <$> runMessage msg attrs
