module LordOfTheRings.Quest.Cards.ThroughTheForest (throughTheForest) where

import LordOfTheRings.Frontend
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGollum)

newtype ThroughTheForest = ThroughTheForest QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheForest :: QuestCard ThroughTheForest
throughTheForest = quest ThroughTheForest Cards.throughTheForest

-- | Setup: Search the encounter deck for Gollum. Choose a player to guard Gollum at the
-- start of the game, and place Gollum in front of that player. Then shuffle the encounter
-- deck. Reveal 1 card per player from the encounter deck, and add it to the staging area.
instance RunMessage ThroughTheForest where
  runMessage msg q@(ThroughTheForest attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Objectives.gollumReturnToMirkwood)
      doStep 1 msg
      pure $ ThroughTheForest $ attrs {questFlipped = True}
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      lead <- getLead
      getGollum >>= traverse_ \g -> choosePlayerM lead attrs (placeObjective g . InPlayArea)
      eachPlayer $ const revealEncounterCard
      pure q
    _ -> ThroughTheForest <$> runMessage msg attrs
