module LordOfTheRings.Objective.Cards.TheDefenseOfHelmsDeep (theDefenseOfHelmsDeep) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (advanceToStageFive)

newtype TheDefenseOfHelmsDeep = TheDefenseOfHelmsDeep ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

theDefenseOfHelmsDeep :: ObjectiveCard TheDefenseOfHelmsDeep
theDefenseOfHelmsDeep = objective TheDefenseOfHelmsDeep Cards.theDefenseOfHelmsDeep

-- | Add 1 to the total threat in the staging area for each resource token here.
instance HasModifiersFor TheDefenseOfHelmsDeep where
  getModifiersFor (TheDefenseOfHelmsDeep a) =
    when (a.placement == StagingArea) $ modified a GameTarget [StagingThreatPerToken (toTarget a) Resource]

-- | Forced: At the end of the quest phase, place 1 resource token here.
instance HasAbilities TheDefenseOfHelmsDeep where
  getAbilities (TheDefenseOfHelmsDeep a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase]

-- | If there are 8 resource tokens here, add The Defense of Helm's Deep to the victory
-- display and advance to stage 5A: read as tokens are placed here.
instance RunMessage TheDefenseOfHelmsDeep where
  runMessage msg o@(TheDefenseOfHelmsDeep attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure o
    PlaceTokens _ (isTarget attrs -> True) Resource _ -> do
      attrs' <- runMessage msg attrs
      when (countTokens Resource attrs'.objectiveTokens >= 8) do
        addToVictoryDisplay attrs
        advanceToStageFive
      pure $ TheDefenseOfHelmsDeep attrs'
    _ -> TheDefenseOfHelmsDeep <$> runMessage msg attrs
