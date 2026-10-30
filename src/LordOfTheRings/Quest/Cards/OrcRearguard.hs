module LordOfTheRings.Quest.Cards.OrcRearguard (orcRearguard) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype OrcRearguard = OrcRearguard QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
orcRearguard :: QuestCard OrcRearguard
orcRearguard = quest OrcRearguard Cards.orcRearguard

-- | No more than 3 progress tokens can be placed on the current quest each round.
instance HasModifiersFor OrcRearguard where
  getModifiersFor (OrcRearguard a) = traverse_ (\q -> modified a q [ProgressPerRoundLimit 3]) =<< getCurrentQuest

-- | While Orc Rearguard is the current quest it gains: "Forced: At the end of the staging
-- step, reveal 1 card from the encounter deck."
instance HasAbilities OrcRearguard where
  getAbilities (OrcRearguard a) = [restricted a 1 (TargetHasModifier (toTarget a) CurrentQuest) $ forced StagingStepEndsWindow]

instance RunMessage OrcRearguard where
  runMessage msg q@(OrcRearguard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure q
    _ -> OrcRearguard <$> runMessage msg attrs
