module LordOfTheRings.Objective.Cards.TheCorsairFleetGondor (theCorsairFleetGondor) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype TheCorsairFleetGondor = TheCorsairFleetGondor ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Gondor side of The Corsair Fleet, in The Battle of the Pelennor Fields' staging
-- area in campaign mode with the resources The Siege of Gondor left on it.
theCorsairFleetGondor :: ObjectiveCard TheCorsairFleetGondor
theCorsairFleetGondor = objective TheCorsairFleetGondor Cards.theCorsairFleetGondor

-- | Forced: At the beginning of the quest phase, remove 1 resource from The Corsair Fleet.
-- Then, if there are no resources here, the first player takes control of Aragorn and adds
-- The Corsair Fleet to the victory display. A quest phase skipped, as stage 1B skips it,
-- has no beginning.
instance HasAbilities TheCorsairFleetGondor where
  getAbilities (TheCorsairFleetGondor a) =
    [restricted a 1 (TargetLacksModifier GameTarget SkipQuestPhase) $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TheCorsairFleetGondor where
  runMessage msg o@(TheCorsairFleetGondor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      when (attrs.resources > 0) $ removeTokens attrs attrs Resource 1
      doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      when (attrs.resources == 0) do
        push $ ScenarioSpecific "pelennorFields.aragornJoins" Null
        addToVictoryDisplay attrs
      pure o
    _ -> TheCorsairFleetGondor <$> runMessage msg attrs
