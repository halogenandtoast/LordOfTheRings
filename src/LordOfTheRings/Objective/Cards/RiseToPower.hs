module LordOfTheRings.Objective.Cards.RiseToPower (riseToPower) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype RiseToPower = RiseToPower ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

riseToPower :: ObjectiveCard RiseToPower
riseToPower = objective RiseToPower Cards.riseToPower

-- | Rise to Power contributes X threat to the staging area during the quest phase, where X
-- is the number of resources on it.
instance HasModifiersFor RiseToPower where
  getModifiersFor (RiseToPower a) = do
    questing <- getsGame ((== QuestPhase) . (.phase))
    let x = countTokens Resource a.objectiveTokens
    when (questing && a.placement == StagingArea && x > 0) $ modified a GameTarget [StagingThreatBonus x]

-- | Forced: At the end of the round, place 2 resources on Rise to Power.
instance HasAbilities RiseToPower where
  getAbilities (RiseToPower a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage RiseToPower where
  runMessage msg o@(RiseToPower attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 2
      pure o
    _ -> RiseToPower <$> runMessage msg attrs
