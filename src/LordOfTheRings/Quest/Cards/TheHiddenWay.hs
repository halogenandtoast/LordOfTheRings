module LordOfTheRings.Quest.Cards.TheHiddenWay (theHiddenWay) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype TheHiddenWay = TheHiddenWay QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHiddenWay :: QuestCard TheHiddenWay
theHiddenWay = quest TheHiddenWay Cards.theHiddenWay

-- | Enemies do not make engagement checks and cannot be optionally engaged.
instance HasModifiersFor TheHiddenWay where
  getModifiersFor (TheHiddenWay a) = modified a GameTarget [NoEngagementChecks, CannotBeOptionallyEngagedBy AnyPlayer]

-- | When Revealed: The first player takes control of all Ranger objectives in the staging
-- area.
instance RunMessage TheHiddenWay where
  runMessage msg q@(TheHiddenWay attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheHiddenWay <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      rangers <- filter ((== StagingArea) . (.placement)) <$> objectivesWithTrait Ranger
      for_ rangers \o -> putObjectiveAllyIntoPlay o.id lead
      pure q
    _ -> TheHiddenWay <$> runMessage msg attrs
