module LordOfTheRings.Quest.Cards.UruksOfTheEye (uruksOfTheEye) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype UruksOfTheEye = UruksOfTheEye QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest (Mordor Orcs). Surge, as its card def says.
uruksOfTheEye :: QuestCard UruksOfTheEye
uruksOfTheEye = quest UruksOfTheEye Cards.uruksOfTheEye

-- | Each Orc enemy in the staging area gets -5 engagement cost. Each Orc enemy engaged
-- with a player gets +1 threat.
instance HasModifiersFor UruksOfTheEye where
  getModifiersFor (UruksOfTheEye a) = do
    staged <- select (EnemyWithTrait Orc <> EnemyInStagingArea)
    for_ staged \e -> modified a e [EngagementCostModifier (-5)]
    engaged <- select (EnemyWithTrait Orc <> EnemyEngagedWith AnyPlayer)
    for_ engaged \e -> modified a e [ThreatModifier 1]

instance RunMessage UruksOfTheEye where
  runMessage msg (UruksOfTheEye attrs) = UruksOfTheEye <$> runMessage msg attrs
