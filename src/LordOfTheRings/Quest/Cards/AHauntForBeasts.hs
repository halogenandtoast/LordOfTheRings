module LordOfTheRings.Quest.Cards.AHauntForBeasts (aHauntForBeasts) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AHauntForBeasts = AHauntForBeasts QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
aHauntForBeasts :: QuestCard AHauntForBeasts
aHauntForBeasts = quest AHauntForBeasts Cards.aHauntForBeasts

-- | Each enemy cannot take more than 4 damage each round.
-- While there is at least one enemy in the staging area, this stage cannot be chosen as the
-- current quest.
instance HasModifiersFor AHauntForBeasts where
  getModifiersFor (AHauntForBeasts a) = do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [DamageLimitEachRound 4]
    whenM (selectAny EnemyInStagingArea) $ modifySelf a [CannotBeCurrentQuest]

instance RunMessage AHauntForBeasts where
  runMessage msg (AHauntForBeasts attrs) = AHauntForBeasts <$> runMessage msg attrs
