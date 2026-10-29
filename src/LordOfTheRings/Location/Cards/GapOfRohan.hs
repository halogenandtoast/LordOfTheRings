module LordOfTheRings.Location.Cards.GapOfRohan (gapOfRohan) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Dunland))

newtype GapOfRohan = GapOfRohan LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gapOfRohan :: LocationCard GapOfRohan
gapOfRohan = location GapOfRohan Cards.gapOfRohan

-- | While Gap of Rohan is in the staging area, Dunland enemies get +1 attack.
instance HasModifiersFor GapOfRohan where
  getModifiersFor (GapOfRohan a) = when (a.placement == StagingArea) do
    enemies <- select (EnemyWithTrait Dunland)
    for_ enemies \e -> modified a e [AttackModifier 1]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, remove 1
-- time counter from the current quest.
instance RunMessage GapOfRohan where
  runMessage msg l@(GapOfRohan a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      removeTimeCountersFromCurrentQuest a 1
      pure l
    _ -> GapOfRohan <$> runMessage msg a
