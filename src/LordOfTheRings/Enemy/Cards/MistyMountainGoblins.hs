module LordOfTheRings.Enemy.Cards.MistyMountainGoblins (mistyMountainGoblins) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MistyMountainGoblins = MistyMountainGoblins EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mistyMountainGoblins :: EnemyCard MistyMountainGoblins
mistyMountainGoblins = enemy MistyMountainGoblins Cards.mistyMountainGoblins

-- | Forced: After Misty Mountain Goblins attacks, remove 1 progress token from the current
-- quest.
instance HasAbilities MistyMountainGoblins where
  getAbilities (MistyMountainGoblins a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Shadow: Remove 1 progress token from the current quest. (3 progress tokens instead if
-- this attack is undefended.)
instance RunMessage MistyMountainGoblins where
  runMessage msg e@(MistyMountainGoblins attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeProgressFromQuest attrs 1
      pure e
    ResolveShadowEffect cid _ _ defender | cid == attrs.cardId -> do
      removeProgressFromQuest attrs (ifUndefended defender 3 1)
      pure e
    _ -> MistyMountainGoblins <$> runMessage msg attrs
