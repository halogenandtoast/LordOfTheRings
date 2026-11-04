module LordOfTheRings.Enemy.Cards.ManOfHarad (manOfHarad) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ManOfHarad = ManOfHarad EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge. Archery 1.
manOfHarad :: EnemyCard ManOfHarad
manOfHarad = enemy ManOfHarad Cards.manOfHarad

-- | Forced: After Man of Harad engages you, discard 1 random card from your hand.
instance HasAbilities ManOfHarad where
  getAbilities (ManOfHarad a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Discard 1 random card from your hand.
instance RunMessage ManOfHarad where
  runMessage msg e@(ManOfHarad attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> discardRandomFromHand pid 1
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardRandomFromHand pid 1
      pure e
    _ -> ManOfHarad <$> runMessage msg attrs
