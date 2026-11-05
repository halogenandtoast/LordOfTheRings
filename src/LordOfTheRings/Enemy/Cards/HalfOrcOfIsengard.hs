module LordOfTheRings.Enemy.Cards.HalfOrcOfIsengard (halfOrcOfIsengard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Player.Types (discardableHand)

newtype HalfOrcOfIsengard = HalfOrcOfIsengard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

halfOrcOfIsengard :: EnemyCard HalfOrcOfIsengard
halfOrcOfIsengard = enemy HalfOrcOfIsengard Cards.halfOrcOfIsengard

instance HasAbilities HalfOrcOfIsengard where
  getAbilities (HalfOrcOfIsengard a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage HalfOrcOfIsengard where
  runMessage msg e@(HalfOrcOfIsengard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyAttacking eid pid) <- ws, eid == a.id]) \pid -> do
        hand <- discardableHand <$> getPlayer pid
        barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
        chooseOrRunOneM pid do
          questionLabeled "halfOrcOfIsengard.choose"
          causedBy a
          when (notNull hand && not barred) $ labeled "halfOrcOfIsengard.discard" $ discardRandomFromHand pid 1
          labeled "halfOrcOfIsengard.attack" $ enemyGetsAttack a a.id 3
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      discardRandomFromHand pid 1
      pure e
    _ -> HalfOrcOfIsengard <$> runMessage msg a
