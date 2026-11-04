module LordOfTheRings.Enemy.Cards.MordorOlogHai (mordorOlogHai) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MordorOlogHai = MordorOlogHai EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

mordorOlogHai :: EnemyCard MordorOlogHai
mordorOlogHai = enemy MordorOlogHai Cards.mordorOlogHai

-- | Cannot have attachments.
instance HasModifiersFor MordorOlogHai where
  getModifiersFor (MordorOlogHai a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Mordor Olog-hai attacks and destroys a character you control, exhaust a
-- character you control.
instance HasAbilities MordorOlogHai where
  getAbilities (MordorOlogHai a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Shadow: Attacking enemy gets +2 Attack.
instance RunMessage MordorOlogHai where
  runMessage msg e@(MordorOlogHai attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      destroyed <- attackDestroyed attrs.id
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid ->
        when (any ((== Just pid) . (.controller)) destroyed) $ chooseAndExhaust attrs pid 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> MordorOlogHai <$> runMessage msg attrs
