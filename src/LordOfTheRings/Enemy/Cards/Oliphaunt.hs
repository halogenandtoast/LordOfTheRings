module LordOfTheRings.Enemy.Cards.Oliphaunt (oliphaunt) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Oliphaunt = Oliphaunt EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

oliphaunt :: EnemyCard Oliphaunt
oliphaunt = enemy Oliphaunt Cards.oliphaunt

-- | Cannot have attachments or take more than 3 damage each round.
instance HasModifiersFor Oliphaunt where
  getModifiersFor (Oliphaunt a) = modifySelf a [CannotHaveAttachments, DamageLimit 3]

-- | Forced: After Oliphaunt engages you, exhaust each ally you control.
instance HasAbilities Oliphaunt where
  getAbilities (Oliphaunt a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +3 attack.
instance RunMessage Oliphaunt where
  runMessage msg e@(Oliphaunt attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        traverse_ exhaust =<< select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 3
      pure e
    _ -> Oliphaunt <$> runMessage msg attrs
