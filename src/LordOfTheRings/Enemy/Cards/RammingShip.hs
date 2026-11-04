module LordOfTheRings.Enemy.Cards.RammingShip (rammingShip) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RammingShip = RammingShip EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

rammingShip :: EnemyCard RammingShip
rammingShip = enemy RammingShip Cards.rammingShip

-- | Cannot have attachments.
instance HasModifiersFor RammingShip where
  getModifiersFor (RammingShip a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Ramming Ship engages you, it gets +4 attack until the end of the round.
instance HasAbilities RammingShip where
  getAbilities (RammingShip a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Put the top card of the Corsair deck into play engaged with you and deal it a
-- shadow card.
instance RunMessage RammingShip where
  runMessage msg e@(RammingShip attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifier attrs attrs.id (AttackModifier 4)
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      revealFromCorsairDeckWithShadow (CardIdSource cid) pid
      pure e
    _ -> RammingShip <$> runMessage msg attrs
