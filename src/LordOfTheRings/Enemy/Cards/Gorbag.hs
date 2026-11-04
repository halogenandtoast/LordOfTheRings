module LordOfTheRings.Enemy.Cards.Gorbag (gorbag) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Gorbag = Gorbag EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

gorbag :: EnemyCard Gorbag
gorbag = enemy Gorbag Cards.gorbag

-- | Cannot have attachments.
instance HasModifiersFor Gorbag where
  getModifiersFor (Gorbag a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Gorbag engages you, it makes an immediate attack against you.
instance HasAbilities Gorbag where
  getAbilities (Gorbag a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage Gorbag where
  runMessage msg e@(Gorbag attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> Gorbag <$> runMessage msg attrs
