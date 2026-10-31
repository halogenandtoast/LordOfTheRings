module LordOfTheRings.Attachment.Cards.Ballista (ballista, ballistaUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype Ballista = Ballista AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Modification boon.
ballista :: AttachmentCard Ballista
ballista = attachment Ballista Cards.ballista & attachTo (AttachToAlly shipObjective)

ballistaUpgraded :: AttachmentCard Ballista
ballistaUpgraded = attachment Ballista Cards.ballistaUpgraded & attachTo (AttachToAlly shipObjective)

-- | Attached Ship gets +1 attack (and, Upgraded, gains ranged).
instance HasModifiersFor Ballista where
  getModifiersFor (Ballista a) = withHost a \host -> modified a host $ AttackModifier 1 : [GainKeyword Ranged | isUpgradedSide a]

-- | Upgraded: Response: After attached Ship attacks, deal 1 damage to a non-unique enemy
-- and remove 1 resource from that enemy.
instance HasAbilities Ballista where
  getAbilities (Ballista a) =
    [ restricted a 1 (exists (not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards)) $ response $ AttackedWindow After host
    | isUpgradedSide a
    , AttachedTo host <- [a.placement]
    ]

instance RunMessage Ballista where
  runMessage msg s@(Ballista a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards)
      chooseOneM pid do
        questionLabeled "ballista.damage"
        causedBy a
        targets enemies \e -> do
          dealDamage a e 1
          removeTokens a e Resource 1
      pure s
    _ -> Ballista <$> removedAsItLeaves msg a
