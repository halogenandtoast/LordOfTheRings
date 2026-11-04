module LordOfTheRings.Attachment.Cards.ArmorPlating (armorPlating, armorPlatingUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ArmorPlating = ArmorPlating AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Modification boon, attached to a ship-objective as the campaign card's setup says.
armorPlating :: AttachmentCard ArmorPlating
armorPlating = attachment ArmorPlating Cards.armorPlating & attachTo (AttachToAlly shipObjective)

armorPlatingUpgraded :: AttachmentCard ArmorPlating
armorPlatingUpgraded = attachment ArmorPlating Cards.armorPlatingUpgraded & attachTo (AttachToAlly shipObjective)

-- | Attached Ship gets +1 defense.
instance HasModifiersFor ArmorPlating where
  getModifiersFor (ArmorPlating a) = withHost a \host -> modified a host [DefenseModifier 1]

-- | Upgraded: Response: After attached Ship defends against an enemy, discard 1 shadow card
-- from another enemy.
instance HasAbilities ArmorPlating where
  getAbilities (ArmorPlating a) =
    [ restricted a 1 (exists EnemyWithFacedownShadow) $ response $ DefendedAttackWindow After (Just host)
    | isUpgradedSide a
    , AttachedTo host <- [a.placement]
    ]

instance RunMessage ArmorPlating where
  runMessage msg s@(ArmorPlating a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      let attackers = [eid | Window _ (DefendedAttack _ eid) <- windows]
      enemies <- traverse getEnemy =<< select EnemyWithFacedownShadow
      shadowed <- for [e | e <- enemies, toId e `notElem` attackers] \e -> (e,) <$> shadowsPlayerCardsCanAffect e
      chooseOneM pid do
        questionLabeled "armorPlating.discardShadow"
        causedBy a
        for_ shadowed \(e, shadows) ->
          targeting (toId e) $ for_ (take 1 shadows) (discardShadowCard (toId e))
      pure s
    _ -> ArmorPlating <$> removedAsItLeaves msg a
