module LordOfTheRings.Attachment.Cards.DwarrowdelfAxe (dwarrowdelfAxe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype DwarrowdelfAxe = DwarrowdelfAxe AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf character. Restricted.
dwarrowdelfAxe :: AttachmentCard DwarrowdelfAxe
dwarrowdelfAxe =
  attachment DwarrowdelfAxe Cards.dwarrowdelfAxe
    & attachTo (AttachToCharacterMatching (HeroWithTrait Dwarf) (AllyWithTrait Dwarf))

-- | Attached character gets +1 attack.
instance HasModifiersFor DwarrowdelfAxe where
  getModifiersFor (DwarrowdelfAxe a) = withHost a \host -> modified a host [AttackModifier 1]

-- | Response: After attached character attacks, deal 1 damage to the defending enemy.
instance HasAbilities DwarrowdelfAxe where
  getAbilities (DwarrowdelfAxe a) = [mkAbility a 1 $ response $ attacked host | AttachedTo host <- [a.placement]]

instance RunMessage DwarrowdelfAxe where
  runMessage msg s@(DwarrowdelfAxe a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      enemies <- filterM (selectAny . EnemyWithId) [eid | Window _ (AttackedEnemy _ eid) <- windows]
      for_ enemies \eid -> dealDamage a eid 1
      pure s
    _ -> DwarrowdelfAxe <$> runMessage msg a
