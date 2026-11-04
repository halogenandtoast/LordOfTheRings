module LordOfTheRings.Attachment.Cards.WarriorSword (warriorSword) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Trait (Trait (Warrior))

newtype WarriorSword = WarriorSword AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a [tactics] or Warrior character. Restricted. Limit 1 per character, on its
-- card def.
warriorSword :: AttachmentCard WarriorSword
warriorSword =
  attachment WarriorSword Cards.warriorSword
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithSphere Tactics, HeroWithTrait Warrior])
          (AllyMatchAny [AllyWithSphere Tactics, AllyWithTrait Warrior])
      )

-- | Attached character gets +1 [attack] for each enemy engaged with you. (Limit +3
-- [attack].) "You" is Warrior Sword's controller.
instance HasModifiersFor WarriorSword where
  getModifiersFor (WarriorSword a) = withHost a \host -> do
    n <- min 3 . length <$> select (EnemyEngagedWith $ PlayerWithId a.controller)
    when (n > 0) $ modified a host [AttackModifier n]
