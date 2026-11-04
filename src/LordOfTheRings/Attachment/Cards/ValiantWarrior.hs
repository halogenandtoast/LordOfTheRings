module LordOfTheRings.Attachment.Cards.ValiantWarrior (valiantWarrior) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Warrior))

newtype ValiantWarrior = ValiantWarrior AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Black Riders. Permanent, on its card def. Setup: Attach to a hero: as the
-- Campaign Pool records it on its hero.
valiantWarrior :: AttachmentCard ValiantWarrior
valiantWarrior = attachment ValiantWarrior Cards.valiantWarrior & attachTo (AttachToHero AnyHero)

-- | Attached hero gets +1 [attack] and gains the Warrior trait.
instance HasModifiersFor ValiantWarrior where
  getModifiersFor (ValiantWarrior a) = withHost a \host -> modified a host [AttackModifier 1, GainTrait Warrior]
