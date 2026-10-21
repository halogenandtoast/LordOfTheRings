module LordOfTheRings.Attachment.Cards.ValiantSword (valiantSword) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Trait (Trait (Noble))

newtype ValiantSword = ValiantSword AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noble hero. Limit 1 per hero. Restricted.
valiantSword :: AttachmentCard ValiantSword
valiantSword =
  attachment ValiantSword Cards.valiantSword
    & attachTo (AttachToHero $ HeroWithTrait Noble <> not_ (HeroWithAttachment $ toCardCode Cards.valiantSword))

-- | Attached hero gets +1 attack (+2 attack if your threat is 40 or higher).
instance HasModifiersFor ValiantSword where
  getModifiersFor (ValiantSword a) = case a.placement of
    AttachedTo host -> do
      threat <- (.threat) <$> getPlayer a.owner
      modified a host [AttackModifier $ if threat >= 40 then 2 else 1]
    _ -> pure ()
