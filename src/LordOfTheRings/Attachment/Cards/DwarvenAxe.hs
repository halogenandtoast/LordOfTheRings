module LordOfTheRings.Attachment.Cards.DwarvenAxe (dwarvenAxe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Trait (Trait (Dwarf))

newtype DwarvenAxe = DwarvenAxe AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dwarvenAxe :: AttachmentCard DwarvenAxe
dwarvenAxe = attachment DwarvenAxe Cards.dwarvenAxe

-- | Attached hero gains +1 attack (+2 attack instead if attached hero is a Dwarf).
instance HasModifiersFor DwarvenAxe where
  getModifiersFor (DwarvenAxe a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      dwarf <- hasTrait Dwarf host . toCardDef =<< getHero hid
      modified a host [AttackModifier $ if dwarf then 2 else 1]
    _ -> pure ()
