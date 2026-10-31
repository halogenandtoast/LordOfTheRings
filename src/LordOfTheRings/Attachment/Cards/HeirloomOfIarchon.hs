module LordOfTheRings.Attachment.Cards.HeirloomOfIarchon (heirloomOfIarchon) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype HeirloomOfIarchon = HeirloomOfIarchon AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from The Treachery of Rhudaur's Clue. Attach to
-- a hero. Restricted.
heirloomOfIarchon :: AttachmentCard HeirloomOfIarchon
heirloomOfIarchon = attachment HeirloomOfIarchon Cards.heirloomOfIarchon

-- | Attached hero gets +1 willpower.
instance HasModifiersFor HeirloomOfIarchon where
  getModifiersFor (HeirloomOfIarchon a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 1]
    _ -> pure ()

-- | Response: After you play Heirloom of Iârchon, heal all damage from attached character.
instance HasAbilities HeirloomOfIarchon where
  getAbilities (HeirloomOfIarchon a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage HeirloomOfIarchon where
  runMessage msg s@(HeirloomOfIarchon a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> healAllDamage a host
      pure s
    _ -> HeirloomOfIarchon <$> runMessage msg a
