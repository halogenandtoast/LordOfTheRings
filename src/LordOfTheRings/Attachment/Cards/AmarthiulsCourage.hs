module LordOfTheRings.Attachment.Cards.AmarthiulsCourage (amarthiulsCourage) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype AmarthiulsCourage = AmarthiulsCourage AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign. Setup: Attach to Amarthiúl, the hero or the
-- objective ally, which the scenario does; otherwise it is out of the game.
amarthiulsCourage :: AttachmentCard AmarthiulsCourage
amarthiulsCourage =
  attachment AmarthiulsCourage Cards.amarthiulsCourage
    & attachTo (AttachToCharacterMatching (HeroWithTitle "Amarthiúl") (AllyWithTitle "Amarthiúl"))

-- | Amarthiúl gets +1 willpower and +1 hit point.
instance HasModifiersFor AmarthiulsCourage where
  getModifiersFor (AmarthiulsCourage a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 1, HitPointModifier 1]
    _ -> pure ()

-- | Action: Ready Amarthiúl. (Limit once per game.)
instance HasAbilities AmarthiulsCourage where
  getAbilities (AmarthiulsCourage a) = [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

instance RunMessage AmarthiulsCourage where
  runMessage msg s@(AmarthiulsCourage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> AmarthiulsCourage <$> runMessage msg a
