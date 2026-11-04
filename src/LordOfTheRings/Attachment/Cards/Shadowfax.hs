module LordOfTheRings.Attachment.Cards.Shadowfax (shadowfax) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))

newtype Shadowfax = Shadowfax AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Gandalf.
shadowfax :: AttachmentCard Shadowfax
shadowfax = attachment Shadowfax Cards.shadowfax & attachTo (AttachToCharacterMatching (HeroWithTitle "Gandalf") (AllyWithTitle "Gandalf"))

-- | Gandalf gains ranged and sentinel.
instance HasModifiersFor Shadowfax where
  getModifiersFor (Shadowfax a) = withHost a \host -> modified a host [GainKeyword Ranged, GainKeyword Sentinel]

-- | Action: Exhaust Shadowfax to ready Gandalf.
instance HasAbilities Shadowfax where
  getAbilities (Shadowfax a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage Shadowfax where
  runMessage msg s@(Shadowfax a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> Shadowfax <$> runMessage msg a
