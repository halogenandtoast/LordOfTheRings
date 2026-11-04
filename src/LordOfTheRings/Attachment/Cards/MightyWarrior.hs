module LordOfTheRings.Attachment.Cards.MightyWarrior (mightyWarrior) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Warrior))

newtype MightyWarrior = MightyWarrior AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def.
mightyWarrior :: AttachmentCard MightyWarrior
mightyWarrior = attachment MightyWarrior Cards.mightyWarrior & attachTo (AttachToHero AnyHero)

-- | Attached hero gains the Warrior trait.
instance HasModifiersFor MightyWarrior where
  getModifiersFor (MightyWarrior a) = case a.placement of
    AttachedTo host -> modified a host [GainTrait Warrior]
    _ -> pure ()

-- | Response: After you play Mighty Warrior from your hand, draw a card.
instance HasAbilities MightyWarrior where
  getAbilities (MightyWarrior a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage MightyWarrior where
  runMessage msg s@(MightyWarrior a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> MightyWarrior <$> runMessage msg a
