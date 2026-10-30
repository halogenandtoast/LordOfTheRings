module LordOfTheRings.Attachment.Cards.StarBrooch (starBrooch) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain, Noldor))

newtype StarBrooch = StarBrooch AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dúnedain or Noldor hero. Limit 1 per hero, on its card def.
starBrooch :: AttachmentCard StarBrooch
starBrooch =
  attachment StarBrooch Cards.starBrooch
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Dunedain, HeroWithTrait Noldor])

-- | While you are engaged with an enemy, attached hero gets +1 willpower and cannot have
-- its willpower reduced.
instance HasModifiersFor StarBrooch where
  getModifiersFor (StarBrooch a) = withHost a \host -> do
    engaged <- selectAny (EnemyEngagedWith $ PlayerWithId a.controller)
    when engaged $ modified a host [WillpowerModifier 1, CannotHaveWillpowerReduced]
