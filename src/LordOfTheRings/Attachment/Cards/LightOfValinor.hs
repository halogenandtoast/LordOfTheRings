module LordOfTheRings.Attachment.Cards.LightOfValinor (lightOfValinor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype LightOfValinor = LightOfValinor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noldor or Silvan hero.
lightOfValinor :: AttachmentCard LightOfValinor
lightOfValinor =
  attachment LightOfValinor Cards.lightOfValinor
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan])

-- | Attached hero does not exhaust to commit to a quest.
instance HasModifiersFor LightOfValinor where
  getModifiersFor (LightOfValinor a) = withHost a \host -> modified a host [DoesNotExhaustToCommit]
