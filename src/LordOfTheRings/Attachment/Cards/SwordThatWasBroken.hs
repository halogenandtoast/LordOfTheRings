module LordOfTheRings.Attachment.Cards.SwordThatWasBroken (swordThatWasBroken) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype SwordThatWasBroken = SwordThatWasBroken AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
swordThatWasBroken :: AttachmentCard SwordThatWasBroken
swordThatWasBroken = attachment SwordThatWasBroken Cards.swordThatWasBroken & attachTo (AttachToHero AnyHero)

-- | Attached hero gains a leadership resource icon. If attached hero is Aragorn, each
-- character you control gets +1 willpower.
instance HasModifiersFor SwordThatWasBroken where
  getModifiersFor (SwordThatWasBroken a) = withHost a \host -> do
    modified a host [GainResourceIcon Leadership]
    aragorn <- case host of
      HeroTarget hid -> selectAny (HeroWithId hid <> HeroWithTitle "Aragorn")
      _ -> pure False
    when aragorn do
      let you = PlayerWithId a.controller
      characters <- selectCharacters (HeroControlledBy you) (AllyControlledBy you)
      for_ characters \c -> modified a c [WillpowerModifier 1]
