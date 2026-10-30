module LordOfTheRings.Attachment.Cards.PrinceOfDolAmroth (princeOfDolAmroth) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Outlands))

newtype PrinceOfDolAmroth = PrinceOfDolAmroth AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Prince Imrahil.
princeOfDolAmroth :: AttachmentCard PrinceOfDolAmroth
princeOfDolAmroth =
  attachment PrinceOfDolAmroth Cards.princeOfDolAmroth & attachTo (AttachToHero $ HeroWithTitle "Prince Imrahil")

-- | Prince Imrahil gains the Outlands trait. While you control Outlands allies that belong
-- to 4 different spheres, add 1 additional resource to Prince Imrahil's resource pool when
-- you collect resources during the resource phase (if Prince Imrahil is a hero).
instance HasModifiersFor PrinceOfDolAmroth where
  getModifiersFor (PrinceOfDolAmroth a) = withHost a \host -> do
    let outlands = AllyControlledBy (PlayerWithId a.controller) <> AllyWithTrait Outlands
    spheres <- filterM (selectAny . (outlands <>) . AllyWithSphere) [Leadership, Tactics, Spirit, Lore]
    modified a host $ GainTrait Outlands : [CollectsAdditionalResources 1 | length spheres >= 4]
