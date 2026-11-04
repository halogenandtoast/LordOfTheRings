module LordOfTheRings.Attachment.Cards.TirelessThoroughbred (tirelessThoroughbred) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Trait (Trait (Warrior))

newtype TirelessThoroughbred = TirelessThoroughbred AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Warrior character. Restricted, on its card def.
tirelessThoroughbred :: AttachmentCard TirelessThoroughbred
tirelessThoroughbred =
  attachment TirelessThoroughbred Cards.tirelessThoroughbred
    & attachTo (AttachToCharacterMatching (HeroWithTrait Warrior) (AllyWithTrait Warrior))

-- | Attached character gains ranged and sentinel.
instance HasModifiersFor TirelessThoroughbred where
  getModifiersFor (TirelessThoroughbred a) = withHost a \host -> modified a host [GainKeyword Ranged, GainKeyword Sentinel]
