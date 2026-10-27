module LordOfTheRings.Treachery.Cards.SignsOfConflict (signsOfConflict) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SignsOfConflict = SignsOfConflict TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

signsOfConflict :: TreacheryCard SignsOfConflict
signsOfConflict = treachery SignsOfConflict Cards.signsOfConflict

-- | Doomed 2. Surge.
-- Shadow: Defending player raises his threat by 2.
instance RunMessage SignsOfConflict where
  runMessage msg t@(SignsOfConflict attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      encounterRaisesThreat pid 2
      pure t
    _ -> SignsOfConflict <$> runMessage msg attrs
