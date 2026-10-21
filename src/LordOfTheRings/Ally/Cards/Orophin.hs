module LordOfTheRings.Ally.Cards.Orophin (orophin) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Silvan))

newtype Orophin = Orophin AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orophin :: AllyCard Orophin
orophin = ally Orophin Cards.orophin

-- | Ranged. Response: After Orophin enters play, return a Silvan ally from your discard
-- pile to your hand.
instance HasAbilities Orophin where
  getAbilities (Orophin a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage Orophin where
  runMessage msg s@(Orophin a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      silvan <- filter (cardMatches (CardWithType AllyType <> CardWithTrait Silvan)) . (.discard) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "returnFromDiscard"
        causedBy a
        targets silvan \c -> returnFromDiscardToHand pid c.id
      pure s
    _ -> Orophin <$> runMessage msg a
