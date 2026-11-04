module LordOfTheRings.Ally.Cards.OdoProudfoot (odoProudfoot) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype OdoProudfoot = OdoProudfoot AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

odoProudfoot :: AllyCard OdoProudfoot
odoProudfoot = ally OdoProudfoot Cards.odoProudfoot

-- | Response: After Odo Proudfoot enters play, search the top 5 cards of your deck for a
-- Hobbit ally and add it to your hand. Shuffle the other cards back into your deck.
instance HasAbilities OdoProudfoot where
  getAbilities (OdoProudfoot a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage OdoProudfoot where
  runMessage msg s@(OdoProudfoot a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 (CardWithTrait Hobbit <> CardWithType AllyType) False
      pure s
    _ -> OdoProudfoot <$> runMessage msg a
