module LordOfTheRings.Ally.Cards.GaladhrimMinstrel (galadhrimMinstrel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (EventType))

newtype GaladhrimMinstrel = GaladhrimMinstrel AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

galadhrimMinstrel :: AllyCard GaladhrimMinstrel
galadhrimMinstrel = ally GaladhrimMinstrel Cards.galadhrimMinstrel

-- | Response: After Galadhrim Minstrel enters play, search the top five cards of your
-- deck for an event card and add it to your hand. Shuffle the other cards back into your
-- deck.
instance HasAbilities GaladhrimMinstrel where
  getAbilities (GaladhrimMinstrel a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage GaladhrimMinstrel where
  runMessage msg s@(GaladhrimMinstrel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 (CardWithType EventType) False
      pure s
    _ -> GaladhrimMinstrel <$> runMessage msg a
