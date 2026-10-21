module LordOfTheRings.Ally.Cards.GildorInglorion (gildorInglorion) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GildorInglorion = GildorInglorion AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gildorInglorion :: AllyCard GildorInglorion
gildorInglorion = ally GildorInglorion Cards.gildorInglorion

-- | Action: Exhaust Gildor Inglorion to look at the top 3 cards of your deck. Switch one of
-- those cards with a card from your hand. Then, return the 3 cards to the top of your
-- deck, in any order.
instance HasAbilities GildorInglorion where
  getAbilities (GildorInglorion a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage GildorInglorion where
  runMessage msg s@(GildorInglorion a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      player <- getPlayer pid
      let top = take 3 player.deck
      chooseOneM pid do
        questionLabeled "gildorInglorion.fromDeck"
        causedBy a
        showing top
        unless (null player.hand) $ targets top \d -> chooseOneM pid do
          questionLabeled "gildorInglorion.fromHand"
          causedBy a
          targets player.hand \h -> do
            swapHandWithDeck pid h d
            returnToTopOfDeckInAnyOrder pid a [if c == d then h else c | c <- top]
        labeled "done" $ returnToTopOfDeckInAnyOrder pid a top
      pure s
    _ -> GildorInglorion <$> runMessage msg a
