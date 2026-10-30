module LordOfTheRings.Event.Cards.HeedTheDream (heedTheDream) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype HeedTheDream = HeedTheDream EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

heedTheDream :: EventCard HeedTheDream
heedTheDream = event HeedTheDream Cards.heedTheDream

-- | Action: Choose a player. That player searches the top 5 cards of his deck for a card,
-- adds it to his hand, and shuffles his deck. Then, the players as a group may spend 3
-- leadership resources to have that player search his deck for another card, add it to
-- his hand, and shuffle his deck.
instance RunMessage HeedTheDream where
  runMessage msg e@(HeedTheDream attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> do
        searchDeck p attrs 5 AnyCard False
        leadership <- getGroupResourcesOfSphere Leadership
        when (leadership >= 3) $ chooseOneM pid do
          questionLabeled "heedTheDream.spend"
          causedBy attrs
          labeled "heedTheDream.searchAgain" do
            spendGroupResources pid Leadership 3
            deckSize <- length . (.deck) <$> getPlayer p
            searchDeck p attrs deckSize AnyCard False
          labeled "heedTheDream.done" $ pure ()
      pure e
    _ -> HeedTheDream <$> runMessage msg attrs
