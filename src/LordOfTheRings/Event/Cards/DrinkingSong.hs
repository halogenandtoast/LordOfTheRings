module LordOfTheRings.Event.Cards.DrinkingSong (drinkingSong) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype DrinkingSong = DrinkingSong EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

drinkingSong :: EventCard DrinkingSong
drinkingSong = event DrinkingSong Cards.drinkingSong

-- | Action: Count the number of cards in your hand and shuffle your hand into your deck.
-- Then, draw an equal number of cards. Draw 1 additional card if you control a unique
-- Hobbit character.
--
-- An encounter card held in hand is counted, but stays: it is no card of the player's deck.
instance RunMessage DrinkingSong where
  runMessage msg e@(DrinkingSong attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      hand <- (.hand) <$> getPlayer pid
      for_ [pc | PlayerCard pc <- hand] (shuffleFromHandIntoOwnersDeck pid)
      hobbit <-
        orM
          [ selectAny (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Hobbit)
          , selectAny (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Hobbit <> UniqueAlly)
          ]
      drawCards attrs pid (length hand + if hobbit then 1 else 0)
      pure e
    _ -> DrinkingSong <$> runMessage msg attrs
