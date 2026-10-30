module LordOfTheRings.Event.Cards.LordsOfTheEldar (lordsOfTheEldar) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor))

newtype LordsOfTheEldar = LordsOfTheEldar EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Lords of the Eldar can only be played from your discard pile, as its card def says.
lordsOfTheEldar :: EventCard LordsOfTheEldar
lordsOfTheEldar = event LordsOfTheEldar Cards.lordsOfTheEldar

-- | Action: Place Lords of the Eldar on the bottom of your deck from your discard pile.
-- Then, until the end of the round all Noldor characters get +1 willpower, +1 attack, and
-- +1 defense. Played, it is back in the discard pile by the time it moves, as Elven-light.
instance RunMessage LordsOfTheEldar where
  runMessage msg e@(LordsOfTheEldar attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      putDiscardOnBottomOfDeck pid card.id
      noldor <- selectCharacters (HeroWithTrait Noldor) (AllyWithTrait Noldor)
      for_ noldor \c -> roundModifiers attrs c [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure e
    _ -> LordsOfTheEldar <$> runMessage msg attrs
