module LordOfTheRings.Event.Cards.DwarvenTomb (dwarvenTomb) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype DwarvenTomb = DwarvenTomb EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Return 1 Spirit card from your discard pile to your hand.
dwarvenTomb :: EventCard DwarvenTomb
dwarvenTomb = event DwarvenTomb Cards.dwarvenTomb

instance RunMessage DwarvenTomb where
  runMessage msg e@(DwarvenTomb attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      -- Dwarven Tomb is in the discard pile already, and may not return itself
      discard <- filter (\c -> c.id /= card.id && c.sphere == Just Spirit) . (.discard) <$> getPlayer pid
      unless (null discard)
        $ push
        $ questionLabelWith (ikey "label.dwarvenTomb.return") (Just $ toTarget attrs) [] pid
          [targetLabel (CardIdTarget c.id) [ReturnFromDiscardToHand pid c.id] | c <- discard]
      pure e
    _ -> DwarvenTomb <$> runMessage msg attrs
