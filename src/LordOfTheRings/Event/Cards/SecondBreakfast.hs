module LordOfTheRings.Event.Cards.SecondBreakfast (secondBreakfast) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype SecondBreakfast = SecondBreakfast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Each player returns the topmost attachment card from his discard pile to his
-- hand.
secondBreakfast :: EventCard SecondBreakfast
secondBreakfast = event SecondBreakfast Cards.secondBreakfast

instance RunMessage SecondBreakfast where
  runMessage msg e@(SecondBreakfast attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> do
        discards <- (.discard) <$> getPlayer p
        for_ (find ((== AttachmentType) . (.cardType)) discards) \c -> returnFromDiscardToHand p c.id
      pure e
    _ -> SecondBreakfast <$> runMessage msg attrs
