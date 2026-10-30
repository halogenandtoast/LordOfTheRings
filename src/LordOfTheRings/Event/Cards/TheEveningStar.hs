module LordOfTheRings.Event.Cards.TheEveningStar (theEveningStar) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheEveningStar = TheEveningStar EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Place 2 progress on any location. Resolve that effect again for each copy of
-- The Evening Star currently in your discard pile (you may choose new targets).
theEveningStar :: EventCard TheEveningStar
theEveningStar = eventWith TheEveningStar Cards.theEveningStar (criteriaL .~ exists LocationNotImmuneToPlayerCards)

-- | Each time the effect resolves is a later step, carrying how many are still to come.
instance RunMessage TheEveningStar where
  runMessage msg e@(TheEveningStar attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      again <- copiesInDiscard pid card
      doStep again msg
      pure e
    DoStep n msg'@(PlayEvent pid card _) | card.id == toId attrs -> do
      locations <- select AnyLocation
      chooseOneM pid do
        questionLabeled "theEveningStar.location"
        causedBy attrs
        targets locations \l -> placeProgress attrs l 2
      when (n > 0) $ doStep (n - 1) msg'
      pure e
    _ -> TheEveningStar <$> runMessage msg attrs
