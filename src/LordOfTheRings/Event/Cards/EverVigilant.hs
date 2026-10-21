module LordOfTheRings.Event.Cards.EverVigilant (everVigilant) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype EverVigilant = EverVigilant EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose and ready an ally card.
everVigilant :: EventCard EverVigilant
everVigilant = eventWith EverVigilant Cards.everVigilant (criteriaL .~ exists ExhaustedAlly)

instance RunMessage EverVigilant where
  runMessage msg e@(EverVigilant attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select ExhaustedAlly
      chooseOneM pid do
        questionLabeled "everVigilant.ready"
        causedBy attrs
        targets allies ready
      pure e
    _ -> EverVigilant <$> runMessage msg attrs
