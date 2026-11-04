module LordOfTheRings.Event.Cards.ToArms (toArms) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ToArms = ToArms EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

toArms :: EventCard ToArms
toArms = eventWith ToArms Cards.toArms (criteriaL .~ exists AllyWithPlayerAttachment)

-- | Action: Choose an ally with a player attachment. Ready that ally.
instance RunMessage ToArms where
  runMessage msg e@(ToArms attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select AllyWithPlayerAttachment
      chooseOrRunOneM pid do
        questionLabeled "toArms.ally"
        causedBy attrs
        targets allies ready
      pure e
    _ -> ToArms <$> runMessage msg attrs
