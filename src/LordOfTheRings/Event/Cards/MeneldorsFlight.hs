module LordOfTheRings.Event.Cards.MeneldorsFlight (meneldorsFlight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Eagle))

newtype MeneldorsFlight = MeneldorsFlight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose an Eagle ally. Return that character to its owner's hand.
meneldorsFlight :: EventCard MeneldorsFlight
meneldorsFlight = eventWith MeneldorsFlight Cards.meneldorsFlight (criteriaL .~ exists (AllyWithTrait Eagle))

instance RunMessage MeneldorsFlight where
  runMessage msg e@(MeneldorsFlight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      eagles <- select (AllyWithTrait Eagle)
      chooseOneM pid do
        questionLabeled "meneldorsFlight.return"
        causedBy attrs
        targets eagles returnToHand
      pure e
    _ -> MeneldorsFlight <$> runMessage msg attrs
