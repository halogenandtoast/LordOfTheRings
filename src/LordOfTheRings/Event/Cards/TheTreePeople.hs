module LordOfTheRings.Event.Cards.TheTreePeople (theTreePeople) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype TheTreePeople = TheTreePeople EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Return a Silvan ally you control to your hand to search the top 5 cards of
-- your deck for a Silvan ally. Put that ally into play and shuffle the other cards back
-- into your deck. You can only play 1 copy of The Tree People each phase.
theTreePeople :: EventCard TheTreePeople
theTreePeople =
  eventWith TheTreePeople Cards.theTreePeople \a ->
    a
      & criteriaL
      .~ ( controlsAlly a.owner (AllyWithTrait Silvan)
            <> TargetLacksModifier (PlayerTarget a.owner) (PlayedThisPhase $ toCardCode Cards.theTreePeople)
         )

instance RunMessage TheTreePeople where
  runMessage msg e@(TheTreePeople attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      phaseModifier attrs (PlayerTarget pid) (PlayedThisPhase $ toCardCode Cards.theTreePeople)
      returnAllyToHand pid attrs (AllyWithTrait Silvan)
      searchDeckIntoPlay pid attrs 5 (CardWithType AllyType <> CardWithTrait Silvan) 1 []
      pure e
    _ -> TheTreePeople <$> runMessage msg attrs
