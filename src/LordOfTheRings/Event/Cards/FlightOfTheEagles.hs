module LordOfTheRings.Event.Cards.FlightOfTheEagles (flightOfTheEagles) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gamePlayers)
import LordOfTheRings.Trait (Trait (Eagle))

newtype FlightOfTheEagles = FlightOfTheEagles EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Its cost: an Eagle ally you control to return to your hand.
flightOfTheEagles :: EventCard FlightOfTheEagles
flightOfTheEagles =
  eventWith FlightOfTheEagles Cards.flightOfTheEagles \a ->
    a & criteriaL .~ exists (AllyControlledBy (PlayerWithId a.owner) <> AllyWithTrait Eagle)

-- | Action: Return an Eagle ally to your hand to shuffle a set-aside Eagle of the North
-- into the encounter deck. Then, remove Flight of the Eagles from the game. "A set-aside"
-- Eagle of the North is any player's, as Flight to the Sea finds its Wind from the Sea.
instance RunMessage FlightOfTheEagles where
  runMessage msg e@(FlightOfTheEagles attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      returnAllyToHand pid attrs (AllyWithTrait Eagle)
      scenario <- toAttrs <$> getsGame (.scenario)
      -- one already shuffled in is still its owner's, but no longer set aside
      let shuffled = map (.id) $ scenario.encounterDeck <> scenario.encounterDiscard
          isEagle c = cardMatches (cardIs Allies.eagleOfTheNorth) c && c.id `notElem` shuffled
      eagles <- getsGame (filter isEagle . concatMap (.setAsideCards) . toList . gamePlayers)
      chooseOrRunOneM pid do
        questionLabeled "flightOfTheEagles.shuffle"
        causedBy attrs
        targets eagles shuffleIntoEncounterDeck
      removeSourceCardFromGame (toSource attrs)
      pure e
    _ -> FlightOfTheEagles <$> runMessage msg attrs
