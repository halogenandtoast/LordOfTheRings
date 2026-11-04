module LordOfTheRings.Event.Cards.FlightToTheSea (flightToTheSea) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gamePlayers)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype FlightToTheSea = FlightToTheSea EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a Noldor or Silvan hero.
flightToTheSea :: EventCard FlightToTheSea
flightToTheSea =
  eventWith FlightToTheSea Cards.flightToTheSea \a ->
    let yours trait = exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait trait)
     in a & criteriaL .~ (DuringPhase PlanningPhase <> AnyCriteria [yours Noldor, yours Silvan])

-- | Planning Action: Shuffle a set aside Wind from the Sea into the encounter deck. Then,
-- remove Flight to the Sea from the game. Wind from the Sea (Encounter keyword) is the
-- scenario's card: it is found by its title among the players' set aside cards, as
-- Ranger Summons finds its Ranger of the North.
instance RunMessage FlightToTheSea where
  runMessage msg e@(FlightToTheSea attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      scenario <- toAttrs <$> getsGame (.scenario)
      -- one already shuffled in is no longer set aside
      let shuffled = map (.id) $ scenario.encounterDeck <> scenario.encounterDiscard
          isWind c = cardMatches (CardWithTitle "Wind from the Sea") c && c.id `notElem` shuffled
      winds <- getsGame (filter isWind . concatMap (.setAsideCards) . toList . gamePlayers)
      chooseOrRunOneM pid do
        questionLabeled "flightToTheSea.shuffle"
        causedBy attrs
        targets (take 1 winds) shuffleIntoEncounterDeck
      removeSourceCardFromGame (toSource attrs)
      pure e
    _ -> FlightToTheSea <$> runMessage msg attrs
