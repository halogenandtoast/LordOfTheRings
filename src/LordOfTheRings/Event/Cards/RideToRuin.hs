module LordOfTheRings.Event.Cards.RideToRuin (rideToRuin) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype RideToRuin = RideToRuin EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Discard a Rohan ally to choose a location. Place 3 progress tokens on that
-- location.
rideToRuin :: EventCard RideToRuin
rideToRuin =
  eventWith RideToRuin Cards.rideToRuin \a ->
    a & criteriaL .~ (exists (AllyControlledBy (PlayerWithId a.owner) <> AllyWithTrait Rohan) <> exists AnyLocation)

instance RunMessage RideToRuin where
  runMessage msg e@(RideToRuin attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Rohan)
      locations <- select AnyLocation
      chooseOneM pid do
        questionLabeled "rideToRuin.discard"
        causedBy attrs
        targets allies \x -> do
          discard x
          chooseOneM pid do
            questionLabeled "rideToRuin.location"
            causedBy attrs
            targets locations \l -> placeProgress attrs l 3
      pure e
    _ -> RideToRuin <$> runMessage msg attrs
