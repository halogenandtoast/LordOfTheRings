module LordOfTheRings.Objective.Cards.TheHavensBurn (theHavensBurn) where

import LordOfTheRings.Card (toCard, toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype TheHavensBurn = TheHavensBurn ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHavensBurn :: ObjectiveCard TheHavensBurn
theHavensBurn = objective TheHavensBurn Cards.theHavensBurn

{- | Aflame X: a location with as much damage as its Aflame value is destroyed and placed
facedown underneath The Havens Burn, read as damage placed on a location is checked.
Forced: When a card is placed underneath The Havens Burn, reveal the top card of the
encounter deck.
If there are X or more cards underneath The Havens Burn, the players lose the game. X is 3
more than the number of players in the game. So do they as the Dream-chaser goes there,
by its own text: read here, so no card is revealed first.
-}
instance RunMessage TheHavensBurn where
  runMessage msg o@(TheHavensBurn attrs) = case msg of
    CheckDefeated _ (LocationTarget lid) -> do
      present <- selectAny (LocationWithId lid)
      when present do
        aflame <- getAflame lid
        l <- getLocation lid
        when (aflame > 0 && (toAttrs l).damage >= aflame) $ placeUnderneath attrs [toCard l]
      pure o
    PlaceUnderneath (isTarget attrs -> True) cards -> do
      players <- length <$> getRemainingPlayers
      let total = length attrs.objectiveUnderneath + length cards
          dreamChaser = any ((== toCardCode Locations.dreamChaserLocation) . toCardCode) cards
      if dreamChaser || total >= players + 3 then gameOver Lost else revealEncounterCard
      TheHavensBurn <$> runMessage msg attrs
    _ -> TheHavensBurn <$> runMessage msg attrs
