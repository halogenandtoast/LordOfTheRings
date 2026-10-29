module LordOfTheRings.Objective.Cards.TheOrcsSearch (theOrcsSearch) where

import LordOfTheRings.Card (CardDef (..), toCard, toCardDef)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype TheOrcsSearch = TheOrcsSearch ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOrcsSearch :: ObjectiveCard TheOrcsSearch
theOrcsSearch = objective TheOrcsSearch Cards.theOrcsSearch

-- | Forced: At the end of the refresh phase, raise each player's threat by 1 for each card
-- underneath The Orcs' Search.
instance HasAbilities TheOrcsSearch where
  getAbilities (TheOrcsSearch a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]

-- | When a location has damage equal to its printed quest points, place it facedown
-- underneath The Orcs' Search: read as damage placed on a location is checked.
instance RunMessage TheOrcsSearch where
  runMessage msg o@(TheOrcsSearch attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid (length attrs.objectiveUnderneath)
      pure o
    CheckDefeated _ (LocationTarget lid) -> do
      searched <- selectAny (LocationWithId lid)
      when searched do
        l <- getLocation lid
        let printed = fromMaybe 0 (cdQuestPoints $ toCardDef l)
        when (printed > 0 && (toAttrs l).damage >= printed) $ placeUnderneath attrs [toCard l]
      pure o
    _ -> TheOrcsSearch <$> runMessage msg attrs
