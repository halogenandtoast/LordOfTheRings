module LordOfTheRings.Location.Cards.ForbiddenDescent (forbiddenDescent) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype ForbiddenDescent = ForbiddenDescent LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

forbiddenDescent :: LocationCard ForbiddenDescent
forbiddenDescent = location ForbiddenDescent Cards.forbiddenDescent

-- | While Forbidden Descent is in the staging area, the first 2 progress that would be
-- placed on a quest each round must be placed on Forbidden Descent, instead.
instance HasModifiersFor ForbiddenDescent where
  getModifiersFor (ForbiddenDescent a) = when (a.placement == StagingArea) $ modified a a [TakesQuestProgressFirst 2]

-- | Forced: If Forbidden Descent leaves play from the staging area, raise each player's
-- threat by 2.
instance RunMessage ForbiddenDescent where
  runMessage msg (ForbiddenDescent a) = case msg of
    _ | a.placement == StagingArea, leaves msg -> do
      eachPlayer \p -> encounterRaisesThreat p 2
      ForbiddenDescent <$> runMessage msg a
    _ -> ForbiddenDescent <$> runMessage msg a
   where
    leaves = \case
      Discard t -> isTarget a t
      AddToVictoryDisplay t -> isTarget a t
      RemoveFromGame t -> isTarget a t
      ShuffleIntoEncounterDeck t -> isTarget a t
      ReturnToTopOfEncounterDeck t -> isTarget a t
      ReturnToBottomOfEncounterDeck t -> isTarget a t
      _ -> False
