module LordOfTheRings.Treachery.Cards.VastnessOfMirkwood (vastnessOfMirkwood) where

import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (objectiveLocationMatcher, objectiveLocationsInPlay)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype VastnessOfMirkwood = VastnessOfMirkwood TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

vastnessOfMirkwood :: TreacheryCard VastnessOfMirkwood
vastnessOfMirkwood = treachery VastnessOfMirkwood Cards.vastnessOfMirkwood

{- | When Revealed: Until the end of the phase, each non-objective location in the staging
area gets +1 threat (+2 threat instead if it has at least 1 progress token on it). If the
number of non-objective locations in the staging area is less than the number of players,
Vastness of Mirkwood gains surge.
An encounter card's lasting effect is recalculated as the game state changes (FAQ 1.55),
and one on "each location in the staging area" reaches a location that enters it later in
the phase (FAQ, Treacherous Fog): a location added later gets it, and one that takes its
first progress goes to +2. The surge is counted as it resolves.
-}
instance RunMessage VastnessOfMirkwood where
  runMessage msg t@(VastnessOfMirkwood attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      let nonObjective = EachMatchingLocationInStagingArea (NotCard objectiveLocationMatcher)
      phaseModifier attrs GameTarget (nonObjective $ ThreatModifier 1)
      phaseModifier attrs GameTarget (nonObjective $ ThreatWithToken Progress 1)
      objectives <- objectiveLocationsInPlay
      staged <- filter (`notElem` objectives) <$> select LocationInStagingArea
      players <- length <$> getRemainingPlayers
      when (length staged < players) revealEncounterCard
      pure t
    _ -> VastnessOfMirkwood <$> runMessage msg attrs
