module LordOfTheRings.Quest.Cards.FlightToGondor (flightToGondor) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FlightToGondor = FlightToGondor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

flightToGondor :: QuestCard FlightToGondor
flightToGondor = quest FlightToGondor Cards.flightToGondor

-- | Allies enter play exhausted.
instance HasModifiersFor FlightToGondor where
  getModifiersFor (FlightToGondor a) = when a.flipped $ modified a GameTarget [AlliesEnterPlayExhausted]

-- | Forced: At the end of the planning phase, advance to a random stage 2 (the scenario
-- picks it).
instance HasAbilities FlightToGondor where
  getAbilities (FlightToGondor a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | a.flipped]

{- | Setup: Set Poros Garrison and each of the following encounter sets aside, out of play:
Mountains of Shadow, Desert Sands, Desert Creatures, Harad Soldiers, and Mordor Orcs. The
first player adds The Black Serpent to the staging area and shuffles the encounter deck.
Each other player reveals an encounter card. The scenario set the cards aside as it
gathered the encounter deck, The Black Serpent among them.
-}
instance RunMessage FlightToGondor where
  runMessage msg q@(FlightToGondor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.theBlackSerpentCrossingsOfPoros)
      shuffleEncounterDeck
      others <- drop 1 <$> getRemainingPlayers
      revealEncounterCards (length others)
      FlightToGondor <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      completeQuest attrs.id
      pure q
    _ -> FlightToGondor <$> runMessage msg attrs
