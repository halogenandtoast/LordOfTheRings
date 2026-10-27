module LordOfTheRings.Quest.Cards.NarrowPaths (narrowPaths) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers

newtype NarrowPaths = NarrowPaths QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

narrowPaths :: QuestCard NarrowPaths
narrowPaths = quest NarrowPaths Cards.narrowPaths

instance HasAbilities NarrowPaths where
  getAbilities (NarrowPaths a) = searchForAnExit a <> bypass a

-- | When Revealed: Each player chooses 1 questing character he controls. Each questing
-- character not chosen does not count its willpower until the end of the turn.
-- Forced: After placing the 1st progress token on Narrow Paths, search the encounter deck
-- and discard pile for Abandoned Tools and add it to the staging area, if able.
instance RunMessage NarrowPaths where
  runMessage msg (NarrowPaths attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      eachPlayer \p -> do
        questing <- selectCharacters (HeroControlledBy (PlayerWithId p) <> CommittedHero) (AllyControlledBy (PlayerWithId p) <> CommittedAlly)
        chooseOrRunOneM p do
          questionLabeled "narrowPaths.choose"
          causedBy attrs
          targets questing \chosen ->
            for_ (filter (/= chosen) questing) \c -> roundModifier attrs c DoesNotCountWillpower
      NarrowPaths <$> runSearchForAnExit msg attrs
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped && attrs.progress == 0 && n > 0 -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Objectives.abandonedTools)
      NarrowPaths <$> runSearchForAnExit msg attrs
    _ -> NarrowPaths <$> runSearchForAnExit msg attrs
