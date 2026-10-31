module LordOfTheRings.Scenario.Scenarios.TheDeadMarshes (theDeadMarshes) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEncounterDiscard, getLead, getRemainingPlayers)
import LordOfTheRings.Matcher.Types (cardIs)
import LordOfTheRings.Modifier (ModifierType (CannotReduceThreat))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheDeadMarshes = TheDeadMarshes ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDeadMarshes :: Scenario
theDeadMarshes =
  Scenario
    $ scenario (TheDeadMarshes . (nightmareDeckL ?~ nightmare)) "the-dead-marshes" "The Dead Marshes"
 where
  nightmare =
    nightmareDeck
      Nightmare.theDeadMarshesNightmare
      EncounterSet.TheDeadMarshesNightmare
      [ (Enemies.easternCrows, 3)
      , (Treacheries.pursuedByShadow, 2)
      , (Treacheries.treacherousFog, 2)
      , (Treacheries.aWispOfPaleSheen, 1)
      , (Treacheries.throughTheMist, 1)
      , (Enemies.giantMarshWorm, 2)
      , (Locations.fensAndMires, 4)
      , (Enemies.wolfRider, 1)
      , (Enemies.hillTroll, 2)
      , (Enemies.goblinSniper, 2)
      , (Enemies.wargs, 2)
      , (Treacheries.despair, 2)
      , (Locations.theBrownLands, 2)
      , (Locations.theEastBight, 2)
      ]

-- | Nightmare: "Players cannot reduce their threat."
instance HasModifiersFor TheDeadMarshes where
  getModifiersFor (TheDeadMarshes a) = when a.nightmare do
    players <- getRemainingPlayers
    for_ players \p -> modified a p [CannotReduceThreat]

instance RunMessage TheDeadMarshes where
  runMessage msg (TheDeadMarshes attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- gatherEncounterSets attrs [EncounterSet.TheDeadMarshes, EncounterSet.SauronsReach, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.intoTheMarshes
      laterStages <- traverse genEncounterCard [Quests.theCapture]
      putQuestIntoPlay firstStage
      pure $ TheDeadMarshes $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Setup: Search the encounter deck for The Mere of Dead Faces, and add it to
    -- the staging area. Then shuffle the encounter deck."
    DoStep 8 Setup | attrs.nightmare -> do
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Locations.theMereOfDeadFaces)
      pure $ TheDeadMarshes attrs
    -- Nightmare: "Forced: After Gollum is placed in the discard pile, shuffle him into the
    -- encounter deck." Looked for once a card has reached the discard pile, as one dealt
    -- for an escape test or as a shadow card does.
    AddToEncounterDiscard _ | attrs.nightmare -> do
      doStep 3 msg
      TheDeadMarshes <$> runMessage msg attrs
    ResolveEscapeTest _ | attrs.nightmare -> do
      doStep 3 msg
      TheDeadMarshes <$> runMessage msg attrs
    DoStep 3 _ -> do
      discarded <- getEncounterDiscard
      when (any ((== toCardCode Objectives.gollumTheDeadMarshes) . toCardCode) discarded)
        $ shuffleEncounterDiscardIntoDeck (cardIs Objectives.gollumTheDeadMarshes)
      pure $ TheDeadMarshes attrs
    _ -> TheDeadMarshes <$> runMessage msg attrs
