module LordOfTheRings.Scenario.Scenarios.TheHuntForGollum (theHuntForGollum) where

import LordOfTheRings.Ability (forced, mkAbility)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLead, objectivesWithTrait)
import LordOfTheRings.Matcher.Window (WindowMatcher (PhaseEndsWindow))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheHuntForGollum.Helpers
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Trait (Trait (Clue))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheHuntForGollum = TheHuntForGollum ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHuntForGollum :: Scenario
theHuntForGollum =
  Scenario
    $ scenario (TheHuntForGollum . (nightmareDeckL ?~ nightmare)) "the-hunt-for-gollum" "The Hunt for Gollum"
 where
  nightmare =
    nightmareDeck
      Nightmare.theHuntForGollumNightmare
      EncounterSet.TheHuntForGollumNightmare
      [ (Enemies.mistyMountainGoblins, 2)
      , (Locations.banksOfTheAnduin, 2)
      , (Locations.gladdenFields, 3)
      , (Enemies.easternCrows, 3)
      , (Treacheries.evilStorm, 3)
      , (Treacheries.treacherousFog, 2)
      , (Locations.theEavesOfMirkwood, 1)
      , (Locations.riverNinglor, 2)
      , (Treacheries.falseLead, 2)
      ]

-- | Nightmare: "Forced: At the end of the refresh phase, choose an unclaimed Clue card not
-- attached to a Mordor enemy and attach it to a Mordor enemy, if able."
instance HasAbilities TheHuntForGollum where
  getAbilities (TheHuntForGollum a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | a.nightmare]

instance RunMessage TheHuntForGollum where
  runMessage msg (TheHuntForGollum attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheHuntForGollum, EncounterSet.JourneyAlongTheAnduin, EncounterSet.SauronsReach]
      firstStage <- genEncounterCard Quests.theHuntBegins
      laterStages <- traverse genEncounterCard [Quests.aNewTerrorAbroad, Quests.onTheTrail]
      putQuestIntoPlay firstStage
      pure $ TheHuntForGollum $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    UseCardAbility _ ScenarioSource 1 _ -> do
      onMordor <- cluesOnMordorEnemies
      clues <- objectivesWithTrait Clue
      enemies <- mordorEnemies
      let free = [o | o <- clues, o.placement == StagingArea, toId o `notElem` onMordor]
      lead <- getLead
      unless (null enemies) $ chooseOrRunOneM lead do
        questionLabeled "theHuntForGollumNightmare.clue"
        causedBy attrs
        for_ free \o -> targeting (toId o) $ attachClueToMordorEnemy attrs lead (toId o)
      pure $ TheHuntForGollum attrs
    -- Nightmare: "If at any point there are four or more Clue cards attached to Mordor
    -- enemies, the players lose the game." Counted once the Clue card has its guard.
    SetObjectiveGuard {} | attrs.nightmare -> do
      doStep 2 msg
      TheHuntForGollum <$> runMessage msg attrs
    DoStep 2 (SetObjectiveGuard {}) -> do
      clues <- length <$> cluesOnMordorEnemies
      when (clues >= 4) $ gameOver Lost
      pure $ TheHuntForGollum attrs
    -- Ravenous Hawk: "Forced: When a card effect instructs a player to "look" at cards in
    -- the encounter deck and Ravenous Hawk is among those cards, that player must put
    -- Ravenous Hawk into play engaged with him."
    -- ponytail: only the look at the top card; looks at several cards are not watched
    LookAtTopOfEncounterDeck pid _ _ | top : rest <- attrs.encounterDeck, toCardCode top == toCardCode Enemies.ravenousHawk -> do
      putEnemyIntoPlayEngaged pid top
      pure $ TheHuntForGollum $ attrs & encounterDeckL .~ rest
    _ -> TheHuntForGollum <$> runMessage msg attrs
