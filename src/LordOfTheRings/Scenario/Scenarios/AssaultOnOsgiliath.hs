module LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath (assaultOnOsgiliath) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (LocationMatcher (AnyLocation, LocationInStagingArea, LocationWithTrait), getLead, getLocation, getRemainingPlayers, select)
import LordOfTheRings.Modifier (ModifierType (ProgressBeyondActiveTo))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Trait (Trait (Osgiliath))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PhaseBegins))

newtype AssaultOnOsgiliath = AssaultOnOsgiliath ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

assaultOnOsgiliath :: Scenario
assaultOnOsgiliath =
  Scenario
    $ scenario (AssaultOnOsgiliath . (nightmareDeckL ?~ nightmare)) "assault-on-osgiliath" "Assault on Osgiliath"
 where
  nightmare =
    nightmareDeck
      Nightmare.assaultOnOsgiliathNightmare
      EncounterSet.AssaultOnOsgiliathNightmare
      [ (Enemies.urukLieutenant, 1)
      , (Enemies.urukSoldier, 4)
      , (Enemies.southronPhalanx, 3)
      , (Locations.westQuarter, 2)
      , (Locations.eastQuarter, 2)
      , (Locations.ruinedSquare, 1)
      , (Locations.ruinedTower, 1)
      , (Treacheries.streetFighting, 2)
      , (Treacheries.theMastersMalice, 2)
      , (Treacheries.thePowerOfMordor, 1)
      , (Enemies.southronMercenaries, 3)
      ]

instance RunMessage AssaultOnOsgiliath where
  runMessage msg (AssaultOnOsgiliath attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.AssaultOnOsgiliath, EncounterSet.MordorElite, EncounterSet.Southrons]
      firstStage <- genEncounterCard Quests.retakeTheCity
      putQuestIntoPlay firstStage
      pure $ AssaultOnOsgiliath $ attrs & encounterDeckL .~ encounterDeck
    -- Nightmare: "Setup: Add Palace Remains to the staging area."
    DoStep 8 Setup | attrs.nightmare -> do
      for_ (find ((== toCardCode Locations.palaceRemains) . toCardCode) attrs.encounterDeck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      pure $ AssaultOnOsgiliath attrs
    -- Nightmare: "If there are more Osgiliath locations in the staging area than players in
    -- the game, this card gains: 'Response: At the beginning of the quest phase, choose 1
    -- non-unique location. If the players quest successfully this phase, remaining progress
    -- made beyond the active location's quest points is placed on the chosen location.'"
    -- The first player answers for the players.
    CheckWindows ws | attrs.nightmare, Window After (PhaseBegins QuestPhase) `elem` ws -> do
      staged <- length <$> select (LocationInStagingArea <> LocationWithTrait Osgiliath)
      players <- length <$> getRemainingPlayers
      when (staged > players) do
        lead <- getLead
        locations <- filterM (fmap (not . cdUnique . toCardDef) . getLocation) =<< select AnyLocation
        chooseOneM lead do
          questionLabeled "assaultOnOsgiliath.response"
          causedBy attrs
          targets locations \l -> phaseModifier ScenarioSource GameTarget (ProgressBeyondActiveTo $ toTarget l)
          labeled "assaultOnOsgiliath.skip" (pure ())
      AssaultOnOsgiliath <$> runMessage msg attrs
    _ -> AssaultOnOsgiliath <$> runMessage msg attrs
