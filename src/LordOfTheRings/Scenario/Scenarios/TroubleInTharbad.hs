module LordOfTheRings.Scenario.Scenarios.TroubleInTharbad (troubleInTharbad) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode), LocationMatcher (LocationWithCardCode), select, selectAny)
import LordOfTheRings.Modifier (ModifierType (ProgressLimit))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (eliminationLowering)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TroubleInTharbad = TroubleInTharbad ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

troubleInTharbad :: Scenario
troubleInTharbad =
  Scenario
    $ scenario (TroubleInTharbad . (nightmareDeckL ?~ nightmare)) "trouble-in-tharbad" "Trouble in Tharbad"
 where
  nightmare =
    nightmareDeck
      Nightmare.troubleInTharbadNightmare
      EncounterSet.TroubleInTharbadNightmare
      [ (Enemies.orcHunter, 1)
      , (Enemies.orcSkirmisher, 2)
      , (Locations.tharbadHideout, 2)
      , (Locations.hiddenAlleyway, 2)
      , (Locations.decrepitRooftops, 1)
      , (Treacheries.orcHuntingParty, 2)
      ]

-- | Nightmare: "Progress cannot be placed on The Crossing at Tharbad while Bellach is in
-- play."
instance HasModifiersFor TroubleInTharbad where
  getModifiersFor (TroubleInTharbad a) = when a.nightmare do
    bellach <- selectAny (EnemyWithCardCode $ toCardCode Enemies.bellachTroubleInTharbad)
    when bellach do
      crossings <- select (LocationWithCardCode $ toCardCode Locations.theCrossingAtTharbad)
      for_ crossings \l -> modified a l [ProgressLimit 0]

-- | The scenario keeps how many times Double Dealings has lowered each player's threat
-- elimination level, by 10 each, which the stages apply.
instance RunMessage TroubleInTharbad where
  runMessage msg (TroubleInTharbad attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.TroubleInTharbad, EncounterSet.MistyMountainOrcs]
      firstStage <- genEncounterCard Quests.doubleDealings
      laterStages <- traverse genEncounterCard [Quests.escapeFromTharbad]
      putQuestIntoPlay firstStage
      pure $ TroubleInTharbad $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Setup: Raise each player's starting threat by 5."
    DoStep 8 Setup | attrs.nightmare -> do
      eachPlayer \p -> raiseThreat p 5
      pure $ TroubleInTharbad attrs
    ScenarioSpecific "tharbad.lowerElimination" v -> do
      let by = fromMaybe 10 $ parseMaybe parseJSON v
      pure $ TroubleInTharbad $ attrs & metaL .~ object ["lowered" .= (eliminationLowering attrs.meta + by)]
    _ -> TroubleInTharbad <$> runMessage msg attrs
