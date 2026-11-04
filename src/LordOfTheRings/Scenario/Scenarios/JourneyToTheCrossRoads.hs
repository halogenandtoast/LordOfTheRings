module LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads (journeyToTheCrossRoads) where

import Data.Aeson.Types (parseMaybe)
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Campaign (CampaignLogKey (HaradEnemies), PoolCard (..), addToPool, recordCount)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (HeroControlledBy), PlayerMatcher (PlayerWithId), getRemainingPlayers, select)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (objectiveUnderneath)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (getTheBlackGate)

newtype JourneyToTheCrossRoads = JourneyToTheCrossRoads ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyToTheCrossRoads :: Scenario
journeyToTheCrossRoads = Scenario $ scenario JourneyToTheCrossRoads "journey-to-the-cross-roads" "Journey to the Cross-roads"

-- | The players who shuffled Brace of Coneys into their decks at the campaign's setup.
coneysTakenBy :: ScenarioAttrs -> [PlayerId]
coneysTakenBy attrs = fromMaybe [] $ parseMaybe parseJSON attrs.meta

{- | Journey to the Cross-roads and Men of Harad make up the encounter deck (The Land of
Shadow rules insert). The Black Gate and The Cross-roads are set aside for stage 1A to add
to the staging area, and the Men of Harad set for it to place under The Cross-roads.

The first player takes control of the Ring-bearer as the game sets up (see
'LordOfTheRings.Saga.fellowshipSetup').
-}
instance RunMessage JourneyToTheCrossRoads where
  runMessage msg (JourneyToTheCrossRoads attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.JourneyToTheCrossRoads, EncounterSet.HaradSaga]
      let aside c =
            toCardCode c `elem` map toCardCode [Objectives.theBlackGate, Locations.theCrossRoadsTheLandOfShadow]
              || cdEncounterSet (toCardDef c) == Just EncounterSet.HaradSaga
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.ambushTheMenOfHarad
      putQuestIntoPlay firstStage
      pure $ JourneyToTheCrossRoads $ attrs & encounterDeckL .~ deck & setAsideL .~ setAside'
    -- Campaign: Setup: Each player must choose: either add 1 resource to each of his
    -- heroes' pools, or shuffle 1 copy of Brace of Coneys into his deck. (The burdens of
    -- Helm's Deep and The Road to Isengard are left out as the deck is dealt: see
    -- 'LordOfTheRings.Saga.removedBurdenSets'.)
    DoStep 9 Setup | isJust attrs.campaign -> do
      players <- getRemainingPlayers
      for_ players \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        chooseOneM pid do
          questionLabeled "journeyToTheCrossRoads.setup"
          labeled "journeyToTheCrossRoads.resources" $ for_ heroes \h -> placeResources attrs h 1
          labeled "journeyToTheCrossRoads.braceOfConeys" do
            shuffleIntoDeck pid =<< genPlayerCard Events.braceOfConeys (Just pid)
            push $ ScenarioSpecific "journeyToTheCrossRoads.coneys" (toJSON pid)
      JourneyToTheCrossRoads <$> runMessage msg attrs
    ScenarioSpecific "journeyToTheCrossRoads.coneys" (parseMaybe parseJSON -> Just pid) ->
      pure $ JourneyToTheCrossRoads $ attrs & metaL .~ toJSON (coneysTakenBy attrs <> [pid])
    -- Resolution: Each player who chose to shuffle a copy of Brace of Coneys into his deck
    -- during setup has earned that boon. Add each earned copy to the campaign pool. Record
    -- the name and quantity of each enemy underneath The Black Gate in the notes section
    -- of the campaign log.
    ResolveCampaign | Just l <- attrs.campaign -> do
      order <- getsGame (.playerOrder)
      gate <- getTheBlackGate
      let coneys = [PoolCard (toCardCode Events.braceOfConeys) (elemIndex p order) Nothing | p <- coneysTakenBy attrs]
          under = Map.fromListWith (+) [(toCardCode c, 1 :: Int) | g <- toList gate, c <- objectiveUnderneath (toAttrs g)]
          logged = Map.foldrWithKey (recordCount HaradEnemies) (foldl' (flip addToPool) l coneys) under
      JourneyToTheCrossRoads <$> runMessage msg (attrs {scenarioCampaign = Just logged})
    _ -> JourneyToTheCrossRoads <$> runMessage msg attrs
