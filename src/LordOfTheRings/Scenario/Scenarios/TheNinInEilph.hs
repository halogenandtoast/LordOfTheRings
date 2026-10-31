module LordOfTheRings.Scenario.Scenarios.TheNinInEilph (theNinInEilph) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches, getEnemy)
import LordOfTheRings.Placement (Placement (StagingArea), placementController)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (addMarshDweller, getMarshDweller, stageOf)
import LordOfTheRings.Source (Source (GameSource))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (AdvancedToStage))

newtype TheNinInEilph = TheNinInEilph ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theNinInEilph :: Scenario
theNinInEilph =
  Scenario
    $ scenario (TheNinInEilph . (nightmareDeckL ?~ nightmare)) "the-nin-in-eilph" "The Nîn-in-Eilph"
 where
  -- A Weary Passage and Impassable Marshland are replaced in the quest deck
  nightmare =
    nightmareDeck
      Nightmare.theNinInEilphNightmare
      EncounterSet.TheNinInEilphNightmare
      [ (Enemies.neekerbreekers, 3)
      , (Locations.hiddenEyot, 1)
      , (Locations.fingerOfGlanduin, 3)
      , (Treacheries.offTrack, 2)
      , (Treacheries.lowOnProvisions, 1)
      ]

{- | The quest deck holds every later stage. The players advance to a stage of the next
number at random: as a stage is defeated, the stages of its number and lower leave the
quest deck, and the next is drawn at random from the lowest number left.
-}
instance RunMessage TheNinInEilph where
  runMessage msg (TheNinInEilph attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.TheNinInEilph, EncounterSet.WearyTravelers]
      firstStage <- genEncounterCard Quests.fleeingFromTharbad
      laterStages <-
        traverse
          genEncounterCard
          [ Quests.throughTheMarshNoEndInSight
          , nightmareOr attrs Quests.throughTheMarshAWearyPassage Quests.throughTheMarshAnArduousJourney
          , Quests.throughTheMarshAForgottenLand
          , nightmareOr attrs Quests.lostInTheSwanfleetImpassableMarshland Quests.lostInTheSwanfleetDeadlyWaters
          , Quests.lostInTheSwanfleetATreacherousSwamp
          , Quests.lostInTheSwanfleetCreaturesOfAForgottenAge
          , Quests.outOfTheSwamp
          ]
      putQuestIntoPlay firstStage
      pure $ TheNinInEilph $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    CompleteQuest qid -> do
      defeated <- getsGame (fmap stageOf . Map.lookup qid . (.entities.quests))
      let kept = filter (\c -> maybe True (stageOf c >) defeated) attrs.questDeck
      TheNinInEilph <$> runMessage msg (attrs & questDeckL .~ kept)
    AdvanceQuest | Just lowest <- minimumMay (map stageOf attrs.questDeck) -> do
      next <- uniform (filter ((== lowest) . stageOf) attrs.questDeck)
      advanceToQuest next
      pure $ TheNinInEilph $ attrs & questDeckL %~ filter (/= next)
    -- Nightmare: "Forced: When the players advance to stage 2A, add Ancient Marsh-dweller to
    -- the staging area (from out of play or engaged with a player)."
    AdvanceToQuest card | attrs.nightmare && stageOf card == 2 -> do
      addMarshDweller
      TheNinInEilph <$> runMessage msg attrs
    -- Swarms of Mosquitoes, in the victory display: "Forced: After the players advance to a
    -- quest stage, return Swarms of Mosquitoes to the staging area."
    CheckWindows ws | Window After AdvancedToStage `elem` ws -> do
      let swarms = [c | EncounterCard c <- attrs.victoryDisplay, cardMatches (cardIs Enemies.swarmsOfMosquitos) c]
      traverse_ addToStaging swarms
      TheNinInEilph <$> runMessage msg (attrs & victoryDisplayL %~ filter (`notElem` map EncounterCard swarms))
    ScenarioSpecific "marshDweller.add" _ ->
      getMarshDweller >>= \case
        Just eid -> do
          dweller <- getEnemy eid
          when (isJust $ placementController dweller.placement) $ placeEnemy eid StagingArea
          healAllDamage GameSource eid
          pure $ TheNinInEilph attrs
        Nothing -> do
          let isDweller :: HasCardDef a => a -> Bool
              isDweller = cardMatches (cardIs Enemies.ancientMarshDweller)
              outOfPlay = attrs.scenarioSetAside <> [c | EncounterCard c <- attrs.victoryDisplay] <> attrs.encounterDiscard
          for_ (find isDweller outOfPlay) addToStaging
          pure
            $ TheNinInEilph
            $ attrs
            & (setAsideL %~ filter (not . isDweller))
            & (victoryDisplayL %~ filter (not . isDweller))
            & (encounterDiscardL %~ filter (not . isDweller))
    _ -> TheNinInEilph <$> runMessage msg attrs
