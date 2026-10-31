{- | Flight of the Stormcaller. The players race the Stormcaller, whose own quest deck of
stages 2C, 3C and 4C sits in the Stormcaller's area: see
"LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers". The scenario keeps that
deck, places the Stormcaller's progress each round, moves its area to the staging area once
the players reach its stage, and ends the game as the stages say.
-}
module LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller (flightOfTheStormcaller) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Matcher
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (StagingArea, StormcallerArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Window (Window (..), WindowType (RoundEnds))

newtype FlightOfTheStormcaller = FlightOfTheStormcaller ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

flightOfTheStormcaller :: Scenario
flightOfTheStormcaller = Scenario $ scenario FlightOfTheStormcaller "flight-of-the-stormcaller" "Flight of the Stormcaller"

{- | The encounter deck is built from the Flight of the Stormcaller, Vast Oceans, Stormy
Weather and Corsair Pirates sets, the Stormcaller set aside for the Stormcaller's area;
The Dream-chaser's Fleet is prepared by the first stage.
-}
instance RunMessage FlightOfTheStormcaller where
  runMessage msg (FlightOfTheStormcaller attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.FlightOfTheStormcaller, EncounterSet.VastOceans, EncounterSet.StormyWeather, EncounterSet.CorsairPirates]
      let (stormcaller, encounterDeck) = partition (cardMatches $ cardIs Enemies.stormcaller) cards
      firstStage <- genEncounterCard Quests.swiftDeparture
      laterStages <- traverse genEncounterCard [Quests.fullSailAhead, Quests.theWeatherTurnsFoul, Quests.theCapeOfAndrast]
      putQuestIntoPlay firstStage
      pure
        $ FlightOfTheStormcaller
        $ attrs
        & encounterDeckL .~ encounterDeck
        & setAsideL .~ stormcaller
        & questDeckL .~ laterStages
    -- the Stormcaller's area: its quest deck, stage 2C on top, and the Stormcaller
    ScenarioSpecific "stormcaller.prepare" _ -> do
      stages <- traverse genEncounterCard [Quests.fullSailAheadStormcaller, Quests.theWeatherTurnsFoulStormcaller, Quests.theCapeOfAndrastStormcaller]
      for_ attrs.scenarioSetAside \card -> do
        eid <- getRandom
        push $ CreateEnemy eid card
        push $ PlaceEnemy eid StormcallerArea
      pure $ withMeta Meta {stormcallerStage = headMay stages, stormcallerDeck = drop 1 stages, stormcallerProgress = 0} (attrs & setAsideL .~ [])
    -- each of the Stormcaller's stages: Forced: At the end of the round, discard the top
    -- card of the encounter deck. The Stormcaller places progress on this stage equal to its
    -- threat plus the threat of the discarded card.
    CheckWindows ws | isJust meta.stormcallerStage, Window When RoundEnds `elem` ws -> do
      discardTopOfEncounterDeck attrs 1
      FlightOfTheStormcaller <$> runMessage msg attrs
    -- Sahír's Escort in the Stormcaller's area adds its threat too
    DiscardedTopOfEncounterDeck ScenarioSource ScenarioTarget discarded -> do
      stormcallers <- select (EnemyWithCardCode $ toCardCode Enemies.stormcaller)
      escorts <- filter ((== StormcallerArea) . (.placement)) <$> (traverse getEnemy =<< select (EnemyWithCardCode $ toCardCode Enemies.sahirsEscort))
      threat <- sum <$> traverse getEnemyThreat (stormcallers <> map toId escorts)
      placeStormcallerProgress $ threat + sum (mapMaybe (cdThreat . toCardDef) discarded)
      FlightOfTheStormcaller <$> runMessage msg attrs
    ScenarioSpecific "stormcaller.progress" (parseMaybe parseJSON -> Just n) -> case meta.stormcallerStage of
      Just stage
        | meta.stormcallerProgress + n >= fromMaybe 0 (cdQuestPoints $ toCardDef stage) -> case meta.stormcallerDeck of
            next : rest -> do
              push $ ScenarioSpecific "stormcaller.advanced" (toJSON next)
              -- stage 3C: When Revealed: Search the encounter deck and discard pile for 1
              -- copy of Sahír's Escort and add it to the Stormcaller's area.
              when (toCardCode next == toCardCode Quests.theWeatherTurnsFoulStormcaller)
                $ push (ScenarioSpecific "stormcaller.escort" Null)
              pure $ withMeta meta {stormcallerStage = Just next, stormcallerDeck = rest, stormcallerProgress = 0} attrs
            -- stage 4C: If the Stormcaller defeats this stage, Captain Sahír has gotten
            -- away with the key, and the players lose the game.
            [] -> do
              gameOver Lost
              pure $ FlightOfTheStormcaller attrs
      Just _ -> pure $ withMeta meta {stormcallerProgress = meta.stormcallerProgress + n} attrs
      Nothing -> pure $ FlightOfTheStormcaller attrs
    ScenarioSpecific "stormcaller.escort" _ -> do
      let isEscort = cardMatches (cardIs Enemies.sahirsEscort)
          found = headMay (filter isEscort attrs.encounterDeck) <|> headMay (filter isEscort attrs.encounterDiscard)
      for_ found \card -> do
        eid <- getRandom
        push $ CreateEnemy eid card
        addToStormcallerArea (EnemyTarget eid)
      deck <- shuffleM $ filter ((`notElem` map (.id) found) . (.id)) attrs.encounterDeck
      pure $ FlightOfTheStormcaller $ attrs & encounterDeckL .~ deck & encounterDiscardL %~ filter ((`notElem` map (.id) found) . (.id))
    -- the players caught up: the cards in the Stormcaller's area are in the staging area
    ScenarioSpecific "stormcaller.join" _ -> do
      enemies <- getsGame (filter ((== StormcallerArea) . (.placement)) . toList . (.entities.enemies))
      locations <- getsGame (filter ((== StormcallerArea) . (.placement)) . toList . (.entities.locations))
      for_ enemies \e -> push $ PlaceEnemy (toId e) StagingArea
      for_ locations \l -> push $ PlaceLocation (toId l) StagingArea
      pure $ FlightOfTheStormcaller attrs
    AdvanceToQuest card -> do
      attrs' <- runMessage msg attrs
      stormcaller <- getStormcallerStage
      when (stageOf card == stormcaller) $ push (ScenarioSpecific "stormcaller.join" Null)
      pure $ FlightOfTheStormcaller attrs'
    -- stages 2 and 3: When you defeat this stage, if the Stormcaller is at this stage, you
    -- have caught up to the Stormcaller and win the game. Stage 4 is the last.
    AdvanceQuest -> do
      stormcaller <- getStormcallerStage
      let defeated = maybe 4 (subtract 1 . stageOf) (headMay attrs.questDeck)
      if defeated == stormcaller
        then winGame >> pure (FlightOfTheStormcaller attrs)
        else FlightOfTheStormcaller <$> runMessage msg attrs
    _ -> FlightOfTheStormcaller <$> runMessage msg attrs
   where
    meta = getMeta attrs
    withMeta m a = FlightOfTheStormcaller $ a & metaL .~ toJSON m
