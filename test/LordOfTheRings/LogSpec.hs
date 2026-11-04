module LordOfTheRings.LogSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend (FrontendMessage (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Log.Entry
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

-- | A test game whose log is kept, newest first.
loggedTest :: (IORef [LogEntry] -> PlayerId -> GameT ()) -> IO ()
loggedTest body = do
  (player, heroes) <- either (error . show) pure =<< loadPlayer "tester" testDeck
  logged <- newIORef []
  env <- newGameEnv (newGame player heroes) (\case Logged e -> modifyIORef' logged (e :); _ -> pure ()) 1
  runGameT env (body logged player.id)

keyOf :: LogEntry -> Maybe Text
keyOf e = case e.logEntryBody of
  LogI18n k _ : _ -> Just k
  _ -> Nothing

entries :: IORef [LogEntry] -> GameT [LogEntry]
entries = fmap reverse . readIORef

spec :: Spec
spec = describe "Game log" do
  it "opens the scenario, the round and its phases with headings" . loggedTest $ \logged pid -> do
    flipFirstQuest pid
    run BeginRound
    headings <- mapMaybe keyOf . filter ((== Structure) . (.logEntryKind)) <$> entries logged
    take 3 headings `shouldBe` ["log.scenarioBegins", "log.round", "log.phase.resource"]

  it "files an enemy's attack under one block, opened by the attack" . loggedTest $ \logged pid -> do
    flipFirstQuest pid
    spider <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
    run $ EngageEnemy pid (toId spider)
    _ <- setEncounterDeck []
    writeIORef logged []
    run $ EnemyAttack pid (toId spider)
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    hero <- getsGame (fromJustNote "no hero" . headMay . toList . (.entities.heroes))
    chooseTarget pid hero
    attack <- entries logged
    case attack of
      header : rest -> do
        keyOf header `shouldBe` Just "log.enemyAttacks"
        (.logGroupRole) <$> header.logEntryGroup `shouldBe` Just GroupHeader
        let gid = (.logGroupId) <$> header.logEntryGroup
        mapMaybe keyOf rest `shouldSatisfy` elem "log.takesDamageFrom"
        all (\e -> ((.logGroupId) <$> e.logEntryGroup) == gid) rest `shouldBe` True
      [] -> liftIO (expectationFailure "nothing logged")

  it "names the cards drawn to their player alone, and counts them for everyone else" . loggedTest $ \logged pid -> do
    run $ DrawCards (Drawing pid 2)
    drawn <- entries logged
    map (.logEntryAudience) drawn `shouldBe` [OnlyPlayer pid, ExceptPlayer pid]
    case drawn of
      LogEntry {logEntryBody = [LogI18n _ vars]} : _ -> (Map.lookup "cards" vars <&> \case LogList cs -> length cs; _ -> 0) `shouldBe` Just 2
      _ -> liftIO (expectationFailure "no draw logged")

  it "says nothing is drawn when Iron Shackles is discarded instead" . loggedTest $ \logged pid -> do
    _ <- setEncounterDeck [Treacheries.ironShackles]
    run RevealEncounterCard
    resolveRevealed
    writeIORef logged []
    run $ DrawCards (Drawing pid 1)
    logKeys <- mapMaybe keyOf <$> entries logged
    logKeys `shouldSatisfy` all (`notElem` ["log.drawsCards", "log.drawsCount"])

  it "says a location immune to the card keeps its progress, as East-gate against Cave In" . loggedTest $ \logged _ -> do
    _ <- putQuestStage Quests.enteringTheMines
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.eastGate
    gate <- getsGame (fromJustNote "no East-gate" . headMay . toList . (.entities.locations))
    run $ PlaceTokens GameSource (toTarget gate) Progress 4
    _ <- setEncounterDeck [Treacheries.caveIn, Enemies.goblinScout]
    writeIORef logged []
    run RevealEncounterCard
    resolveRevealed
    said <- mapMaybe keyOf <$> entries logged
    said `shouldSatisfy` elem "log.immuneTo"
    said `shouldSatisfy` notElem "log.progressRemoved"

  it "says Closing In goes underneath The Orcs' Search, not that it is removed from the game" . loggedTest $ \logged _ -> do
    run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.theOrcsSearch
    _ <- setEncounterDeck [Treacheries.closingIn, Locations.forestGate]
    writeIORef logged []
    run RevealEncounterCard
    resolveRevealed
    said <- mapMaybe keyOf <$> entries logged
    said `shouldSatisfy` elem "log.placedUnderneath"
    said `shouldSatisfy` notElem "log.removedFromGame"
