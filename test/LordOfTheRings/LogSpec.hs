module LordOfTheRings.LogSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Frontend (FrontendMessage (..))
import LordOfTheRings.Log.Entry
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
