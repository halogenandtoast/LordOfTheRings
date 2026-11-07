module LordOfTheRings.Standalone.AnnuminasManWallsSpec (spec) where

import Data.Set qualified as Set
import Data.UUID qualified as UUID
import System.Random (mkStdGen)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioSetAside)
import LordOfTheRings.Standalone.AnnuminasCitySpec qualified as City
import LordOfTheRings.Window (WindowType (RoundBegins, RoundEnds), mkWhen)
import LordOfTheRings.EpicTestHelpers (commandEffect, acknowledgeEpicCommand)
import TestImport

wall :: GameT Target
wall = do
  target <- putQuestStage Quests.manTheWallsAnnuminas
  run $ Flip target
  resolveRevealed
  pure target

replicaFor :: Int -> [EpicBoard] -> GameT ()
replicaFor ordinal boards = do
  generator <- asks gameRandomGen
  liftIO $ writeIORef generator $ mkStdGen (100 + ordinal)
  let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal ordinal) shared [] boards [] mempty mempty Nothing})

boardAt :: Int -> Text -> Int -> Bool -> EpicBoard
boardAt ordinal name hosts drawable = EpicBoard (GroupOrdinal ordinal) name [] False $ Just $
  EpicStage 0 [EpicQuest (toCardCode Quests.manTheWallsAnnuminas) 2 0 (Just 50) True] [] False hosts drawable

commands :: GameT [EpicCommand]
commands = getsGame $ maybe [] epicPendingCommands . scenarioEpic . toAttrs . (.scenario)

spec :: Spec
spec = describe "Annúminas Man the Walls" do
  it "adds a local set-aside Host at round start, retaining its physical identity" . gameTest $ \_ -> do
    _ <- wall
    host <- genEncounterCard Enemies.hostOfAngmar
    run $ SetAside host
    run $ CheckWindows [mkWhen RoundBegins]
    getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [host.id]
    getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` []
    getEncounterDeck `shouldReturn` []

  it "does nothing if no stage has a set-aside Host" . gameTest $ \pid -> do
    replicaFor 2 [boardAt 0 "City" 0 True, boardAt 1 "Gate" 0 True]
    _ <- wall
    run $ CheckWindows [mkWhen RoundBegins]
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getQuestion pid `shouldReturn` Nothing
    commands `shouldReturn` []

  it "offers every stage with a Host, and preserves the source-stage choice through reload" . gameTest $ \pid -> do
    replicaFor 2 [boardAt 0 "City" 1 True, boardAt 1 "Gate" 0 True]
    _ <- wall
    run . SetAside =<< genEncounterCard Enemies.hostOfAngmar
    run $ CheckWindows [mkWhen RoundBegins]
    length . maybe [] questionChoices <$> getQuestion pid `shouldReturn` 2
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    chooseOptionMatching "choose City's Host" pid $ \case Label label _ -> "City" `isInfixOf` label; _ -> False
    sent <- commands
    map (.epicCommandTo) sent `shouldBe` [GroupOrdinal 0]
    length . maybe [] questionChoices <$> getQuestion pid `shouldReturn` 0
    getsGame (length . (.entities.enemies)) `shouldReturn` 0

  it "waits for a remote Host before resource collection and persists both sides of the transfer" do
    savedOrigin <- newIORef Nothing
    savedReply <- newIORef Nothing
    gameTest $ \_ -> do
      replicaFor 2 [boardAt 0 "City" 1 True]
      _ <- wall
      run BeginRound
      getsGame (map (.resources) . toList . (.entities.heroes)) `shouldReturn` [0, 0, 0]
      [request] <- commands
      current <- getGame
      queue <- peekQueue
      restored <- either error pure $ eitherDecode $ encode current
      restoredQueue <- either error pure $ eitherDecode $ encode queue
      liftIO $ writeIORef savedOrigin $ Just (restored, restoredQueue, request)
    (_, _, request) <- fromJustNote "missing request" <$> readIORef savedOrigin
    gameTest $ \_ -> do
      replicaFor 0 []
      host <- genEncounterCard Enemies.hostOfAngmar
      run $ SetAside host
      run $ ApplyEpicCommand request
      getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` []
      [reply] <- commands
      current <- getGame
      restored <- either error pure $ eitherDecode $ encode current
      putGame =<< preloadModifiers restored
      run $ ApplyEpicCommand request
      [persistedReply] <- commands
      persistedReply `shouldBe` reply
      liftIO $ writeIORef savedReply $ Just (persistedReply, host.id)
    (origin, queue, _) <- fromJustNote "missing origin" <$> readIORef savedOrigin
    (reply, cid) <- fromJustNote "missing reply" <$> readIORef savedReply
    env <- newGameEnv origin (const $ pure ()) 300
    runGameT env do
      setQueue queue
      void $ runDebug $ ApplyEpicCommand reply
      getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [cid]
      getsGame (map (.resources) . toList . (.entities.heroes)) `shouldReturn` [1, 1, 1]
      Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      replica.epicPendingRequests `shouldBe` Set.empty
      void $ runDebug $ ApplyEpicCommand reply
      getsGame (length . (.entities.enemies)) `shouldReturn` 1
      void $ runDebug $ ApplyEpicCommand (reply {epicCommandId = UUID.nil})
      getsGame (length . (.entities.enemies)) `shouldReturn` 1

  it "acknowledges an exhausted remote supply and resumes the waiting game" do
    saved <- newIORef Nothing
    gameTest $ \_ -> do
      replicaFor 2 [boardAt 0 "City" 1 True]
      _ <- wall
      run $ CheckWindows [mkWhen RoundBegins]
      [request] <- commands
      current <- getGame
      queue <- peekQueue
      liftIO $ writeIORef saved $ Just (current, queue, request)
    (origin, queue, request) <- fromJustNote "missing request" <$> readIORef saved
    replyRef <- newIORef Nothing
    gameTest $ \_ -> do
      replicaFor 0 []
      run $ ApplyEpicCommand request
      [reply] <- commands
      liftIO $ writeIORef replyRef $ Just reply
    reply <- fromJustNote "missing empty reply" <$> readIORef replyRef
    env <- newGameEnv origin (const $ pure ()) 301
    runGameT env do
      setQueue queue
      void $ runDebug $ ApplyEpicCommand reply
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      replica.epicPendingRequests `shouldBe` Set.empty

  it "draws one card per player at the selected local stage" . twoPlayerTest testDeck $ \one two -> do
    target <- wall
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf one target
    chooseLabel "$label.epic.thisGroup" one
    length . (.hand) <$> getPlayer one `shouldReturn` 1
    length . (.hand) <$> getPlayer two `shouldReturn` 1

  it "respects encounter-card drawing restrictions while permitting player-card-only restrictions" . twoPlayerTest testDeck $ \one two -> do
    target <- wall
    run $ CreateEffect $ Effect GameSource (PlayerTarget one) [CannotDrawCardsFromCardEffects] UntilEndOfRound
    run $ CreateEffect $ Effect GameSource (PlayerTarget two) [CannotDrawCardsFromPlayerCardEffects] UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf one target
    chooseLabel "$label.epic.thisGroup" one
    length . (.hand) <$> getPlayer one `shouldReturn` 0
    length . (.hand) <$> getPlayer two `shouldReturn` 1

  it "does not offer its draw response when no stage can draw" . gameTest $ \pid -> do
    target <- wall
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCards] UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False

  it "queues a remote draw and delivers it exactly once after reload" . gameTest $ \pid -> do
    City.setReplica [boardAt 1 "Other" 0 True]
    target <- wall
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    chooseOptionMatching "choose Other" pid $ \case Label label _ -> "Other" `isInfixOf` label; _ -> False
    sent <- commands
    map commandEffect sent `shouldBe` [EpicDrawCards]
    length . (.hand) <$> getPlayer pid `shouldReturn` 0
    traverse_ acknowledgeEpicCommand sent
    let delivered = EpicCommand UUID.nil (GroupOrdinal 1) (GroupOrdinal 0) (toCardCode Quests.manTheWallsAnnuminas) EpicDrawCards ScenarioTarget
    run $ ApplyEpicCommand delivered
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    run $ ApplyEpicCommand delivered
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "uses the shared stage-two arrival and modified Host requirement" . gameTest $ \_ -> do
    _ <- setEncounterDeck $ replicate 3 Enemies.wargOfCarnDum
    _ <- wall
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    length <$> getEncounterDeck `shouldReturn` 2
    run . SetAside =<< genEncounterCard Enemies.hostOfAngmar
    run $ CheckWindows [mkWhen RoundBegins]
    hosts <- getsGame (filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) . toList . (.entities.enemies))
    run $ AddToVictoryDisplay $ EnemyTarget $ toId $ fromJustNote "missing Host" $ headMay hosts
    (getQuestPoints . toAttrs =<< getActiveQuest) `shouldReturn` 40

  it "does not offer a draw when every eligible player's deck is empty" . gameTest $ \pid -> do
    target <- wall
    current <- getGame
    putGame $ current & playersL . ix pid . deckL .~ []
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False
