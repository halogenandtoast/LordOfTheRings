module LordOfTheRings.Standalone.AnnuminasRoundEndSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Ability.Types (AbilityLimitType (PerRound), UsedAbility (..), isForcedAbility)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Runner (epicRoundPhaseOf)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (getWindowAbilities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (locationTokens)
import LordOfTheRings.Log.Narrator (narrationFor)
import LordOfTheRings.Modifier (ModifierType (DefenseModifier))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Standalone.AnnuminasCitySpec (stageCity, setReplica)
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

external :: Message -> GameT ()
external = void . runDebug

freezeRound :: GameT ()
freezeRound = do
  abilities <- filter isForcedAbility <$> getWindowAbilities [mkWhen RoundEnds]
  external $ FreezeEpicRoundEnd abilities

finishForcedChoices :: Int -> GameT ()
finishForcedChoices remaining = do
  phase <- getsGame epicRoundPhaseOf
  when (phase == Just EpicRoundResolving) do
    questions <- getsGame (.question)
    case headMay (mapToList questions) of
      Just (pid, q) | remaining > 0, notNull (questionChoices q) -> do
        chooseOptionMatching "resolve frozen round-end triggers" pid (const True)
        finishForcedChoices (remaining - 1)
      _ -> error "frozen round-end effects did not settle"

ramResources :: LocationId -> GameT Int
ramResources lid = countTokens Resource . locationTokens . toAttrs <$> getLocation lid

spec :: Spec
spec = describe "Annúminas synchronized round boundary" do
  it "holds raw card cleanup and per-round uses until this team's effects are released" . gameTest $ \pid -> do
    setReplica []
    _ <- putAllyIntoPlay pid Allies.gandalf
    chooseLabel "$label.pass" pid
    beravor <- heroOf pid "01012"
    let used = UsedAbility (case beravor of HeroTarget hid -> HeroSource hid; _ -> error "expected Beravor") 1 PerRound 1
    current <- getGame
    putGame $ current & playersL . ix pid %~ (\player -> player {playerUsedAbilities = [used]})
    run EndRound
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    (.usedAbilities) <$> getPlayer pid `shouldReturn` [used]
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundWaiting
    getsGame (.round) `shouldReturn` 0
    maybe [] questionChoices <$> getQuestion pid `shouldReturn` []
    external $ FreezeEpicRoundEnd []
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    external BeginEpicRoundEnd
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    (.usedAbilities) <$> getPlayer pid `shouldReturn` []
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundEnded
    getsGame (.round) `shouldReturn` 0

  it "persists a frozen Ready snapshot without resolving its City or Ram triggers" . gameTest $ \_ -> do
    setReplica []
    _ <- stageCity 50
    ram <- stageLocation Locations.batteringRam
    run EndRound
    freezeRound
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundReady
    getsGame (fmap length . gameFrozenRoundEndAbilities) `shouldReturn` Just 2
    ramResources ram `shouldReturn` 0
    current <- getGame
    queue <- peekQueue
    restored <- either error pure $ eitherDecode $ encode current
    resumed <- liftIO $ newGameEnv restored (const $ pure ()) 1
    liftIO $ runGameT resumed do
      setQueue queue
      external BeginEpicRoundEnd
      finishForcedChoices 10
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundEnded
      ramResources ram `shouldReturn` 1
      getsGame (.round) `shouldReturn` 0

  it "does not discover a new forced trigger after an explicitly empty frozen snapshot" . gameTest $ \_ -> do
    setReplica []
    run EndRound
    external $ FreezeEpicRoundEnd []
    ram <- genEncounterCard Locations.batteringRam
    external $ AddToStaging ram
    lid <- getsGame $ toId . fromJustNote "Ram missing" . headMay . toList . (.entities.locations)
    external BeginEpicRoundEnd
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundEnded
    ramResources lid `shouldReturn` 0

  it "keeps lasting effects through Ended, expires them separately, then starts the next round" . gameTest $ \pid -> do
    setReplica []
    beravor <- heroOf pid "01012"
    run $ CreateEffect $ Effect GameSource beravor [DefenseModifier 2] UntilEndOfRound
    run EndRound
    external $ FreezeEpicRoundEnd []
    external BeginEpicRoundEnd
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundEnded
    getModifiers beravor >>= (`shouldSatisfy` elem (DefenseModifier 2))
    external $ EndEffects UntilEndOfRound
    getModifiers beravor >>= (`shouldSatisfy` elem (DefenseModifier 2))
    external FinishEpicRoundEffects
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundExpired
    getModifiers beravor >>= (`shouldSatisfy` notElem (DefenseModifier 2))
    getsGame (.round) `shouldReturn` 0
    entries <- maybe (pure []) id $ narrationFor BeginRound
    length entries `shouldBe` 0
    getsGame gameFrozenRoundEndAbilities `shouldReturn` Just []
    external BeginEpicNextRound
    getsGame (.round) `shouldReturn` 1
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundIdle
    getsGame gameFrozenRoundEndAbilities `shouldReturn` Nothing

  it "ignores duplicate or out-of-order coordinator releases" . gameTest $ \_ -> do
    setReplica []
    run EndRound
    external BeginEpicRoundEnd
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundWaiting
    freezeRound
    external BeginEpicNextRound
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundReady
    external BeginEpicRoundEnd
    external BeginEpicRoundEnd
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundEnded
    external FinishEpicRoundEffects
    external FinishEpicRoundEffects
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundExpired
    getsGame (.round) `shouldReturn` 0

  it "does not publish the private frozen ability records" . gameTest $ \_ -> do
    setReplica []
    _ <- stageCity 50
    run EndRound
    freezeRound
    current <- getGame
    case publicGame current of
      Object fields -> KeyMap.lookup "frozenRoundEndAbilities" fields `shouldBe` Nothing
      _ -> error "invalid public game"

  it "reads older game and replica snapshots with no round coordinator fields" . gameTest $ \_ -> do
    setReplica []
    current <- getGame
    let legacyGame = case toJSON current of Object fields -> Object $ KeyMap.delete "frozenRoundEndAbilities" fields; value -> value
    restored <- either error pure $ eitherDecode $ encode legacyGame
    gameFrozenRoundEndAbilities restored `shouldBe` Nothing
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    let legacyReplica = case toJSON replica of Object fields -> Object $ KeyMap.delete "epicRoundState" fields; value -> value
    decoded <- either error pure $ eitherDecode $ encode legacyReplica
    epicRoundState decoded `shouldBe` Nothing

  for_ [False, True] $ \epicDreadnaught ->
    it ("retains ordinary round flow outside synchronized events: " <> show epicDreadnaught) . gameTest $ \pid -> do
      when epicDreadnaught do
        let shared = fromJustNote "invalid Dreadnaught event" $ newSharedEvent 3
        current <- getGame
        putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 0) shared [] [] [] mempty mempty Nothing})
      beravor <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource beravor [DefenseModifier 2] UntilEndOfRound
      run EndRound
      getsGame (.round) `shouldReturn` 1
      getsGame epicRoundPhaseOf `shouldReturn` Nothing
      getModifiers beravor >>= (`shouldSatisfy` notElem (DefenseModifier 2))

  it "does not retrigger a physically transported Lieutenant at a team that already froze its triggers" do
    packetRef <- newIORef Nothing
    gameTest $ \_ -> do
      let remoteStage = EpicStage 0 [EpicQuest (toCardCode Quests.stopTheAttack) 3 0 Nothing True] [] False 0 False
      setReplica [EpicBoard (GroupOrdinal 1) "Walls" [] False (Just remoteStage)]
      _ <- stageEnemy Enemies.lieutenantOfAngmar
      run EndRound
      freezeRound
      external BeginEpicRoundEnd
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundResolving
      Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      case replica.epicPendingCommands of
        [command] -> liftIO $ writeIORef packetRef $ Just command
        _ -> error "Lieutenant did not move to the other stage"
    command <- fromJustNote "missing transport command" <$> readIORef packetRef
    gameTest $ \_ -> do
      setReplica []
      current <- getGame
      putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = fmap (\r -> r {epicGroup = GroupOrdinal 1}) a.scenarioEpic})
      run EndRound
      external $ FreezeEpicRoundEnd []
      let transfer = case command.epicCommandEffect of EpicResolveWithReply _ _ (EpicReceiveEnemy payload _) -> payload; _ -> error "missing enemy payload"
      -- The movement itself retains the enemy; arrival attacks are covered by
      -- the Lieutenant tests. Here only the end-of-round trigger is under test.
      external $ ApplyEpicCommand command {epicCommandEffect = EpicReceiveEnemy transfer EpicStageEnemy}
      getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [transfer.epicEnemyCard.id]
      external BeginEpicRoundEnd
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundEnded
      Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      replica.epicPendingCommands `shouldBe` []
      getsGame (length . (.entities.enemies)) `shouldReturn` 1
