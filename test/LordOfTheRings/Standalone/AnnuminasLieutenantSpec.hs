module LordOfTheRings.Standalone.AnnuminasLieutenantSpec (spec) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioVictoryDisplay))
import LordOfTheRings.Standalone.AnnuminasInvadersSpec (mainStage)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, strongDefender, defendAttack, passAttackActions)
import LordOfTheRings.Trait (Trait (Easterling, Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import System.Random (mkStdGen)
import TestImport

setReplica :: Int -> [EpicBoard] -> GameT ()
setReplica ordinal boards = do
  let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal ordinal) shared [] boards [] mempty mempty})
  -- Real Epic games start with independent seeds. Keep independent fixtures
  -- independent too, including their physical encounter-card identities.
  generator <- asks gameRandomGen
  liftIO $ writeIORef generator $ mkStdGen (100 + ordinal)

boardAt :: Int -> Text -> Int -> EpicBoard
boardAt ordinal name threat = EpicBoard (GroupOrdinal ordinal) name [] False (Just $ EpicStage threat [] [] False 0 False [])

pendingCommands :: GameT [EpicCommand]
pendingCommands = getsGame $ maybe [] epicPendingCommands . scenarioEpic . toAttrs . (.scenario)

departingLieutenant :: IO EpicCommand
departingLieutenant = do
  result <- newIORef Nothing
  gameTest $ \_ -> do
    setReplica 0 [boardAt 1 "Gate" 1, boardAt 2 "Walls" 7]
    _ <- mainStage 10
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    run $ DealDamage GameSource (EnemyTarget eid) 2
    run $ PlaceTokens GameSource (EnemyTarget eid) Resource 3
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [DefenseModifier 1] UntilEndOfRound
    shadow <- setEncounterDeck [Locations.oldForestRoad]
    run $ DealShadowCard eid
    run $ CheckWindows [mkWhen RoundEnds]
    commands <- pendingCommands
    length commands `shouldBe` 1
    getsGame (Map.member eid . (.entities.enemies)) `shouldReturn` False
    getsGame (filter ((== EnemyTarget eid) . effectTarget) . gameEffects) `shouldReturn` []
    getEncounterDiscard `shouldReturn` []
    current <- getGame
    for_ shadow $ \card -> show (encode $ publicGame current) `shouldSatisfy` (not . List.isInfixOf (show card.id))
    let command = fromJustNote "missing transfer command" $ headMay commands
    command.epicCommandTo `shouldBe` GroupOrdinal 1
    case command.epicCommandEffect of
      EpicReceiveEnemy transfer EpicAttackFirstPlayer -> do
        transfer.epicEnemyId `shouldBe` eid
        transfer.epicEnemyTokens `shouldBe` mapFromList [(Damage, 2), (Resource, 3)]
        transfer.epicEnemyShadows `shouldBe` shadow
      _ -> error "wrong arrival effect"
    liftIO $ writeIORef result $ Just command
  fromJustNote "source did not emit a command" <$> readIORef result

spec :: Spec
spec = describe "Annúminas Lieutenant of Angmar" do
  it "requires ten progress on the main quest, ignores side-quest progress, and remains immune to player effects" . gameTest $ \pid -> do
    stage <- mainStage 9
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    side <- genPlayerCard Quests.gatherInformation (Just pid)
    run $ PutSideQuestIntoPlay pid side
    sideId <- getsGame $ toId . fromJustNote "missing side quest" . find ((== toCardCode Quests.gatherInformation) . toCardCode) . toList . (.entities.quests)
    run $ PlaceTokens GameSource (QuestTarget sideId) Progress 3
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 0
    run $ PlaceTokens GameSource stage Progress 1
    getModifiers eid >>= (`shouldSatisfy` notElem CannotTakeDamage)
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1
    hero <- heroOf pid "01012"
    run $ DealDamage (case hero of HeroTarget hid -> HeroSource hid; _ -> error "expected hero") (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1
    run $ RemoveTokens GameSource stage Progress 1
    getModifiers eid >>= (`shouldSatisfy` elem CannotTakeDamage)

  it "loses both printed damage restrictions while blank" . gameTest $ \pid -> do
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    getModifiers eid >>= (`shouldSatisfy` notElem CannotTakeDamage)
    getModifiers eid >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)
    hero <- heroOf pid "01012"
    run $ DealDamage (case hero of HeroTarget hid -> HeroSource hid; _ -> error "expected hero") (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1
    run $ CheckWindows [mkWhen RoundEnds]
    getQuestion pid `shouldReturn` Nothing

  for_ [False, True] $ \rotated ->
    it ("returns from engagement to staging and attacks the first player with a fresh shadow in standard play, rotated: " <> show rotated) . twoPlayerTest testDeck $ \one two -> do
      let lead = if rotated then two else one
          other = if rotated then one else two
      when rotated $ run $ SetFirstPlayer two
      eid <- stageEnemy Enemies.lieutenantOfAngmar
      run $ EngageEnemy other eid
      defender <- strongDefender lead
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ CheckWindows [mkWhen RoundEnds]
      defendAttack lead defender
      getDamage defender `shouldReturn` 0
      (.placement) <$> getEnemy eid `shouldReturn` StagingArea
      length <$> getEncounterDeck `shouldReturn` 1
      (.shadowCards) <$> getEnemy eid `shouldReturn` []

  it "offers only staging areas tied for the lowest total threat and preserves a pending choice through reload" . gameTest $ \pid -> do
    setReplica 0 [boardAt 1 "Gate" 1, boardAt 2 "Walls" 1]
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    run $ CheckWindows [mkWhen RoundEnds]
    choices <- maybe [] questionChoices <$> getQuestion pid
    length choices `shouldBe` 2
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    chooseOptionMatching "choose Walls" pid $ \case Label label _ -> "Walls" `isInfixOf` label; _ -> False
    map (.epicCommandTo) <$> pendingCommands `shouldReturn` [GroupOrdinal 2]
    getsGame (Map.member eid . (.entities.enemies)) `shouldReturn` False

  it "lets the first player choose the current stage when it is tied for the minimum" . gameTest $ \pid -> do
    setReplica 0 [boardAt 1 "Gate" 4, boardAt 2 "Walls" 8]
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    defender <- strongDefender pid
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run $ CheckWindows [mkWhen RoundEnds]
    chooseLabel "$label.epic.thisGroup" pid
    defendAttack pid defender
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
    pendingCommands `shouldReturn` []

  it "uses the current local threat rather than its stale replica snapshot" . gameTest $ \pid -> do
    setReplica 0 [boardAt 0 "City" 99, boardAt 1 "Gate" 2, boardAt 2 "Walls" 3]
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    run $ EngageEnemy pid eid
    defender <- strongDefender pid
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ CheckWindows [mkWhen RoundEnds]
    defendAttack pid defender
    pendingCommands `shouldReturn` []
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  for_ [9, 10] $ \progress ->
    it ("preserves identity, tokens, shadows and lasting effects while recomputing the destination's damage condition at progress " <> show progress) do
      command <- departingLieutenant
      gameTest $ \_ -> do
        setReplica 1 []
        _ <- mainStage progress
        let transfer = case command.epicCommandEffect of EpicReceiveEnemy packet _ -> packet; _ -> error "missing transfer"
            staged = command {epicCommandEffect = EpicReceiveEnemy transfer EpicStageEnemy}
            received = either error id $ eitherDecode $ encode staged
        run $ ApplyEpicCommand received
        foe <- getEnemy transfer.epicEnemyId
        (toAttrs foe).cardId `shouldBe` transfer.epicEnemyCard.id
        enemyTokens (toAttrs foe) `shouldBe` transfer.epicEnemyTokens
        foe.shadowCards `shouldBe` transfer.epicEnemyShadows
        foe.placement `shouldBe` StagingArea
        getEnemyDefense transfer.epicEnemyId `shouldReturn` 5
        modifiers <- getModifiers transfer.epicEnemyId
        (CannotTakeDamage `elem` modifiers) `shouldBe` (progress < 10)
        current <- getGame
        restored <- either error pure $ eitherDecode $ encode current
        putGame =<< preloadModifiers restored
        run $ ApplyEpicCommand received
        getsGame (length . (.entities.enemies)) `shouldReturn` 1
        getEnemyDefense transfer.epicEnemyId `shouldReturn` 5

  it "attacks the destination's first player after arrival and deals a shadow from that team's deck" do
    command <- departingLieutenant
    twoPlayerTest testDeck $ \one two -> do
      setReplica 1 []
      run $ SetFirstPlayer two
      defender <- strongDefender two
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ ApplyEpicCommand command
      defendAttack two defender
      resolveShadowsInOrder two
      passAttackActions
      getQuestion one `shouldReturn` Nothing
      getDamage defender `shouldReturn` 0
      length <$> getEncounterDeck `shouldReturn` 1
      length <$> getEncounterDiscard `shouldReturn` 2
      let eid = case command.epicCommandEffect of EpicReceiveEnemy transfer _ -> transfer.epicEnemyId; _ -> error "missing transfer"
      (.placement) <$> getEnemy eid `shouldReturn` StagingArea
      run $ ApplyEpicCommand command
      length <$> getEncounterDeck `shouldReturn` 1

  it "does not fire entering-play effects when the enemy arrives, while ordinary arrivals still fire them" do
    command <- departingLieutenant
    gameTest $ \_ -> do
      setReplica 1 []
      _ <- putQuestStage Quests.warriorsOfTheEast
      horse <- genEncounterCard Treacheries.easterlingHorse
      run $ AddToEncounterDiscard [horse]
      let transfer = case command.epicCommandEffect of EpicReceiveEnemy packet _ -> packet; _ -> error "missing transfer"
          tagged = transfer {epicEnemyEffects = transfer.epicEnemyEffects <> [Effect GameSource (EnemyTarget transfer.epicEnemyId) [GainTrait Easterling] UntilEndOfRound]}
          staged = command {epicCommandEffect = EpicReceiveEnemy tagged EpicStageEnemy}
      run $ ApplyEpicCommand staged
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0
      map (.id) <$> getEncounterDiscard `shouldReturn` [horse.id]
      _ <- stageEnemy Enemies.riderOfRhun
      getsGame (length . (.entities.treacheries)) `shouldReturn` 1
      getEncounterDiscard `shouldReturn` []

  it "does not fire leaving-play effects when moving to another team, while a real departure still fires them" . gameTest $ \_ -> do
    setReplica 0 [boardAt 1 "Gate" 1]
    stage <- putQuestStage Quests.theRingwraithsAttack
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [GainTrait Nazgul] UntilEndOfRound
    run $ Flip stage
    run $ MoveAnnuminasEnemy eid (GroupOrdinal 1) EpicStageEnemy
    getsGame (.outcome) `shouldReturn` Nothing
    rider <- stageEnemy Enemies.blackRider
    run $ Discard GameSource (EnemyTarget rider)
    getsGame (.outcome) `shouldReturn` Just Won

  it "contributes four victory points when its nine hit points are defeated" . gameTest $ \_ -> do
    _ <- mainStage 10
    eid <- stageEnemy Enemies.lieutenantOfAngmar
    run $ DealDamage GameSource (EnemyTarget eid) 9
    getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.lieutenantOfAngmar]
    getsGame ((.victoryPoints) . toAttrs . (.scenario)) `shouldReturn` 4
