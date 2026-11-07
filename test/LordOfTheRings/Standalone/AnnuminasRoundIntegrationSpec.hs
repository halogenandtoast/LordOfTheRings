module LordOfTheRings.Standalone.AnnuminasRoundIntegrationSpec (spec) where

import LordOfTheRings.Ability.Types (isForcedAbility)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Epic.Coordinator
import LordOfTheRings.Epic.Runner (epicRoundPhaseOf)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (getWindowAbilities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getMainQuestCards)
import LordOfTheRings.Modifier (ModifierType (DefenseModifier, ThreatModifier))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas (theSiegeOfAnnuminas)
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioMeta)
import LordOfTheRings.Standalone.AnnuminasCitySpec (stageCity)
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, strongDefender, defendAttack)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

type RoundTeam = (GroupOrdinal, GameEnv)

external :: Message -> GameT ()
external = void . runDebug

teamAt :: Int -> [RoundTeam] -> GameEnv
teamAt ordinal = fromJustNote "missing Epic team" . lookup (GroupOrdinal ordinal)

teamGames :: [RoundTeam] -> IO [Game]
teamGames = traverse (\(_, env) -> runGameT env getGame)

-- Every resumption uses a new process environment, restoring both the queue and
-- the exact RNG, as the event persistence layer does between player answers.
reloadTeams :: [RoundTeam] -> IO [RoundTeam]
reloadTeams teams = for teams $ \(ordinal, env) -> do
  state <- runGameT env getGame
  queue <- runGameT env peekQueue
  generator <- readIORef $ gameRandomGen env
  restored <- either error pure $ eitherDecode $ encode state
  restoredQueue <- either error pure $ eitherDecode $ encode queue
  resumed <- newGameEnv restored (const $ pure ()) 999
  writeIORef (gameRandomGen resumed) generator
  runGameT resumed $ setQueue restoredQueue
  resumedGenerator <- readIORef $ gameRandomGen resumed
  show resumedGenerator `shouldBe` show generator
  encode restored `shouldBe` encode state
  restoredQueue `shouldBe` queue
  pure (ordinal, resumed)

roundStep :: [RoundTeam] -> IO (Maybe EpicRoundStep)
roundStep teams = do
  states <- for teams $ \(ordinal, env) -> runGameT env do
    state <- getGame
    codes <- map toCardCode <$> getMainQuestCards
    pure (ordinal, scenarioEpic (toAttrs state.scenario) >>= epicRoundState, codes, epicRoundBlocked state)
  pure $ nextEpicRoundStep states

freezeTeams :: [RoundTeam] -> IO ()
freezeTeams teams = do
  -- Capture every list before mutating or releasing any team.
  frozen <- for teams $ \(_, env) -> runGameT env $ filter isForcedAbility <$> getWindowAbilities [mkWhen RoundEnds]
  for_ (zip teams frozen) $ \((_, env), abilities) -> runGameT env $ external $ FreezeEpicRoundEnd abilities

-- Drain native outboxes and deliver effects, without choosing for any player.
-- The assertions below inspect physical cards and attacks across this boundary.
flushTeams :: Int -> SharedEventState -> [RoundTeam] -> IO SharedEventState
flushTeams remaining shared teams
  | remaining <= 0 = error "Epic test deliveries did not settle"
  | otherwise = do
      states <- teamGames teams
      let replicas = map (fromJustNote "missing replica" . scenarioEpic . toAttrs . (.scenario)) states
          nextShared = foldl' (\state delta -> fst $ applySharedDelta delta state) shared $ concatMap epicPendingDeltas replicas
          commands = concatMap epicPendingCommands replicas
          boards = [epicBoardFor ordinal "" state | ((ordinal, _), state) <- zip teams states]
      for_ teams $ \(ordinal, env) -> runGameT env do
        state <- getGame
        putGame $ state & scenarioL %~ overAttrs (\a -> a {scenarioEpic = fmap (\r -> r {epicBoards = boards, epicPendingDeltas = [], epicPendingCommands = []}) a.scenarioEpic})
        external $ Run $ SyncEpic nextShared : [ApplyEpicCommand command | command <- commands, command.epicCommandTo == ordinal]
      updated <- teamGames teams
      let updatedBoards = [epicBoardFor ordinal "" state | ((ordinal, _), state) <- zip teams updated]
          hasOutbox state = maybe False (\r -> notNull r.epicPendingDeltas || notNull r.epicPendingCommands) $ scenarioEpic $ toAttrs state.scenario
      if any hasOutbox updated || boards /= updatedBoards
        then flushTeams (remaining - 1) nextShared teams
        else pure nextShared

finishTeamChoices :: Int -> GameT ()
finishTeamChoices remaining = do
  state <- getGame
  when (epicRoundPhaseOf state == Just EpicRoundResolving) $
    case headMay $ mapToList state.question of
      Just (pid, q) | remaining > 0, notNull (questionChoices q) -> do
        let pass = \case Label label _ -> label == "$label.pass"; _ -> False
            options = questionChoices q
        if any pass options
          then chooseLabel "$label.pass" pid
          else chooseOptionMatching "resolve round-end forced effect" pid (const True)
        finishTeamChoices $ remaining - 1
      -- A remote request holds the continuation until its native reply arrives.
      _ -> pure ()

makeTeam :: Int -> IO RoundTeam
makeTeam ordinal = do
  (player, heroes) <- either (error . show) pure =<< loadPlayer ("Team " <> tshow ordinal) testDeck
  env <- newGameEnv (newGameIn theSiegeOfAnnuminas player heroes) (const $ pure ()) (100 + ordinal)
  runGameT env do
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
        replica = EpicReplica (GroupOrdinal ordinal) shared [] [] [] mempty mempty Nothing
        stage = fromJustNote "missing stage" $ atMay [Quests.defendTheCityAnnuminas, Quests.guardTheGateAnnuminas, Quests.manTheWallsAnnuminas] ordinal
    state <- getGame
    putGame $ state & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just replica, scenarioMeta = toJSON (2 :: Int)})
    target <- putQuestStage stage
    run $ Flip target
    resolveRevealed
    _ <- setEncounterDeck $ replicate 5 Locations.oldForestRoad
    beravor <- heroOf player.id "01012"
    run $ CreateEffect $ Effect GameSource beravor [DefenseModifier 1] UntilEndOfRound
    when (ordinal == 0) $ void $ stageCity 50
  pure (GroupOrdinal ordinal, env)

assertRoundEffects :: Bool -> [RoundTeam] -> IO ()
assertRoundEffects active teams = for_ teams $ \(_, env) -> runGameT env do
  pid <- getLead
  beravor <- heroOf pid "01012"
  mods <- getModifiers beravor
  (DefenseModifier 1 `elem` mods) `shouldBe` active

spec :: Spec
spec = describe "Annúminas three-team round integration" do
  it "waits for cross-team attacks, moves each Host one stage, and expires all teams' effects after F" do
    original <- traverse makeTeam [0, 1, 2]
    lieutenant <- runGameT (teamAt 0 original) $ stageEnemy Enemies.lieutenantOfAngmar
    runGameT (teamAt 1 original) $ void $ stageLocation Locations.hillsOfEvendim
    (host, defender) <- runGameT (teamAt 2 original) do
      eid <- stageEnemy Enemies.hostOfAngmar
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [ThreatModifier (-10), DefenseModifier 2] UntilEndOfRound
      pid <- getLead
      defending <- strongDefender pid
      pure (eid, defending)
    let initialShared = fromJustNote "invalid event" $ newAnnuminasEvent 3
    shared <- flushTeams 20 initialShared original
    for_ original $ \(_, env) -> runGameT env $ run EndRound
    waiting <- reloadTeams original
    roundStep waiting `shouldReturn` Just FreezeEpicTeams
    freezeTeams waiting
    ready <- reloadTeams waiting
    map epicRoundPhaseOf <$> teamGames ready `shouldReturn` replicate 3 (Just EpicRoundReady)
    roundStep ready `shouldReturn` Just (ResolveEpicTeam $ GroupOrdinal 0)
    runGameT (teamAt 0 ready) do
      external BeginEpicRoundEnd
      -- The Lieutenant is the first forced effect in the native list.
      finishTeamChoices 20
    sharedBeforeAttack <- flushTeams 20 shared ready
    attacked <- reloadTeams ready
    roundStep attacked `shouldReturn` Nothing
    runGameT (teamAt 0 attacked) do
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundResolving
      state <- getGame
      epicRoundBlocked state `shouldBe` True
    runGameT (teamAt 2 attacked) do
      state <- getGame
      epicRoundBlocked state `shouldBe` True
      pid <- getLead
      defendAttack pid defender
    sharedAfterAttack <- flushTeams 20 sharedBeforeAttack attacked
    resumed <- reloadTeams attacked
    runGameT (teamAt 0 resumed) $ finishTeamChoices 20
    sharedAfterB <- flushTeams 20 sharedAfterAttack resumed
    annuminasCityStrength sharedAfterB `shouldBe` Just 48
    afterB <- reloadTeams resumed
    assertRoundEffects True afterB
    roundStep afterB `shouldReturn` Just (ResolveEpicTeam $ GroupOrdinal 1)
    runGameT (teamAt 1 afterB) $ external BeginEpicRoundEnd >> finishTeamChoices 20
    sharedAfterD <- flushTeams 20 sharedAfterB afterB
    afterD <- reloadTeams afterB
    roundStep afterD `shouldReturn` Just (ResolveEpicTeam $ GroupOrdinal 2)
    assertRoundEffects True afterD
    runGameT (teamAt 2 afterD) $ external BeginEpicRoundEnd >> finishTeamChoices 20
    sharedAfterMove <- flushTeams 20 sharedAfterD afterD
    afterMove <- reloadTeams afterD
    runGameT (teamAt 2 afterMove) $ finishTeamChoices 20
    _ <- flushTeams 20 sharedAfterMove afterMove
    ended <- reloadTeams afterMove
    runGameT (teamAt 0 ended) $ getsGame (map toId . toList . (.entities.enemies)) `shouldReturn` []
    runGameT (teamAt 1 ended) do
      getsGame (map toId . toList . (.entities.enemies)) `shouldReturn` [host]
      getModifiers host >>= (`shouldSatisfy` elem (DefenseModifier 2))
    runGameT (teamAt 2 ended) do
      getsGame (map toId . toList . (.entities.enemies)) `shouldReturn` [lieutenant]
      -- Exactly one destination attack dealt a new shadow card. The incoming
      -- Lieutenant never acquires a second end-of-round trigger at F.
      length <$> getEncounterDeck `shouldReturn` 4
      getDamage defender `shouldReturn` 0
    assertRoundEffects True ended
    roundStep ended `shouldReturn` Just ExpireEpicTeams
    for_ ended $ \(_, env) -> runGameT env $ external FinishEpicRoundEffects
    expired <- reloadTeams ended
    assertRoundEffects False expired
    roundStep expired `shouldReturn` Just BeginEpicTeams
    for_ expired $ \(_, env) -> runGameT env $ external BeginEpicNextRound
    started <- reloadTeams expired
    for_ started $ \(_, env) -> runGameT env do
      getsGame (.round) `shouldReturn` 1
      getsGame gameFrozenRoundEndAbilities `shouldReturn` Nothing
