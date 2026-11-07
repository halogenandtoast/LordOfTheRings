{- | Epic persistence. Every writer acquires the event row before reading a group
game. Its engine state, shared deltas, and delivery outbox commit together.
-}
module Epic (module Epic) where

import Data.Aeson (Result (..))
import Data.Set qualified as Set
import Database.Persist.Postgresql
import GameLog (storeLog)
import LordOfTheRings.Classes.Entity (overAttrs, toAttrs)
import LordOfTheRings.Classes.HasGame (getGame, runReadGame)
import LordOfTheRings.Ability (isForcedAbility)
import LordOfTheRings.Helpers.Window (getWindowAbilities)
import LordOfTheRings.Window (WindowType (RoundEnds, RoundBegins, DolGuldurStageEffectsResolved, DolGuldurSiegeResolved), mkWhen)
import LordOfTheRings.Classes.HasQueue (peekQueue, setQueue)
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Epic.DolGuldur qualified as Dol
import LordOfTheRings.Epic.DolGuldur.Commands qualified as DolCommands
import LordOfTheRings.Epic.DolGuldur.Runtime
import LordOfTheRings.Card (CardCode, toCardCode)
import LordOfTheRings.Epic.Coordinator
import LordOfTheRings.Epic.Types
import LordOfTheRings.Frontend (FrontendMessage)
import LordOfTheRings.Game
import LordOfTheRings.GameT
import LordOfTheRings.Message
import LordOfTheRings.Target (Target (ScenarioTarget))
import LordOfTheRings.Prelude hiding (delete)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioQuestDeck))
import Model
import System.Random.Internal (StdGen (..))
import System.Random.SplitMix (seedSMGen', unseedSMGen)
import Text.Read (readMaybe)

-- A snapshot carries its own queue and random generator. Groups never share an
-- encounter deck, question, player hand, or RNG.
data EpicGame = EpicGame
  { epicGameGroup :: LotrEpicGroup
  , epicGameRow :: LotrGame
  , epicGameState :: Game
  , epicGameQueue :: [Message]
  , epicGameGenerator :: StdGen
  , epicGameSent :: [FrontendMessage]
  }

parseStored :: (MonadIO m, FromJSON a) => Text -> Value -> m a
parseStored label value = case fromJSON value of
  Success result -> pure result
  Error message -> liftIO $ throwIO $ userError $ unpack label <> ": " <> message

parseGenerator :: MonadIO m => Text -> m StdGen
parseGenerator value =
  maybe
    (liftIO $ throwIO $ userError "Invalid stored Epic random generator")
    (pure . StdGen . seedSMGen')
    (readMaybe $ unpack value)

showGenerator :: StdGen -> Text
showGenerator = tshow . unseedSMGen . unStdGen

-- Lock order is always event, then its games. Callers must retain this lock
-- through engine execution and persistence, including undo and lobby changes.
lockEvent :: MonadIO m => LotrEpicEventId -> ReaderT SqlBackend m LotrEpicEvent
lockEvent eid = do
  rows <- rawSql "SELECT ?? FROM lotr_epic_events WHERE id = ? FOR UPDATE" [toPersistValue eid]
  case rows of
    [Entity _ event] -> pure event
    _ -> liftIO $ throwIO $ userError "Epic event no longer exists"

loadGames :: MonadIO m => LotrEpicEventId -> ReaderT SqlBackend m [EpicGame]
loadGames eid = do
  groups <- selectList [LotrEpicGroupEventId ==. eid] [Asc LotrEpicGroupOrdinal]
  for groups \(Entity _ groupRow) -> do
    row <- getJust groupRow.lotrEpicGroupGameId
    state <- parseStored "Epic group game" row.lotrGameCurrentData
    queue <- parseStored "Epic group queue" row.lotrGameCurrentQueue
    generator <- parseGenerator row.lotrGameRandomGen
    pure $ EpicGame groupRow row state queue generator []

replicaOf :: EpicGame -> Maybe EpicReplica
replicaOf = scenarioEpic . toAttrs . (.scenario) . epicGameState

setReplica :: EpicReplica -> EpicGame -> EpicGame
setReplica replica snapshot =
  snapshot
    { epicGameState =
        snapshot.epicGameState & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just replica})
    }

ordinalOf :: EpicGame -> GroupOrdinal
ordinalOf = GroupOrdinal . lotrEpicGroupOrdinal . epicGameGroup

gameIdOf :: EpicGame -> LotrGameId
gameIdOf = lotrEpicGroupGameId . epicGameGroup

raiseUndoFloor :: MonadIO m => LotrGameId -> Int -> ReaderT SqlBackend m ()
raiseUndoFloor gid step =
  getBy (UniqueEpicUndoFloor gid) >>= \case
    Nothing -> insert_ $ LotrEpicUndoFloor gid step
    Just (Entity key floor') -> when (floor'.lotrEpicUndoFloorStep < step) $ update key [LotrEpicUndoFloorStep =. step]

{- | Apply accepted remote effects before restoring the old question, rebuilding
action/response windows with the new board. The queued remainder is durable
even when an effect opens a new question before later commands can resolve.
-}
runExternal
  :: MonadIO m => SharedEventState -> [EpicBoard] -> [EpicCommand] -> EpicGame -> m EpicGame
runExternal shared boards commands snapshot = do
  let old = fromJustNote "Epic group has no replica" $ replicaOf snapshot
      replica = old {epicBoards = boards, epicPendingDeltas = [], epicPendingCommands = []}
      changed = setReplica replica snapshot
      -- A contributor updates its replica optimistically. Still notify its
      -- entities, which may need to react to the authoritative shared total.
      sync = [SyncEpic shared | old.epicShared /= shared || not (null old.epicPendingDeltas) || maybe False (\outcome -> snapshot.epicGameState.outcome /= Just outcome) (epicOutcome shared)]
      messages = sync <> map ApplyEpicCommand commands
  if isJust snapshot.epicGameState.outcome
    then do
      let terminal = setReplica
            replica
              { epicShared = shared
              , epicAppliedCommands = old.epicAppliedCommands <> Set.fromList (map epicCommandId commands)
              }
            snapshot
      case epicOutcome shared of
        Nothing -> pure terminal
        Just outcome -> liftIO do
          -- Reconcile an optimistic local ending with the event's accepted
          -- outcome, including the city's mirrored tokens. Do not run old queues.
          let state = terminal.epicGameState & outcomeL ?~ outcome & questionL .~ mempty
          env <- newGameEnv state (const $ pure ()) 0
          synchronized <- runGameT env $ runGame (SyncEpic shared) state
          pure terminal {epicGameState = synchronized & outcomeL ?~ outcome & questionL .~ mempty}
    else
      if null messages && old.epicBoards == boards
        then pure changed
        else runLocalMessages messages changed

-- Keep each team's queue and RNG independent when the event releases a barrier.
runLocalMessages :: MonadIO m => [Message] -> EpicGame -> m EpicGame
runLocalMessages messages snapshot = liftIO do
  said <- newIORef []
  env <- newGameEnv snapshot.epicGameState (\message -> modifyIORef' said (message :)) 0
  runGameT env $ setQueue snapshot.epicGameQueue
  writeIORef (gameRandomGen env) snapshot.epicGameGenerator
  void $ runGameT env $ runDebug $ Run messages
  state <- runGameT env getGame
  queue <- runGameT env peekQueue
  generator <- readIORef (gameRandomGen env)
  sent <- reverse <$> readIORef said
  pure snapshot
    { epicGameState = state
    , epicGameQueue = queue
    , epicGameGenerator = generator
    , epicGameSent = snapshot.epicGameSent <> sent
    }

-- Coordinator packets are consumed under the same event lock as ordinary
-- deliveries. Responses can resume a native queue and emit further packets,
-- which must also be collected before any outbox is cleared.
collectDolCommands
  :: MonadIO m => SharedEventState -> [EpicGame] -> m (SharedEventState, [EpicGame])
collectDolCommands initialShared initialGames = collect (0 :: Int) mempty initialShared initialGames
 where
  collect rounds seen shared games
    | rounds >= 100 = liftIO $ throwIO $ userError "Dol coordinator requests did not settle"
    | otherwise = case
        [ (ordinalOf snapshot, command)
        | snapshot <- games
        , command <- foldMap epicPendingCommands $ replicaOf snapshot
        , DolCommands.isDolCoordinatorCommand command
        , command.epicCommandId `Set.notMember` seen
        ] of
          [] -> pure (shared, games)
          packets -> do
            (updated, resumed) <- foldM accept (shared, games) packets
            collect (rounds + 1) (seen <> Set.fromList (map (epicCommandId . snd) packets)) updated resumed
  accept (shared, games) (origin, command) = do
    unless (command.epicCommandFrom == origin) $ liftIO $ throwIO $ userError "Dol command has the wrong originating group"
    (updated, response) <- requireDol $ DolCommands.handleDolCoordinatorCommand
      (fromMaybe 0 (maximumMay $ map ((.round) . epicGameState) games)) (GroupOrdinal 0) (dolPrevious games) command shared
    resumed <- case response of
      Nothing -> pure games
      Just (DolCommands.RejectDolClaim destination nonce) -> respond updated destination [RejectDolStageClaim nonce] games
      Just (DolCommands.AcknowledgeDolClaim destination nonce) -> respond updated destination
        [ResolveEpicEffect "attack-on-dol-guldur-007" (EpicAcknowledge nonce) ScenarioTarget] games
    pure (updated, resumed)
  respond shared destination messages = traverse $ \snapshot ->
    if ordinalOf snapshot /= destination then pure snapshot else do
      let synchronized = case replicaOf snapshot of
            Nothing -> snapshot
            Just replica -> setReplica (replica {epicShared = shared}) snapshot
      runLocalMessages messages synchronized

requireDol :: MonadIO m => Either Text a -> m a
requireDol = either (liftIO . throwIO . userError . unpack) pure

dolPrevious :: [EpicGame] -> [(GroupOrdinal, Maybe CardCode)]
dolPrevious games =
  [ (ordinalOf snapshot, headMay
      [q.epicQuestCard | stage <- maybeToList board.epicBoardStage, q <- stage.epicStageQuests
      , q.epicQuestCard `elem` Dol.dolFinalStages])
  | snapshot <- games
  , let board = epicBoardFor (ordinalOf snapshot) "" snapshot.epicGameState
  ]

dolRealEnded :: EpicGame -> Bool
dolRealEnded snapshot = maybe False ((== EpicRoundEnded) . epicRoundPhase) $ replicaOf snapshot >>= epicRoundState

dolCoordinationActive :: SharedEventState -> Bool
dolCoordinationActive shared = maybe False (not . Dol.dolTransitionFinished . epicDolTransition) shared.dolGuldurCoordination

-- Run one durable Dol transition intent. The pure planner records each
-- in-flight step before native queues run; native marker packets acknowledge it.
coordinateDolEvent
  :: MonadIO m => SharedEventState -> [EpicGame]
  -> m (Maybe (SharedEventState, [EpicGame]))
coordinateDolEvent shared games
  | isNothing (dolGuldurCityStrength shared) || length games /= 3 = pure Nothing
  | isJust (epicOutcome shared) || any (isJust . (.outcome) . epicGameState) games = pure Nothing
  | otherwise = case shared.dolGuldurCoordination of
      Nothing | canRotate -> startRotation
      Just state | Dol.dolTransitionFinished state.epicDolTransition
        , Dol.dolEpochRound (Dol.dolTransitionEpoch state.epicDolTransition) < roundNumber
        , canRotate -> startRotation
      Just state | not (Dol.dolTransitionFinished state.epicDolTransition) -> do
        let transition = state.epicDolTransition
        if Dol.dolTransitionPhase transition == Dol.DolDrainingQueues
          && not ordinaryBlocked && any revealedTeam games
          then drainOne shared games
          else do
            intent <- requireDol $ either (Left . tshow) Right $ Dol.nextDolStep eligibility transition
            case intent of
              Nothing
                | Dol.dolTransitionPhase transition == Dol.DolCollectingClaims
                , Dol.dolEpochKind (Dol.dolTransitionEpoch transition) == Dol.DolRotateStages
                , not ordinaryBlocked, all dolRealEnded games
                , length (Dol.dolAcceptedClaims transition) < 3 -> do
                    prompted <- traverse (\snapshot -> if ordinalOf snapshot `elem` map fst (Dol.dolAcceptedClaims transition)
                      then pure snapshot
                      else runLocalMessages [ScenarioSpecific "dolGuldur.chooseStage" Null] snapshot) games
                    pure $ Just (shared, prompted)
                | otherwise -> pure Nothing
              Just (step, updatedTransition) -> do
                let updated = shared
                      {dolGuldurCoordination = Just state {epicDolTransition = updatedTransition}, revision = shared.revision + 1}
                synchronized <- synchronize updated games
                execute step updated synchronized
      _ -> pure Nothing
 where
  roundNumber = fromMaybe 0 (maximumMay $ map ((.round) . epicGameState) games)
  ordinaryBlocked = any (\snapshot -> dolOrdinaryBlocked shared (ordinalOf snapshot) snapshot.epicGameState) games
  canRotate = all dolRealEnded games && not ordinaryBlocked
    && all (isJust . snd) (dolPrevious games)
  revealedTeam snapshot = maybe False ((== EpicRoundRevealed) . epicRoundPhase) $ replicaOf snapshot >>= epicRoundState
  releasedQueuesEnded = case shared.dolGuldurCoordination of
    Just state | Dol.dolEpochKind (Dol.dolTransitionEpoch state.epicDolTransition) == Dol.DolInitialStages ->
      let released = Dol.dolRevealedTeams state.epicDolTransition
       in not (null released) && all dolRealEnded [snapshot | snapshot <- games, ordinalOf snapshot `elem` released]
    _ -> all dolRealEnded games
  eligibility = Dol.DolEligibility True (not ordinaryBlocked) (all dolRealEnded games) releasedQueuesEnded
  startRotation = do
    updated <- requireDol $ DolCommands.startDolCoordination
      (Dol.DolEpoch roundNumber Dol.DolRotateStages) (GroupOrdinal 0) (dolPrevious games) shared
    synchronized <- synchronize updated games
    pure $ Just (updated, synchronized)
  synchronize updated snapshots =
    let boards = [epicBoardFor (ordinalOf snapshot) "" snapshot.epicGameState | snapshot <- snapshots]
     in traverse (runExternal updated boards []) snapshots
  runAt destination messages = traverse $ \snapshot ->
    if ordinalOf snapshot == destination then runLocalMessages messages snapshot else pure snapshot
  execute step updated snapshots = do
    let state = fromJustNote "Missing Dol transition" updated.dolGuldurCoordination
        epoch = Dol.dolTransitionEpoch state.epicDolTransition
        done code acknowledgement = QueueEpicCommand (GroupOrdinal 0) code (EpicDolStepDone epoch acknowledgement) ScenarioTarget
    case step of
      Dol.ResolveDolCity holder -> do
        resolved <- runAt holder
          [CheckWindows [mkWhen DolGuldurStageEffectsResolved], done "attack-on-dol-guldur-007" Dol.DolCityResolved] snapshots
        pure $ Just (updated, resolved)
      Dol.ResolveUnusedDolStage holder code -> do
        resolved <- runAt holder
          [CheckWindows [mkWhen DolGuldurSiegeResolved], done code Dol.DolUnusedStageResolved] snapshots
        pure $ Just (updated, resolved)
      Dol.PrepareDolTeams choices -> do
        prepared <- for snapshots $ \snapshot -> case lookup (ordinalOf snapshot) choices of
          Nothing -> pure snapshot
          Just code -> do
            let deck = scenarioQuestDeck $ toAttrs snapshot.epicGameState.scenario
            card <- maybe (liftIO $ throwIO $ userError "Reserved physical Dol stage is missing") pure $ find ((== code) . toCardCode) deck
            runLocalMessages [PrepareEpicQuestAdvance EpicLeaveQuest card] snapshot
        pure $ Just (updated, prepared)
      Dol.RevealDolTeam destination _ -> do
        revealed <- runAt destination [RevealEpicQuestAdvance] snapshots
        pure $ Just (updated, revealed)
      Dol.ReleaseDolTeams -> do
        resumed <- drainOne updated snapshots
        pure $ resumed <|> Just (updated, snapshots)
      Dol.ContinueDolInitialStages -> pure $ Just (updated, snapshots)
      Dol.FinishDolTransition -> pure $ Just (updated, snapshots)
  drainOne updated snapshots = do
    let state = fromJustNote "Missing Dol claim replies" updated.dolGuldurCoordination
        transition = state.epicDolTransition
        previousOrder snapshot = (join $ lookup (ordinalOf snapshot) (Dol.dolPreviousStages transition), ordinalOf snapshot)
        revealed snapshot = maybe False ((== EpicRoundRevealed) . epicRoundPhase) $ replicaOf snapshot >>= epicRoundState
        resolving snapshot = maybe False ((== EpicRoundResolving) . epicRoundPhase) $ replicaOf snapshot >>= epicRoundState
    if any resolving snapshots
      then pure Nothing
      else case find revealed (sortOn previousOrder snapshots) of
        Nothing -> pure Nothing
        Just next -> do
          let phase = if Dol.dolEpochKind (Dol.dolTransitionEpoch transition) == Dol.DolInitialStages
                then EpicRoundResolving else EpicRoundEnded
              replies = [ResolveEpicEffect "attack-on-dol-guldur-007" (EpicAcknowledge nonce) ScenarioTarget
                | (origin, nonce) <- state.epicDolClaimReplies, origin == ordinalOf next]
          resumed <- runAt (ordinalOf next) (ResumeEpicQuestAdvance phase : replies) snapshots
          pure $ Just (updated, resumed)

-- Annúminas rules, Epic Multiplayer: all teams finish the refresh phase before
-- any end-of-round effects resolve, in B,D,F order. The frozen trigger lists and
-- barrier phases live in the persisted games, so choices and reconnects cannot
-- recompute a later team's triggers after an enemy has moved there.
coordinateEpicRound
  :: MonadIO m => SharedEventState -> [EpicGame]
  -> m (Maybe (SharedEventState, [EpicGame]))
coordinateEpicRound shared games
  | length games /= 3 || any (isJust . (.outcome) . epicGameState) games = pure Nothing
  | dolCoordinationActive shared =
      case nextEpicRoundStep (map dolTeamState games) of
        Just (ResolveEpicTeam destination)
          | Just state <- shared.dolGuldurCoordination
          , Dol.dolEpochKind (Dol.dolTransitionEpoch state.epicDolTransition) == Dol.DolInitialStages
          , Dol.dolTransitionPhase state.epicDolTransition == Dol.DolCollectingClaims -> do
              resumed <- traverse (\snapshot -> if ordinalOf snapshot == destination
                then runLocalMessages [BeginEpicRoundEnd] snapshot else pure snapshot) games
              pure $ Just (shared, resumed)
        _ -> pure Nothing
  | otherwise = do
      updated <- if all expired games && isJust (dolGuldurCityStrength shared)
        then resetDolSiege shared else pure shared
      let boards = [epicBoardFor (ordinalOf snapshot) "" snapshot.epicGameState | snapshot <- games]
      synchronized <- if updated == shared then pure games else traverse (runExternal updated boards []) games
      fmap ((updated,) <$>) $ coordinateEpicRoundNative synchronized
 where
  expired snapshot = maybe False ((== EpicRoundExpired) . epicRoundPhase) $ replicaOf snapshot >>= epicRoundState
  dolTeamState snapshot =
    let own = ordinalOf snapshot
        board = epicBoardFor own "" snapshot.epicGameState
        codes = [q.epicQuestCard | stage <- maybeToList board.epicBoardStage, q <- stage.epicStageQuests]
     in (own, replicaOf snapshot >>= epicRoundState, codes, dolOrdinaryBlocked shared own snapshot.epicGameState)

-- This also resets the first two preparation rounds, when no stage-three
-- rotation exists yet. Reset before ANY team begins the next round.
resetDolSiege :: MonadIO m => SharedEventState -> m SharedEventState
resetDolSiege shared
  | sharedCounter DolGuldurSiegeTracked shared == 0 = pure shared
  | otherwise = do
      ident <- liftIO getRandom
      pure $ fst $ applySharedDelta (SharedDelta ident (GroupOrdinal 0) DolGuldurSiegeTracked
        $ negate $ sharedCounter DolGuldurSiegeTracked shared) shared

coordinateEpicRoundNative :: MonadIO m => [EpicGame] -> m (Maybe [EpicGame])
coordinateEpicRoundNative games
  | length games /= 3 || any (isJust . (.outcome) . epicGameState) games = pure Nothing
  | otherwise = case nextEpicRoundStep (map teamState games) of
      Just FreezeEpicTeams -> do
        let frozen =
              [ (gameIdOf snapshot, runReadGame (filter isForcedAbility <$> getWindowAbilities [mkWhen RoundEnds]) snapshot.epicGameState)
              | snapshot <- games
              ]
        Just <$> traverse (\snapshot -> runLocalMessages
          [FreezeEpicRoundEnd $ fromJustNote "Missing Epic trigger snapshot" $ lookup (gameIdOf snapshot) frozen] snapshot) games
      Just (ResolveEpicTeam ordinal) ->
        Just <$> traverse (\snapshot -> if ordinalOf snapshot == ordinal
          then runLocalMessages [BeginEpicRoundEnd] snapshot
          else pure snapshot) games
      Just ExpireEpicTeams -> Just <$> traverse (runLocalMessages [FinishEpicRoundEffects]) games
      Just BeginEpicTeams -> Just <$> traverse (runLocalMessages [BeginEpicRoundBeginning]) games
      Just FreezeEpicBeginningTeams -> do
        let frozen =
              [ (gameIdOf snapshot, runReadGame (filter isForcedAbility <$> getWindowAbilities [mkWhen RoundBegins]) snapshot.epicGameState)
              | snapshot <- games
              ]
        Just <$> traverse (\snapshot -> runLocalMessages
          [FreezeEpicRoundBeginning $ fromJustNote "Missing Epic beginning trigger snapshot" $ lookup (gameIdOf snapshot) frozen] snapshot) games
      Just ReleaseEpicResources -> Just <$> traverse (runLocalMessages [BeginEpicRoundResources]) games
      Just RevealEpicTeams -> Just <$> traverse (runLocalMessages [RevealEpicQuestAdvance]) games
      Just ReleaseEpicBeginningTeams
        | all atBeginReady games, isAnnuminas, annuminasStageTwoReady boards -> do
            let nextQuest snapshot = headMay $ scenarioQuestDeck $ toAttrs snapshot.epicGameState.scenario
            case traverse nextQuest games of
              Nothing -> liftIO $ throwIO $ userError "Annúminas final quest is missing"
              Just chosen -> Just <$> traverse (\(snapshot, card) ->
                runLocalMessages [PrepareEpicQuestAdvance EpicDefeatQuest card] snapshot) (zip games chosen)
        | otherwise -> Just <$> traverse (runLocalMessages [ReleaseEpicRoundBeginning]) games
      Nothing -> pure Nothing
 where
  isAnnuminas = any (maybe False (isJust . annuminasCityStrength . epicShared) . replicaOf) games
  atBeginReady snapshot = maybe False ((== EpicRoundBeginReady) . epicRoundPhase) $ replicaOf snapshot >>= epicRoundState
  boards = [epicBoardFor (ordinalOf snapshot) "" snapshot.epicGameState | snapshot <- games]
  teamState snapshot =
    let board = epicBoardFor (ordinalOf snapshot) "" snapshot.epicGameState
        codes = [q.epicQuestCard | stage <- maybeToList board.epicBoardStage, q <- stage.epicStageQuests]
     in (ordinalOf snapshot, replicaOf snapshot >>= epicRoundState, codes, epicRoundBlocked snapshot.epicGameState)

{- | Flush every group's outbox until cross-group consequences settle or await
a player choice. The caller holds the event lock and saves any local undo
patch in this same transaction, against the returned committed source state.
-}
commitGames
  :: MonadIO m
  => LotrEpicEventId -> Maybe SharedEventState -> [EpicGame] -> ReaderT SqlBackend m [EpicGame]
commitGames eid baseline initialGames = do
  event <- getJust eid
  storedShared <- parseStored "Epic shared state" event.lotrEpicEventSharedState
  let original = fromMaybe storedShared baseline
  (shared, games) <- settle (0 :: Int) storedShared initialGames
  now <- liftIO getCurrentTime
  update eid [LotrEpicEventSharedState =. toJSON shared, LotrEpicEventUpdatedAt =. now]
  -- A shared milestone may have advanced or ended another group. Rewinding it
  -- requires the whole event, so its contributing step becomes an undo floor.
  when
    ( (stageOneComplete shared && not (stageOneComplete original))
        || (dreadnaughtDestroyed shared && not (dreadnaughtDestroyed original))
        || (isJust (epicOutcome shared) && isNothing (epicOutcome original))
    )
    $ for_ games \snapshot -> raiseUndoFloor (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep
  for games \snapshot -> do
    let gid = gameIdOf snapshot
        before = snapshot.epicGameRow
        after = snapshot.epicGameState
    stored <- storeLog gid (max (-1) $ before.lotrGameStep - 1) now snapshot.epicGameSent
    row <-
      updateGet
        gid
        [ LotrGameCurrentData =. toJSON after
        , LotrGameCurrentQueue =. toJSON snapshot.epicGameQueue
        , LotrGameRandomGen =. showGenerator snapshot.epicGameGenerator
        , LotrGameRevealedStep =. before.lotrGameRevealedStep
        , LotrGameRevision =. before.lotrGameRevision + 1
        , LotrGameStep =. before.lotrGameStep
        , LotrGameUpdatedAt =. now
        ]
    pure snapshot {epicGameRow = row, epicGameSent = stored}
 where
  settle rounds shared games
    | rounds >= 100 = liftIO $ throwIO $ userError "Epic effect delivery did not settle"
    | otherwise = do
        now <- liftIO getCurrentTime
        contributed <- foldM (collectDeltas now) shared games
        (coordinatorShared, sourceGames) <- collectDolCommands contributed games
        -- A rejected/stale claim can resume native effects and generate deltas.
        shared' <- foldM (collectDeltas now) coordinatorShared sourceGames
        when (shared'.dolGuldurCoordination /= shared.dolGuldurCoordination) $
          for_ sourceGames $ \snapshot -> raiseUndoFloor (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep
        for_ sourceGames \snapshot -> for_ (filter (not . DolCommands.isDolCoordinatorCommand) $ foldMap epicPendingCommands $ replicaOf snapshot) \command -> do
          unless (command.epicCommandFrom == ordinalOf snapshot)
            $ liftIO
            $ throwIO
            $ userError "Epic command has the wrong originating group"
          destination <-
            maybe (liftIO $ throwIO $ userError "Epic destination group no longer exists") pure
              $ find ((== command.epicCommandTo) . ordinalOf) sourceGames
          inserted <-
            insertUnique
              $ LotrEpicDelivery eid command.epicCommandId (gameIdOf destination) (toJSON command) False now
          when (isJust inserted) $ raiseUndoFloor (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep
        pending <- selectList [LotrEpicDeliveryEventId ==. eid, LotrEpicDeliveryDelivered ==. False] []
        deliveries <- for pending \(Entity key row) -> (key,row.lotrEpicDeliveryGameId,) <$> parseStored "Epic command" row.lotrEpicDeliveryCommand
        let boards =
              [ epicBoardFor (ordinalOf snapshot) snapshot.epicGameGroup.lotrEpicGroupName snapshot.epicGameState
              | snapshot <- sourceGames
              ]
        games' <- for sourceGames \snapshot -> do
          let commands = [command | (_, gid, command) <- deliveries, gid == gameIdOf snapshot]
          unless (null commands) $ raiseUndoFloor (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep
          runExternal shared' boards commands snapshot
        for_ deliveries \(key, _, _) -> update key [LotrEpicDeliveryDelivered =. True]
        let outbox snapshot =
              maybe False (\r -> not (null r.epicPendingDeltas && null r.epicPendingCommands))
                $ replicaOf snapshot
            newBoards =
              [ epicBoardFor (ordinalOf snapshot) snapshot.epicGameGroup.lotrEpicGroupName snapshot.epicGameState
              | snapshot <- games'
              ]
        if any outbox games' || boards /= newBoards
          then settle (rounds + 1) shared' games'
          else do
            dolStep <- coordinateDolEvent shared' games'
            step <- case dolStep of
              Just changed -> pure $ Just changed
              Nothing -> coordinateEpicRound shared' games'
            case step of
              Nothing -> pure (shared', games')
              Just (coordinatedShared, coordinated) -> do
                -- A barrier changes every group's timing and cannot be undone by
                -- rewinding one local game independently.
                for_ games' $ \snapshot -> raiseUndoFloor (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep
                settle (rounds + 1) coordinatedShared coordinated
  collectDeltas now shared snapshot = foldM apply shared (foldMap epicPendingDeltas $ replicaOf snapshot)
   where
    apply state delta = do
      unless (delta.deltaGroup == ordinalOf snapshot)
        $ liftIO
        $ throwIO
        $ userError "Epic delta has the wrong originating group"
      let (next, effective) = applySharedDelta delta state
      for_ effective \accepted ->
        insert_
          $ LotrEpicDelta eid (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep (toJSON accepted) now
      pure next

{- | Undo additive contributions against the current event state, never against
the old replica saved in an undo patch. Other groups' progress is preserved.
-}
revertSteps
  :: MonadIO m => LotrEpicEventId -> LotrGameId -> [Int] -> ReaderT SqlBackend m SharedEventState
revertSteps eid gid steps = do
  event <- getJust eid
  shared <- parseStored "Epic shared state" event.lotrEpicEventSharedState
  rows <-
    selectList
      [LotrEpicDeltaEventId ==. eid, LotrEpicDeltaGameId ==. gid, LotrEpicDeltaStep <-. steps]
      []
  shared' <- foldM revert shared rows
  update eid [LotrEpicEventSharedState =. toJSON shared']
  deleteWhere [LotrEpicDeltaId <-. map entityKey rows]
  pure shared
 where
  revert state (Entity _ row) = do
    delta <- parseStored "Epic undo delta" row.lotrEpicDeltaDelta
    ident <- liftIO getRandom
    pure $ fst $ applySharedDelta delta {deltaId = ident, deltaAmount = negate delta.deltaAmount} state
