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
import LordOfTheRings.Window (WindowType (RoundEnds, RoundBegins), mkWhen)
import LordOfTheRings.Classes.HasQueue (peekQueue, setQueue)
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Epic.Coordinator
import LordOfTheRings.Epic.Types
import LordOfTheRings.Frontend (FrontendMessage)
import LordOfTheRings.Game
import LordOfTheRings.GameT
import LordOfTheRings.Message
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

-- Annúminas rules, Epic Multiplayer: all teams finish the refresh phase before
-- any end-of-round effects resolve, in B,D,F order. The frozen trigger lists and
-- barrier phases live in the persisted games, so choices and reconnects cannot
-- recompute a later team's triggers after an enemy has moved there.
coordinateEpicRound :: MonadIO m => [EpicGame] -> m (Maybe [EpicGame])
coordinateEpicRound games
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
        shared' <- foldM (collectDeltas now) shared games
        for_ games \snapshot -> for_ (foldMap epicPendingCommands $ replicaOf snapshot) \command -> do
          unless (command.epicCommandFrom == ordinalOf snapshot)
            $ liftIO
            $ throwIO
            $ userError "Epic command has the wrong originating group"
          destination <-
            maybe (liftIO $ throwIO $ userError "Epic destination group no longer exists") pure
              $ find ((== command.epicCommandTo) . ordinalOf) games
          inserted <-
            insertUnique
              $ LotrEpicDelivery eid command.epicCommandId (gameIdOf destination) (toJSON command) False now
          when (isJust inserted) $ raiseUndoFloor (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep
        pending <- selectList [LotrEpicDeliveryEventId ==. eid, LotrEpicDeliveryDelivered ==. False] []
        deliveries <- for pending \(Entity key row) -> (key,row.lotrEpicDeliveryGameId,) <$> parseStored "Epic command" row.lotrEpicDeliveryCommand
        let boards =
              [ epicBoardFor (ordinalOf snapshot) snapshot.epicGameGroup.lotrEpicGroupName snapshot.epicGameState
              | snapshot <- games
              ]
        games' <- for games \snapshot -> do
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
          else coordinateEpicRound games' >>= \case
            Nothing -> pure (shared', games')
            Just coordinated -> do
              -- A barrier changes every group's timing and cannot be undone by
              -- rewinding one local game independently.
              for_ games' $ \snapshot -> raiseUndoFloor (gameIdOf snapshot) snapshot.epicGameRow.lotrGameStep
              settle (rounds + 1) shared' coordinated
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
