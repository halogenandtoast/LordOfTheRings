-- | A game's log as stored: entries stamped with a sequence number and the step they were
-- written under, read back as a bounded tail and paged further back on request, as in
-- Arkham's @arkham_log_entries@.
module GameLog (
  storeLog,
  logTail,
  logBefore,
  wholeLog,
  restoreLog,
  trimLog,
  clearLog,
  readsEntry,
) where

import Data.Aeson (Result (..))
import Database.Persist.Postgresql
import LordOfTheRings.Frontend (FrontendMessage (..))
import LordOfTheRings.Id
import LordOfTheRings.Log.Entry
import LordOfTheRings.Prelude
import Model

-- | Store the entries among what the game sent, under the step, giving back what was
-- sent with each entry as stored.
storeLog :: (MonadIO m) => LotrGameId -> Int -> UTCTime -> [FrontendMessage] -> ReaderT SqlBackend m [FrontendMessage]
storeLog gid step now sent = do
  next <- maybe 1 ((+ 1) . (.lotrLogEntrySeq) . entityVal) <$> selectFirst [LotrLogEntryGameId ==. gid] [Desc LotrLogEntrySeq]
  let go _ [] = pure []
      go n (Logged e : rest) = do
        let stored = e {logEntrySeq = n, logEntryStep = undoStep step}
        insert_ $ LotrLogEntry gid step n (tshow e.logEntryKind) (toJSON stored) now
        (Logged stored :) <$> go (n + 1) rest
      go n (m : rest) = (m :) <$> go n rest
  go next sent

-- | Where an undo can land: nowhere, for what was written while the game was dealt.
undoStep :: Int -> Maybe Int
undoStep step = guard (step >= 0) $> step

-- | The latest entries, oldest first: what a client opens the game with.
logTail :: (MonadIO m) => LotrGameId -> ReaderT SqlBackend m [LogEntry]
logTail gid = readRows <$> selectList [LotrLogEntryGameId ==. gid] [Desc LotrLogEntrySeq, LimitTo 150]

-- | A page of the entries before the one given, oldest first, for scrolling back.
logBefore :: (MonadIO m) => LotrGameId -> Int -> ReaderT SqlBackend m [LogEntry]
logBefore gid before = readRows <$> selectList [LotrLogEntryGameId ==. gid, LotrLogEntrySeq <. before] [Desc LotrLogEntrySeq, LimitTo 100]

-- | Every entry, oldest first, for an export.
wholeLog :: (MonadIO m) => LotrGameId -> ReaderT SqlBackend m [LogEntry]
wholeLog gid = readRows <$> selectList [LotrLogEntryGameId ==. gid] [Desc LotrLogEntrySeq]

-- | Store exported entries as they were, each under its own step.
restoreLog :: (MonadIO m) => LotrGameId -> UTCTime -> [LogEntry] -> ReaderT SqlBackend m ()
restoreLog gid now entries =
  insertMany_
    [ LotrLogEntry gid (fromMaybe (-1) e.logEntryStep) n (tshow e.logEntryKind) (toJSON e) now
    | (n, e) <- zip [1 ..] entries
    ]

-- | Newest first in, oldest first out. The row's own seq and step win over the payload's,
-- as chat is moved by an undo.
readRows :: [Entity LotrLogEntry] -> [LogEntry]
readRows = reverse . mapMaybe (row . entityVal)
 where
  row r = case fromJSON r.lotrLogEntryPayload of
    Success e -> Just e {logEntrySeq = r.lotrLogEntrySeq, logEntryStep = undoStep r.lotrLogEntryStep}
    Error _ -> Nothing

-- | Take out what was written at or after the step undone to. What players said stays,
-- moved to just before it.
trimLog :: (MonadIO m) => LotrGameId -> Int -> ReaderT SqlBackend m ()
trimLog gid step = do
  deleteWhere [LotrLogEntryGameId ==. gid, LotrLogEntryStep >=. step, LotrLogEntryKind !=. tshow Chat]
  updateWhere [LotrLogEntryGameId ==. gid, LotrLogEntryStep >=. step] [LotrLogEntryStep =. step - 1]

clearLog :: (MonadIO m) => LotrGameId -> ReaderT SqlBackend m ()
clearLog gid = deleteWhere [LotrLogEntryGameId ==. gid]

-- | Whether a viewer may read the entry, given whose hands they see.
readsEntry :: (PlayerId -> Bool) -> LogEntry -> Bool
readsEntry seesHand e = case e.logEntryAudience of
  Everyone -> True
  OnlyPlayer pid -> seesHand pid
  ExceptPlayer pid -> not (seesHand pid)
