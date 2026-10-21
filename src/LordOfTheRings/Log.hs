-- | Building log entries, as in Arkham's "Arkham.Log": typed parts in, structure out, so
-- the client draws chips and the sentence is translated.
--
-- > sendLog $ mechanic [ikeyPart "log.takesDamage" ["target" ~> ref, "count" ~> n]]
--
-- Most of the log is derived from the message stream by "LordOfTheRings.Log.Narrator";
-- a card writes an entry itself only for what the engine cannot see.
module LordOfTheRings.Log (module LordOfTheRings.Log, module LordOfTheRings.Log.Entry) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Frontend
import LordOfTheRings.Id
import LordOfTheRings.Log.Entry
import LordOfTheRings.Name
import LordOfTheRings.Prelude

-- * Parts

-- | Anything that can stand in a log sentence.
class ToLogPart a where
  toLogPart :: a -> LogPart

instance ToLogPart LogPart where
  toLogPart = id

instance ToLogPart Text where
  toLogPart = LogText

instance ToLogPart Int where
  toLogPart = LogNumber

instance ToLogPart LogRef where
  toLogPart = LogRefPart

instance ToLogPart Name where
  toLogPart = LogText . display

instance ToLogPart Card where
  toLogPart = LogRefPart . cardRef

instance ToLogPart Sphere where
  toLogPart = LogIcon . sphereKey

instance (ToLogPart a) => ToLogPart [a] where
  toLogPart = LogList . map toLogPart

-- | An i18n key under @log.@ in the client's locale files, with variables that may
-- themselves be rich.
ikeyPart :: Text -> [(Text, LogPart)] -> LogPart
ikeyPart k = LogI18n k . Map.fromList

(~>) :: (ToLogPart a) => Text -> a -> (Text, LogPart)
k ~> a = (k, toLogPart a)

infixr 6 ~>

delta :: Int -> LogPart
delta = LogDelta

icon :: Text -> LogPart
icon = LogIcon

-- | The wire name of a sphere icon.
sphereKey :: Sphere -> Text
sphereKey = toLower . tshow

-- * Refs

-- | A card that is not in play, or not yet: one in hand, revealed, or dealt as a shadow.
cardRef :: Card -> LogRef
cardRef c =
  (logRef RefCard (display c.name))
    { logRefCardCode = Just (toCardCode c)
    , logRefCardId = Just c.id
    }

-- | An id as the client sees it: its JSON text, never its 'Show'.
idText :: (ToJSON a) => a -> Text
idText a = case toJSON a of
  String t -> t
  other -> tshow other

-- | A card in play, by its entity's id.
entityRef :: (ToJSON i, IsCard a) => LogRefKind -> i -> a -> LogRef
entityRef kind eid a = (cardRef (toCard a)) {logRefKind = kind, logRefEntityId = Just (idText eid)}

playerRef :: PlayerId -> Text -> LogRef
playerRef pid name = (logRef RefPlayer name) {logRefEntityId = Just (idText pid)}

-- * Entries

structure :: [LogPart] -> LogEntry
structure = mkLogEntry Structure

action :: [LogPart] -> LogEntry
action = mkLogEntry Action

mechanic :: [LogPart] -> LogEntry
mechanic = mkLogEntry Mechanic

test :: [LogPart] -> LogEntry
test = mkLogEntry Test

notice :: [LogPart] -> LogEntry
notice = mkLogEntry Notice

chat :: [LogPart] -> LogEntry
chat = mkLogEntry Chat

toned :: LogTone -> LogEntry -> LogEntry
toned t e = e {logEntryTone = Just t}

inGroup :: Text -> LogGroupRole -> LogEntry -> LogEntry
inGroup gid role e = e {logEntryGroup = Just (LogGroup gid role)}

-- | Opens a block and names it.
opensGroup :: Text -> LogEntry -> LogEntry
opensGroup gid = inGroup gid GroupHeader

-- | A line inside a block.
inGroupOf :: Text -> LogEntry -> LogEntry
inGroupOf gid = inGroup gid GroupMember

-- | Closes a block with its outcome, which is what it collapses to.
closesGroup :: Text -> LogEntry -> LogEntry
closesGroup gid = inGroup gid GroupSummary

-- | Detail beneath an entry, so one event reads as one entry.
withChildren :: [LogEntry] -> LogEntry -> LogEntry
withChildren cs e = e {logEntryChildren = e.logEntryChildren <> cs}

-- | What caused an entry, as data rather than prose.
because :: LogRef -> LogEntry -> LogEntry
because r e = e {logEntrySource = Just r}

-- | Only for whoever sees this player's hand.
forPlayer :: PlayerId -> LogEntry -> LogEntry
forPlayer pid e = e {logEntryAudience = OnlyPlayer pid}

-- | For everyone who does not.
exceptPlayer :: PlayerId -> LogEntry -> LogEntry
exceptPlayer pid e = e {logEntryAudience = ExceptPlayer pid}

-- * Sending

sendLog :: (Frontend m) => LogEntry -> m ()
sendLog = send . Logged
