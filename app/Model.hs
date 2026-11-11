{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Model (module Model) where

import Data.ByteString.Char8 qualified as BS8
import Data.UUID qualified as UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import LordOfTheRings.Prelude
import Yesod.Core (PathPiece (..))

instance PersistField UUID where
  toPersistValue = PersistLiteral_ Escaped . BS8.pack . UUID.toString
  fromPersistValue (PersistLiteral_ _ t) = maybe (Left "Invalid UUID") Right $ UUID.fromString $ BS8.unpack t
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = UUID.toText
  fromPathPiece = UUID.fromText

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User sql=users
  username Text
  email Text
  passwordDigest Text
  admin Bool default=false
  -- | sees the beta scenarios when making a game, as in Arkham
  beta Bool default=false
  -- | what "allow spectators" starts as when making a game
  allowSpectatorsByDefault Bool default=false
  -- | whether they file bugs on GitHub themselves or have the site file them; unset
  -- until they are asked, as they first file one
  hasGithub Bool Maybe
  createdAt UTCTime
  UniqueUsername username
  UniqueEmail email
  deriving Show

LotrGame sql=lotr_games
  Id UUID default=gen_random_uuid()
  userId UserId OnDeleteCascade
  name Text
  playerName Text
  currentData Value
  currentQueue Value
  randomGen Text
  step Int
  -- | monotonic publication order for Epic updates, including undo
  revision Int default=0
  -- | what reset deals again: the scenario, and the deck when it was not the starter
  scenario Text default='passage-through-mirkwood'
  deck Value Maybe
  -- | whether anyone may watch, not only the owner and admins; chosen when the game is made
  allowSpectators Bool default=false
  -- | played with friends, a seat each, rather than every seat being the owner's
  withFriends Bool default=false
  -- | a game with friends is dealt once all of its seats are taken; until then its
  -- current data is null
  seats Int default=1
  -- | fewer of the difficult encounter cards and a resource more for each hero, dealt
  -- again the same way
  easyMode Bool default=false
  -- | the scenario's Nightmare Deck in place of its standard encounter deck; never with
  -- easy mode
  nightmare Bool default=false
  -- | the Dreadnaught pack's printed Hard Mode setup, distinct from a Nightmare Deck
  hardMode Bool default=false
  -- | the campaign the game is a scenario of, in campaign mode
  campaignId LotrCampaignId Maybe OnDeleteSetNull
  -- | chosen when the game is made: each deal counts toward everyone's statistics until a
  -- debug message or an undo past something revealed takes that deal out
  publicScoring Bool default=false
  -- | the deal being played still counts; dealing again restores it from publicScoring
  countsPublicly Bool default=false
  -- | the latest step whose answer brought hidden cards into view; undoing to before it
  -- takes the deal out of public statistics
  revealedStep Int default=0
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

-- | One Epic Multiplayer event owns independent group games and their shared counters.
LotrEpicEvent sql=lotr_epic_events
  Id UUID default=gen_random_uuid()
  userId UserId OnDeleteCascade
  name Text
  mode Text
  scenario Text default='the-hunt-for-the-dreadnaught'
  started Bool default=false
  sharedState Value
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

LotrEpicGroup sql=lotr_epic_groups
  Id UUID default=gen_random_uuid()
  eventId LotrEpicEventId OnDeleteCascade
  gameId LotrGameId OnDeleteCascade
  ordinal Int
  name Text
  UniqueEpicOrdinal eventId ordinal
  UniqueEpicGame gameId
  deriving Show

-- | Effective contributions, stamped with the originating game's undo step.
LotrEpicDelta sql=lotr_epic_deltas
  eventId LotrEpicEventId OnDeleteCascade
  gameId LotrGameId OnDeleteCascade
  step Int
  delta Value
  createdAt UTCTime
  deriving Show

-- | Durable, idempotent cross-group delivery, committed with its originating move.
LotrEpicDelivery sql=lotr_epic_deliveries
  Id UUID default=gen_random_uuid()
  eventId LotrEpicEventId OnDeleteCascade
  commandId UUID
  gameId LotrGameId OnDeleteCascade
  command Value
  delivered Bool default=false
  createdAt UTCTime
  UniqueEpicCommand commandId
  deriving Show

LotrEpicUndoFloor sql=lotr_epic_undo_floors
  gameId LotrGameId OnDeleteCascade
  step Int
  UniqueEpicUndoFloor gameId
  deriving Show

-- | A campaign played scenario by scenario, each its own game (see "LordOfTheRings.Campaign").
-- The log is as the scenario being played began; the game holds it as that scenario is
-- resolved, and it comes back here as the next one is dealt.
LotrCampaign sql=lotr_campaigns
  Id UUID default=gen_random_uuid()
  userId UserId OnDeleteCascade
  name Text
  log Value
  -- | whether each scenario's game counts toward public statistics, as a game's does
  publicScoring Bool default=false
  nextCampaignRewardClaimed Bool default=false
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

-- | Who sits where at a game with friends, and the deck they brought (null for the
-- starter deck). Seats are dealt in position order, so position n plays the nth player.
-- The owner may take several seats; whoever joins takes one.
LotrSeat sql=lotr_seats
  gameId LotrGameId OnDeleteCascade
  position Int
  userId UserId OnDeleteCascade
  deck Value Maybe
  UniqueSeat gameId position
  deriving Show

-- | Enough to step a game back one answer: a patch from the game after the answer to
-- the game before it, and the queue and random generator from before it.
LotrStep sql=lotr_steps
  gameId LotrGameId OnDeleteCascade
  step Int
  patchDown Value
  queue Value
  randomGen Text
  round Int
  phase Text
  createdAt UTCTime
  UniqueGameStep gameId step
  deriving Show

-- | One entry of a game's log, written under the step the game was on before the answer
-- that wrote it, so undoing to that step takes it back out. Entries written while the
-- game was dealt sit before step 0, where no undo reaches. Rows are only ever added,
-- and deleted by step; chat survives an undo, moved to just before the step undone to.
LotrLogEntry sql=lotr_log_entries
  gameId LotrGameId OnDeleteCascade
  step Int
  seq Int
  kind Text
  payload Value
  createdAt UTCTime
  UniqueLogSeq gameId seq
  deriving Show

-- | One deal of a game, kept for statistics after the game is dealt again or deleted.
-- The result is filled in as the game ends, and taken out again by an undo past the end.
LotrPlay sql=lotr_plays
  Id UUID default=gen_random_uuid()
  gameId LotrGameId Maybe OnDeleteSetNull
  userId UserId OnDeleteCascade
  scenario Text
  -- | the GameMode shown: NormalMode, EasyMode or NightmareMode
  mode Text
  campaign Bool
  -- | counted in everyone's statistics: the game was made to be, and this deal has had no
  -- debug message nor an undo past something revealed
  public Bool default=false
  -- | the deck lists played, in seat order
  decks Value
  -- | Won or Lost; none while the game goes on, or after it was dealt again unfinished
  outcome Text Maybe
  -- | the group score, only of a victory
  score Int Maybe
  rounds Int Maybe
  startedAt UTCTime
  endedAt UTCTime Maybe
  deriving Show

-- | An imported deck list. Slots name our card code when the importer could place
-- the card, and always keep the code the source used.
LotrDeck sql=lotr_decks
  userId UserId OnDeleteCascade
  name Text
  description Text
  startingThreat Int Maybe
  source Text
  sourceId Text Maybe
  sourceUrl Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  usedAt UTCTime Maybe
  -- ^ when a game was last dealt from it
  deriving Show

LotrDeckSlot sql=lotr_deck_slots
  deckId LotrDeckId OnDeleteCascade
  kind Text
  position Int
  sourceCode Text
  cardCode Text Maybe
  quantity Int
  deriving Show

-- | A bug the site filed on GitHub for someone without an account, followed until they
-- have seen it cleared. The board's sweep (see "BugSync") keeps it up to date.
LotrBugReport sql=lotr_bug_reports
  Id UUID default=gen_random_uuid()
  userId UserId OnDeleteCascade
  -- | the game the bug was filed from, while it lasts
  gameId LotrGameId Maybe OnDeleteSetNull
  title Text
  -- | the reporter's game cannot go on until it is fixed
  blocking Bool
  issueNumber Int
  issueUrl Text
  -- | "filed" until the first sweep, then "board:<column>", or "closed:<reason>"
  status Text
  -- | when the issue was closed; reopening it takes this away
  clearedAt UTCTime Maybe
  -- | the latest status change or comment the reporter has yet to see, if after viewedAt
  activityAt UTCTime
  viewedAt UTCTime
  -- | the issue's updatedAt as of the last time its comments were read
  syncedAt UTCTime Maybe
  createdAt UTCTime
  UniqueBugIssue issueNumber
  deriving Show

-- | A comment on a bug report's issue. One posted from the site for the reporter names
-- them, though the site's GitHub account posted it.
LotrBugComment sql=lotr_bug_comments
  reportId LotrBugReportId OnDeleteCascade
  githubId Int
  -- | the GitHub account that posted it
  author Text
  -- | the reporter, when they posted it from the site
  userId UserId Maybe OnDeleteSetNull
  body Text
  createdAt UTCTime
  UniqueBugComment githubId
  deriving Show

-- | Each status a bug report's issue has moved to, as the sweep saw it.
LotrBugStatusChange sql=lotr_bug_status_changes
  reportId LotrBugReportId OnDeleteCascade
  status Text
  createdAt UTCTime
  deriving Show
|]
