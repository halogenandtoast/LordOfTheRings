{- | Engine-visible Epic state, following Arkham.Epic.Types. The event row is the
authority; individual games read a replica and emit uniquely identified deltas.
Applying a delta and saving the originating game must share one transaction.
-}
module LordOfTheRings.Epic.Types (
  GroupOrdinal (..),
  SharedKey (..),
  SharedDelta (..),
  SharedEventState (..),
  EpicReplica (..),
  EpicBoard (..),
  EpicStage (..),
  EpicQuest (..),
  EpicTarget (..),
  EpicTargetQuery (..),
  EpicEffect (..),
  EpicEnemyArrival (..),
  EpicEnemyTransfer (..),
  EpicCommand (..),
  newSharedEvent,
  newAnnuminasEvent,
  annuminasCityStrength,
  annuminasCityDefeated,
  sharedCounter,
  applySharedDelta,
  stageOneComplete,
  dreadnaughtDestroyed,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Card.EncounterCard (EncounterCard)
import LordOfTheRings.Card.PlayerCard (PlayerCard)
import LordOfTheRings.Effect (Effect)
import LordOfTheRings.Id (EnemyId, CardId, PlayerId)
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Target)
import LordOfTheRings.Token (Tokens)

newtype GroupOrdinal = GroupOrdinal {unGroupOrdinal :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

data SharedKey = SearchingTheHighSeasProgress | DreadnaughtDamage | AnnuminasCityStrength
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Every retried delivery carries the same id. A committed contribution cannot be
counted twice after a timeout, reconnect, or overlapping group requests.
-}
data SharedDelta = SharedDelta
  { deltaId :: UUID
  , deltaGroup :: GroupOrdinal
  , deltaKey :: SharedKey
  , deltaAmount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SharedEventState = SharedEventState
  { totalPlayers :: Int
  -- ^ Frozen when the event starts, including players subsequently eliminated.
  , counters :: Map Text Int
  , appliedDeltas :: Set UUID
  , revision :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newSharedEvent :: Int -> Maybe SharedEventState
newSharedEvent players
  | players > 0 = Just $ SharedEventState players mempty mempty 0
  | otherwise = Nothing

-- The Epic setup card sets one shared city to 50, for three teams of 1–4.
newAnnuminasEvent :: Int -> Maybe SharedEventState
newAnnuminasEvent players
  | players >= 3 && players <= 12 =
      Just $ SharedEventState players (Map.singleton (counterKey AnnuminasCityStrength) 50) mempty 0
  | otherwise = Nothing

annuminasCityStrength :: SharedEventState -> Maybe Int
annuminasCityStrength = Map.lookup (counterKey AnnuminasCityStrength) . (.counters)

annuminasCityDefeated :: SharedEventState -> Bool
annuminasCityDefeated state = annuminasCityStrength state == Just 0

counterKey :: SharedKey -> Text
counterKey SearchingTheHighSeasProgress = "searching-the-high-seas-progress"
counterKey DreadnaughtDamage = "dreadnaught-damage"
counterKey AnnuminasCityStrength = "annuminas-city-strength"

sharedCounter :: SharedKey -> SharedEventState -> Int
sharedCounter key state = Map.findWithDefault 0 (counterKey key) state.counters

{- | Return the state and the effective contribution, bounded at zero and any
printed global requirement. The effective amount is what the step journal saves
for undo; using the requested amount would erase another group's contribution.
-}
applySharedDelta :: SharedDelta -> SharedEventState -> (SharedEventState, Maybe SharedDelta)
applySharedDelta delta state
  | delta.deltaId `Set.member` state.appliedDeltas = (state, Nothing)
  | otherwise =
      let old = sharedCounter delta.deltaKey state
          ceiling' = case delta.deltaKey of
            SearchingTheHighSeasProgress -> state.totalPlayers * 8
            DreadnaughtDamage -> state.totalPlayers * 15
            AnnuminasCityStrength -> maxBound
          -- Annúminas rules, p. 5: reaching zero loses immediately. A repair
          -- delivered by another team afterwards cannot revive the city. There
          -- is no printed upper limit on repairing a surviving city.
          new
            | delta.deltaKey == AnnuminasCityStrength && annuminasCityDefeated state = 0
            | otherwise = fromInteger $ max 0 $ min (toInteger ceiling') (toInteger old + toInteger delta.deltaAmount)
          effective = delta {deltaAmount = new - old}
       in ( state
              { counters =
                  ( if delta.deltaKey == SearchingTheHighSeasProgress && new >= 8 * state.totalPlayers
                      then Map.insert "search-complete" 1
                      else id
                  )
                    $ Map.insert (counterKey delta.deltaKey) new state.counters
              , appliedDeltas = Set.insert delta.deltaId state.appliedDeltas
              , revision = state.revision + 1
              }
          , Just effective
          )

stageOneComplete :: SharedEventState -> Bool
-- Once the organizer announces sufficient progress, every group advances at its
-- next quest-phase end independently (Dreadnaught rules, Global Values).
stageOneComplete state =
  Map.findWithDefault 0 "search-complete" state.counters
    > 0
    || sharedCounter SearchingTheHighSeasProgress state
    >= 8
    * state.totalPlayers

dreadnaughtDestroyed :: SharedEventState -> Bool
dreadnaughtDestroyed state = sharedCounter DreadnaughtDamage state >= 15 * state.totalPlayers

{- | A group's local view and durable outbox. The API drains the outbox under the
event lock in the same transaction that commits the game's new state.
-}
data EpicReplica = EpicReplica
  { epicGroup :: GroupOrdinal
  , epicShared :: SharedEventState
  , epicPendingDeltas :: [SharedDelta]
  , epicBoards :: [EpicBoard]
  , epicPendingCommands :: [EpicCommand]
  , epicAppliedCommands :: Set UUID
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON EpicReplica where
  parseJSON =
    genericParseJSON defaultOptions
      . withDefaults
        [ ("epicBoards", toJSON ([] :: [EpicBoard]))
        , ("epicPendingCommands", toJSON ([] :: [EpicCommand]))
        , ("epicAppliedCommands", toJSON (mempty :: Set UUID))
        ]

-- Only explicitly cross-group effects use these public targets. Ordinary cards
-- and their matchers continue to see only their own group (Dreadnaught rules).
data EpicTargetQuery = EpicEnemy | EpicLocation | EpicShip | EpicEngagedNonUniqueEnemy | EpicWarshipGroup
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicEffect = EpicDamage Int | EpicProgress Int | EpicUpgradeShip | EpicShuffleWarship | EpicDiscardUntilEnemy
  | EpicDiscardCaptive | EpicRescueCaptives
  | EpicGrantDunedain | EpicDelayStaging
  | EpicReceiveEnemy EpicEnemyTransfer EpicEnemyArrival
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicEnemyArrival = EpicStageEnemy | EpicAttackFirstPlayer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Annúminas's immune enemies move between stages without leaving play. Carry
-- their state, including private shadow cards, through the durable outbox.
data EpicEnemyTransfer = EpicEnemyTransfer
  { epicEnemyId :: EnemyId
  , epicEnemyCard :: EncounterCard
  , epicEnemyTokens :: Tokens
  , epicEnemyShadows :: [EncounterCard]
  , epicEnemyRevealedShadows :: [CardId]
  , epicEnemyFaceupShadows :: [CardId]
  , epicEnemyFacedown :: [PlayerCard]
  , epicEnemyOwner :: Maybe PlayerId
  , epicEnemyEffects :: [Effect]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicTarget = EpicTarget
  { epicTarget :: Target
  , epicTargetCard :: EncounterCard
  , epicTargetKind :: EpicTargetQuery
  , epicTargetUnique :: Bool
  , epicTargetDamage :: Int
  , epicTargetExhausted :: Bool
  , epicTargetUpgradeable :: Bool
  , epicTargetEngaged :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicBoard = EpicBoard
  { epicBoardGroup :: GroupOrdinal
  , epicBoardName :: Text
  , epicBoardTargets :: [EpicTarget]
  , epicBoardHasWarship :: Bool
  , epicBoardStage :: Maybe EpicStage
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON EpicBoard where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("epicBoardStage", Null)]

-- These are public values, not another team's game state. In particular no
-- cards beneath a quest, encounter-deck order, or players' hands are included.
data EpicStage = EpicStage
  { epicStageThreat :: Int
  , epicStageQuests :: [EpicQuest]
  , epicStageVictoryCards :: [CardCode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicQuest = EpicQuest
  { epicQuestCard :: CardCode
  , epicQuestNumber :: Int
  , epicQuestProgress :: Int
  , epicQuestRequirement :: Maybe Int
  , epicQuestFlipped :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- The coordinator delivers each command once, independently of either group's
-- next action. Destination games retain command ids to make retries harmless.
data EpicCommand = EpicCommand
  { epicCommandId :: UUID
  , epicCommandFrom :: GroupOrdinal
  , epicCommandTo :: GroupOrdinal
  , epicCommandCard :: CardCode
  , epicCommandEffect :: EpicEffect
  , epicCommandTarget :: Target
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
