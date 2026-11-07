{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | Engine-visible Epic state, following Arkham.Epic.Types. The event row is the
authority; individual games read a replica and emit uniquely identified deltas.
Applying a delta and saving the originating game must share one transaction.
-}
module LordOfTheRings.Epic.Types (
  GroupOrdinal (..),
  SharedKey (..),
  SharedDelta (..),
  SharedEventState (..),
  EpicDolState (..),
  EpicReplica (..),
  EpicRoundPhase (..),
  EpicRoundState (..),
  EpicQuestAdvanceMode (..),
  EpicBoard (..),
  EpicStage (..),
  EpicPlayer (..),
  EpicQuest (..),
  EpicTarget (..),
  EpicTargetQuery (..),
  EpicEffect (..),
  EpicEnemyArrival (..),
  EpicEnemyTransfer (..),
  EpicLorienSupportTransfer (..),
  EpicCommand (..),
  newSharedEvent,
  newAnnuminasEvent,
  newDolGuldurEvent,
  dolGuldurCityStrength,
  dolGuldurOutcome,
  epicOutcome,
  annuminasCityStrength,
  annuminasCityDefeated,
  annuminasOutcome,
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
import LordOfTheRings.Outcome (Outcome (Won, Lost))
import LordOfTheRings.Epic.DolGuldur (DolTransition, DolClaim, DolEpoch, DolAck)
import LordOfTheRings.Effect (Effect)
import LordOfTheRings.Id (EnemyId, AllyId, ObjectiveId, CardId, PlayerId)
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Target)
import LordOfTheRings.Token (Tokens)

newtype GroupOrdinal = GroupOrdinal {unGroupOrdinal :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

data SharedKey = SearchingTheHighSeasProgress | DreadnaughtDamage | AnnuminasCityStrength | AnnuminasVictory | AnnuminasDefeat | DolGuldurCityStrength | DolGuldurVictory | DolGuldurDefeat | DolGuldurSiegeTracked
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
  , dolGuldurCoordination :: Maybe EpicDolState
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON SharedEventState where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("dolGuldurCoordination", Null)]

data EpicDolState = EpicDolState
  { epicDolTransition :: DolTransition GroupOrdinal
  , epicDolClaimReplies :: [(GroupOrdinal, UUID)]
  , epicDolProcessedCommands :: Set UUID
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newSharedEvent :: Int -> Maybe SharedEventState
newSharedEvent players
  | players > 0 = Just $ SharedEventState players mempty mempty 0 Nothing
  | otherwise = Nothing

-- The Epic setup card sets one shared city to 50, for three teams of 1–4.
newAnnuminasEvent :: Int -> Maybe SharedEventState
newAnnuminasEvent players
  | players >= 3 && players <= 12 =
      Just $ SharedEventState players (Map.singleton (counterKey AnnuminasCityStrength) 50) mempty 0 Nothing
  | otherwise = Nothing

-- The mode card sets a single siege tracker to 5 plus 5 per player.
newDolGuldurEvent :: Int -> Maybe SharedEventState
newDolGuldurEvent players
  | players >= 3 && players <= 12 =
      Just $ SharedEventState players (Map.singleton (counterKey DolGuldurCityStrength) (5 + 5 * players)) mempty 0 Nothing
  | otherwise = Nothing

dolGuldurCityStrength :: SharedEventState -> Maybe Int
dolGuldurCityStrength = Map.lookup (counterKey DolGuldurCityStrength) . (.counters)

dolGuldurOutcome :: SharedEventState -> Maybe Outcome
dolGuldurOutcome state
  | isNothing (dolGuldurCityStrength state) = Nothing
  | sharedCounter DolGuldurDefeat state > 0 = Just Lost
  | sharedCounter DolGuldurVictory state > 0 = Just Won
  | otherwise = Nothing

epicOutcome :: SharedEventState -> Maybe Outcome
epicOutcome state = annuminasOutcome state <|> dolGuldurOutcome state

annuminasCityStrength :: SharedEventState -> Maybe Int
annuminasCityStrength = Map.lookup (counterKey AnnuminasCityStrength) . (.counters)

annuminasCityDefeated :: SharedEventState -> Bool
annuminasCityDefeated state = annuminasCityStrength state == Just 0

-- The first authoritative terminal condition ends the whole event.
annuminasOutcome :: SharedEventState -> Maybe Outcome
annuminasOutcome state
  | isNothing (annuminasCityStrength state) = Nothing
  | annuminasCityDefeated state || sharedCounter AnnuminasDefeat state > 0 = Just Lost
  | sharedCounter AnnuminasVictory state > 0 = Just Won
  | otherwise = Nothing

counterKey :: SharedKey -> Text
counterKey SearchingTheHighSeasProgress = "searching-the-high-seas-progress"
counterKey DreadnaughtDamage = "dreadnaught-damage"
counterKey AnnuminasCityStrength = "annuminas-city-strength"
counterKey AnnuminasVictory = "annuminas-victory"
counterKey AnnuminasDefeat = "annuminas-defeat"
counterKey DolGuldurCityStrength = "dol-guldur-city-strength"
counterKey DolGuldurVictory = "dol-guldur-victory"
counterKey DolGuldurDefeat = "dol-guldur-defeat"
counterKey DolGuldurSiegeTracked = "dol-guldur-siege-tracked"

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
            AnnuminasVictory -> 1
            AnnuminasDefeat -> 1
            DolGuldurCityStrength -> maxBound
            DolGuldurVictory -> 1
            DolGuldurDefeat -> 1
            DolGuldurSiegeTracked -> maxBound
          -- Annúminas rules, p. 5: reaching zero loses immediately. A repair
          -- delivered by another team afterwards cannot revive the city. There
          -- is no printed upper limit on repairing a surviving city.
          new
            | delta.deltaKey `elem` [AnnuminasCityStrength, AnnuminasVictory, AnnuminasDefeat]
            , isJust (annuminasOutcome state) = old
            | delta.deltaKey `elem` [AnnuminasVictory, AnnuminasDefeat]
            , isNothing (annuminasCityStrength state) = old
            | delta.deltaKey `elem` [DolGuldurCityStrength, DolGuldurVictory, DolGuldurDefeat]
            , isJust (dolGuldurOutcome state) = old
            | delta.deltaKey `elem` [DolGuldurCityStrength, DolGuldurVictory, DolGuldurDefeat]
            , isNothing (dolGuldurCityStrength state) = old
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

-- The event coordinator freezes every team's triggers before releasing any
-- end-of-round effects, then expires lasting effects before the next round.
data EpicRoundPhase = EpicRoundIdle | EpicRoundWaiting | EpicRoundReady | EpicRoundResolving | EpicRoundEnded | EpicRoundExpired
  | EpicRoundBeginning | EpicRoundBeginReady | EpicRoundPreparingAdvance | EpicRoundPrepared | EpicRoundRevealing | EpicRoundRevealed | EpicRoundBeginResolving | EpicRoundBegan
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicQuestAdvanceMode = EpicDefeatQuest | EpicLeaveQuest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicRoundState = EpicRoundState
  { epicRoundNumber :: Int
  , epicRoundPhase :: EpicRoundPhase
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | A group's local view and durable outbox. The API drains the outbox under the
event lock in the same transaction that commits the game's new state.
-}
data EpicReplica = EpicReplica
  { epicGroup :: GroupOrdinal
  , epicShared :: SharedEventState
  , epicPendingDeltas :: [SharedDelta]
  , epicBoards :: [EpicBoard]
  , epicPendingCommands :: [EpicCommand]
  , epicPendingRequests :: Set UUID
  , epicAppliedCommands :: Set UUID
  , epicRoundState :: Maybe EpicRoundState
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON EpicReplica where
  parseJSON =
    genericParseJSON defaultOptions
      . withDefaults
        [ ("epicRoundState", Null)
        , ("epicBoards", toJSON ([] :: [EpicBoard]))
        , ("epicPendingCommands", toJSON ([] :: [EpicCommand]))
        , ("epicPendingRequests", toJSON (mempty :: Set UUID))
        , ("epicAppliedCommands", toJSON (mempty :: Set UUID))
        ]

-- Only explicitly cross-group effects use these public targets. Ordinary cards
-- and their matchers continue to see only their own group (Dreadnaught rules).
data EpicTargetQuery = EpicEnemy | EpicLocation | EpicShip | EpicEngagedNonUniqueEnemy | EpicWarshipGroup | EpicHealingGroup | EpicDrawingGroup | EpicOtherNonUniqueNoVictoryEnemy | EpicNonUniqueEnemy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EpicEffect = EpicDamage Int | EpicProgress Int | EpicUpgradeShip | EpicShuffleWarship | EpicDiscardUntilEnemy
  | EpicDiscardCaptive | EpicRescueCaptives | EpicCaptureHero
  | EpicGrantDunedain | EpicDelayStaging | EpicGrantHeroResources | EpicHealControlledCharacter
  | EpicDrawCards | EpicDiscardEnemy | EpicDestroyEnemy
  | EpicDolDrawResource | EpicDolDiscardToOne | EpicDolDiscardAlly
  | EpicDolEngageEnemies | EpicDolEngageEnemy | EpicDolSearchCard | EpicDolSearchAlly
  | EpicSendSetAsideHost GroupOrdinal UUID
  | EpicReceiveSetAsideHost UUID (Maybe EncounterCard)
  | EpicReceiveEnemy EpicEnemyTransfer EpicEnemyArrival
  | EpicSubmitDolClaim DolClaim
  | EpicDolStepDone DolEpoch (DolAck GroupOrdinal)
  | EpicResolveWithReply GroupOrdinal UUID EpicEffect
  | EpicAcknowledge UUID
  | EpicTransferLorienSupport GroupOrdinal PlayerId PlayerId
  | EpicReceiveLorienSupport EpicLorienSupportTransfer PlayerId
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

-- Only the two named encounter supports may be given between Dol Guldur teams.
-- Their entity and physical identities stay stable: this is a control change,
-- not entering or leaving play. Basic fields avoid entity/Criteria import cycles.
data EpicLorienSupportTransfer
  = EpicLorienHostTransfer
      { epicLorienAllyId :: AllyId
      , epicLorienCard :: EncounterCard
      , epicLorienOrigin :: GroupOrdinal
      , epicLorienController :: PlayerId
      , epicLorienOwner :: PlayerId
      , epicLorienTokens :: Tokens
      , epicLorienExhausted :: Bool
      , epicLorienCommitted :: Bool
      , epicLorienAttacking :: Bool
      , epicLorienFacedown :: [PlayerCard]
      , epicLorienPoison :: [PlayerCard]
      , epicLorienUnconscious :: Bool
      , epicLorienEffects :: [Effect]
      }
  | EpicLorienPowerTransfer
      { epicLorienObjectiveId :: ObjectiveId
      , epicLorienCard :: EncounterCard
      , epicLorienOrigin :: GroupOrdinal
      , epicLorienController :: PlayerId
      , epicLorienTokens :: Tokens
      , epicLorienExhausted :: Bool
      , epicLorienUnderneath :: [Either PlayerCard EncounterCard]
      , epicLorienEffects :: [Effect]
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
  , epicStageHealingAvailable :: Bool
  , epicStageSetAsideHosts :: Int
  , epicStageDrawingAvailable :: Bool
  , epicStagePlayers :: [EpicPlayer]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON EpicStage where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("epicStageHealingAvailable", Bool False), ("epicStageSetAsideHosts", toJSON (0 :: Int)), ("epicStageDrawingAvailable", Bool False), ("epicStagePlayers", toJSON ([] :: [EpicPlayer]))]

-- Public identities needed by explicitly cross-team player choices.
data EpicPlayer = EpicPlayer
  { epicPlayerId :: PlayerId
  , epicPlayerName :: Text
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
