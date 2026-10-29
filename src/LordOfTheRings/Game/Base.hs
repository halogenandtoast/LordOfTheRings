{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Game.Base (module LordOfTheRings.Game.Base) where

import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally.Types (Ally)
import LordOfTheRings.Card (Card)
import LordOfTheRings.Effect
import LordOfTheRings.Entities
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Log.Block
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Types
import LordOfTheRings.Source (Source)
import LordOfTheRings.Target

data Game = Game
  { gamePlayers :: Map PlayerId Player
  , gamePlayerOrder :: [PlayerId]
  , gameActivePlayer :: PlayerId
  , gameFirstPlayer :: PlayerId
  , gameEntities :: Entities
  , gameScenario :: Scenario
  , gamePhase :: Phase
  , gameRound :: Int
  , gameQuestion :: Map PlayerId (Question Message)
  , gameQuestionVersion :: Int
  , gameActiveCosts :: Map CostId ActiveCost
  , gameModifiers :: Map Target [Modifier]
  , gameEffects :: [Effect]
  , gameOutcome :: Maybe Outcome
  , gameLastCommittedWillpower :: Maybe Int
  -- ^ What was committed at the last quest resolution, for the board's tally
  , gameLogBlocks :: [LogBlock]
  -- ^ The log's open blocks, outermost first. See "LordOfTheRings.Log.Block".
  , gameLeftPlay :: Map AllyId Ally
  -- ^ Allies that just left play, kept until the window for their leaving closes so
  -- their own abilities can answer it.
  , gameAttackDestroyed :: [AttackDestroyed]
  -- ^ What attacks' damage destroyed: an enemy's attack's, kept until that enemy attacks
  -- again, and a player's attack's on an enemy, until the attack has resolved.
  }
  deriving stock (Show, Generic)

{- | A card an attack's damage destroyed, as "if this attack destroys a character" reads:
the attack, as its damage's source ('EnemyAttackSource', or 'PlayerAttackSource' for an
attack on an enemy), the card destroyed and who controlled it, and the card itself, for
what reads it once it has left play (Desecrated Ruins, the ally's cost).
-}
data AttackDestroyed = AttackDestroyed
  { attackDestroyedBy :: Source
  , attackDestroyedTarget :: Target
  , attackDestroyedController :: Maybe PlayerId
  , attackDestroyedCard :: Card
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "target" AttackDestroyed Target where
  getField = attackDestroyedTarget

instance HasField "controller" AttackDestroyed (Maybe PlayerId) where
  getField = attackDestroyedController

instance HasField "card" AttackDestroyed Card where
  getField = attackDestroyedCard

makeLensesWith suffixedFields ''Game

instance ToJSON Game where
  toJSON = genericToJSON (aesonOptions $ Just "game")

instance HasField "players" Game (Map PlayerId Player) where
  getField = gamePlayers

instance HasField "playerOrder" Game [PlayerId] where
  getField = gamePlayerOrder

instance HasField "activePlayer" Game PlayerId where
  getField = gameActivePlayer

instance HasField "firstPlayer" Game PlayerId where
  getField = gameFirstPlayer

instance HasField "entities" Game Entities where
  getField = gameEntities

instance HasField "scenario" Game Scenario where
  getField = gameScenario

instance HasField "phase" Game Phase where
  getField = gamePhase

instance HasField "round" Game Int where
  getField = gameRound

instance HasField "question" Game (Map PlayerId (Question Message)) where
  getField = gameQuestion

instance HasField "questionVersion" Game Int where
  getField = gameQuestionVersion

instance HasField "outcome" Game (Maybe Outcome) where
  getField = gameOutcome
