{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Game.Base (module LordOfTheRings.Game.Base) where

import LordOfTheRings.ActiveCost
import LordOfTheRings.Effect
import LordOfTheRings.Entities
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Types
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
  }
  deriving stock (Show, Generic)

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
