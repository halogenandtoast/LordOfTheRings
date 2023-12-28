{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Game.Base where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question

class (Monad m) => HasGame m where
  getGame :: m Game

data Game = Game {players :: Map PlayerId Player, activePlayer :: PlayerId}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CostTarget = CostCardTarget SomeCard
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ActiveCost = ActiveCost
  { id :: CostId
  , cost :: Cost
  , target :: CostTarget
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Player = Player
  { id :: PlayerId
  , heroes :: [Hero]
  , hand :: [SomeCard]
  , deck :: [SomeCard]
  , threat :: Int
  , question :: Maybe (Question Message)
  , cost :: Maybe ActiveCost
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
