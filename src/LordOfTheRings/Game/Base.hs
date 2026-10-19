{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Game.Base (module LordOfTheRings.Game.Base) where

import Control.Lens
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Entities
import LordOfTheRings.Id
import LordOfTheRings.Target
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Type

data Game = Game
  { players :: Map PlayerId Player
  , activePlayer :: PlayerId
  , firstPlayer :: PlayerId
  , entities :: Entities
  , scenario :: Scenario
  , question :: Map PlayerId (Question Message)
  , questionVersion :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

entitiesL :: Lens' Game Entities
entitiesL = lens (.entities) (\g e -> g {entities = e})

data CostTarget = CostCardTarget Card
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Cost = ResourceCost Sphere CardCost | Free
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ActiveCost = ActiveCost
  { id :: CostId
  , cost :: Cost
  , target :: CostTarget
  , player :: PlayerId
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Player = Player
  { id :: PlayerId
  , hand :: [Card]
  , deck :: [Card]
  , threat :: Int
  , cost :: Maybe ActiveCost
  , name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Targetable Player where
  toTarget p = PlayerTarget p.id
