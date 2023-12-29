{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Game.Base where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Ally.Type
import LordOfTheRings.Card
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question

class (Monad m) => HasGame m where
  getGame :: m Game

data Entities = Entities
  { allies :: Map AllyId Ally
  , heroes :: Map HeroId Hero
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Monoid Entities where
  mempty = Entities mempty mempty

instance Semigroup Entities where
  (Entities a1 h1) <> (Entities a2 h2) = Entities (a1 <> a2) (h1 <> h2)

data Game = Game
  { players :: Map PlayerId Player
  , activePlayer :: PlayerId
  , entities :: Entities
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CostTarget = CostCardTarget SomeCard
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Cost = ResourceCost Sphere ResourceCost | Free
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
  , hand :: [SomeCard]
  , deck :: [SomeCard]
  , threat :: Int
  , question :: Maybe (Question Message)
  , cost :: Maybe ActiveCost
  , name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
