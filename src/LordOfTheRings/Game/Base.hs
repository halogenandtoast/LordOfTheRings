{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Game.Base (module LordOfTheRings.Game.Base) where

import Control.Lens
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.Type
import LordOfTheRings.Card
import LordOfTheRings.Enemy.Type
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id
import LordOfTheRings.Target
import LordOfTheRings.Location.Type
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Type

data Entities = Entities
  { allies :: Map AllyId Ally
  , heroes :: Map HeroId Hero
  , enemies :: Map EnemyId Enemy
  , locations :: Map LocationId Location
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

alliesL :: Lens' Entities (Map AllyId Ally)
alliesL = lens (.allies) (\e a -> e {allies = a})

heroesL :: Lens' Entities (Map HeroId Hero)
heroesL = lens (.heroes) (\e h -> e {heroes = h})

locationsL :: Lens' Entities (Map LocationId Location)
locationsL = lens (.locations) (\e h -> e {locations = h})

enemiesL :: Lens' Entities (Map EnemyId Enemy)
enemiesL = lens (.enemies) (\e h -> e {enemies = h})

instance Monoid Entities where
  mempty = Entities mempty mempty mempty mempty

instance Semigroup Entities where
  (Entities a1 h1 e1 l1) <> (Entities a2 h2 e2 l2) =
    Entities (a1 <> a2) (h1 <> h2) (e1 <> e2) (l1 <> l2)

data Game = Game
  { players :: Map PlayerId Player
  , activePlayer :: PlayerId
  , firstPlayer :: PlayerId
  , entities :: Entities
  , scenario :: Scenario
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
  , question :: Maybe (Question Message)
  , cost :: Maybe ActiveCost
  , name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Targetable Player where
  toTarget p = PlayerTarget p.id
