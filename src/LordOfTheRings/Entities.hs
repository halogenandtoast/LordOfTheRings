{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Entities (module LordOfTheRings.Entities) where

import LordOfTheRings.Ally.Types
import LordOfTheRings.Attachment.Types
import LordOfTheRings.Enemy.Types
import LordOfTheRings.Hero.Types
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Location.Types
import LordOfTheRings.Objective.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types
import LordOfTheRings.Treachery.Types

data Entities = Entities
  { entitiesHeroes :: Map HeroId Hero
  , entitiesAllies :: Map AllyId Ally
  , entitiesEnemies :: Map EnemyId Enemy
  , entitiesLocations :: Map LocationId Location
  , entitiesTreacheries :: Map TreacheryId Treachery
  , entitiesQuests :: Map QuestId Quest
  , entitiesAttachments :: Map AttachmentId Attachment
  , entitiesObjectives :: Map ObjectiveId Objective
  }
  deriving stock (Show, Generic)

makeLensesWith suffixedFields ''Entities

instance ToJSON Entities where
  toJSON = genericToJSON (aesonOptions $ Just "entities")

instance Semigroup Entities where
  Entities h1 a1 e1 l1 t1 q1 x1 o1 <> Entities h2 a2 e2 l2 t2 q2 x2 o2 =
    Entities (h1 <> h2) (a1 <> a2) (e1 <> e2) (l1 <> l2) (t1 <> t2) (q1 <> q2) (x1 <> x2) (o1 <> o2)

instance Monoid Entities where
  mempty = Entities mempty mempty mempty mempty mempty mempty mempty mempty

instance HasField "heroes" Entities (Map HeroId Hero) where
  getField = entitiesHeroes

instance HasField "allies" Entities (Map AllyId Ally) where
  getField = entitiesAllies

instance HasField "enemies" Entities (Map EnemyId Enemy) where
  getField = entitiesEnemies

instance HasField "locations" Entities (Map LocationId Location) where
  getField = entitiesLocations

instance HasField "treacheries" Entities (Map TreacheryId Treachery) where
  getField = entitiesTreacheries

instance HasField "quests" Entities (Map QuestId Quest) where
  getField = entitiesQuests

instance HasField "attachments" Entities (Map AttachmentId Attachment) where
  getField = entitiesAttachments

instance HasField "objectives" Entities (Map ObjectiveId Objective) where
  getField = entitiesObjectives
