module LordOfTheRings.Source (module LordOfTheRings.Source) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Source
  = AbilitySource Source Int
  | HeroSource HeroId
  | AllySource AllyId
  | EnemySource EnemyId
  | LocationSource LocationId
  | AttachmentSource AttachmentId
  | TreacherySource TreacheryId
  | QuestSource QuestId
  | PlayerSource PlayerId
  | CardIdSource CardId
  | ScenarioSource
  | GameSource
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

class Sourceable a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource

instance Sourceable Source where
  toSource = id
