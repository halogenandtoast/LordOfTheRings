module LordOfTheRings.Source (module LordOfTheRings.Source) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Target

data Source
  = AbilitySource Source Int
  | HeroSource HeroId
  | AllySource AllyId
  | EnemySource EnemyId
  | LocationSource LocationId
  | AttachmentSource AttachmentId
  | TreacherySource TreacheryId
  | ObjectiveSource ObjectiveId
  | QuestSource QuestId
  | PlayerSource PlayerId
  | CardIdSource CardId
  | ScenarioSource
  | GameSource
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The card a source is, when it is one on the board.
sourceTarget :: Source -> Maybe Target
sourceTarget = \case
  AbilitySource s _ -> sourceTarget s
  HeroSource i -> Just $ HeroTarget i
  AllySource i -> Just $ AllyTarget i
  EnemySource i -> Just $ EnemyTarget i
  LocationSource i -> Just $ LocationTarget i
  AttachmentSource i -> Just $ AttachmentTarget i
  TreacherySource i -> Just $ TreacheryTarget i
  ObjectiveSource i -> Just $ ObjectiveTarget i
  QuestSource i -> Just $ QuestTarget i
  CardIdSource i -> Just $ CardIdTarget i
  PlayerSource _ -> Nothing
  ScenarioSource -> Nothing
  GameSource -> Nothing

class Sourceable a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource

instance Sourceable Source where
  toSource = id
