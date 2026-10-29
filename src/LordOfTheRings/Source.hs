module LordOfTheRings.Source (module LordOfTheRings.Source) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Target

data Source
  = AbilitySource Source Int
  | HeroSource HeroId
  | AllySource AllyId
  | EnemySource EnemyId
  | -- | the damage an enemy's attack deals, as Arkham's EnemyAttackSource: not its other
    -- effects, nor its shadow card's
    EnemyAttackSource EnemyId
  | LocationSource LocationId
  | AttachmentSource AttachmentId
  | TreacherySource TreacheryId
  | ObjectiveSource ObjectiveId
  | QuestSource QuestId
  | PlayerSource PlayerId
  | -- | the damage the player's attack on an enemy deals
    PlayerAttackSource PlayerId
  | CardIdSource CardId
  | ScenarioSource
  | GameSource
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Whether the source is a player card: a hero, ally, attachment or event (an event is
-- its card id).
isPlayerCardSource :: Source -> Bool
isPlayerCardSource = \case
  AbilitySource s _ -> isPlayerCardSource s
  HeroSource _ -> True
  AllySource _ -> True
  AttachmentSource _ -> True
  CardIdSource _ -> True
  _ -> False

-- | The card a source is, when it is one on the board.
sourceTarget :: Source -> Maybe Target
sourceTarget = \case
  AbilitySource s _ -> sourceTarget s
  HeroSource i -> Just $ HeroTarget i
  AllySource i -> Just $ AllyTarget i
  EnemySource i -> Just $ EnemyTarget i
  EnemyAttackSource i -> Just $ EnemyTarget i
  LocationSource i -> Just $ LocationTarget i
  AttachmentSource i -> Just $ AttachmentTarget i
  TreacherySource i -> Just $ TreacheryTarget i
  ObjectiveSource i -> Just $ ObjectiveTarget i
  QuestSource i -> Just $ QuestTarget i
  CardIdSource i -> Just $ CardIdTarget i
  PlayerSource _ -> Nothing
  PlayerAttackSource _ -> Nothing
  ScenarioSource -> Nothing
  GameSource -> Nothing

-- | The enemy dealing damage from this source: by its attack, or by another of its effects.
sourceEnemy :: Source -> Maybe EnemyId
sourceEnemy = \case
  EnemySource i -> Just i
  EnemyAttackSource i -> Just i
  _ -> Nothing

class Sourceable a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource

instance Sourceable Source where
  toSource = id
