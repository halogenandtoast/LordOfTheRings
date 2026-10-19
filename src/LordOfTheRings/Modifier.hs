module LordOfTheRings.Modifier (module LordOfTheRings.Modifier) where

import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Sphere
import LordOfTheRings.Trait

data ModifierType
  = WillpowerModifier Int
  | AttackModifier Int
  | DefenseModifier Int
  | HitPointModifier Int
  | ThreatModifier Int
  | EngagementCostModifier Int
  | GainTrait Trait
  | GainResourceIcon Sphere
  | ReturnToHandAtEndOfPhase
  | ShuffleIntoDeckAtEndOfPhase
  | -- | a card in the staging area adds nothing to the staging total
    DoesNotContributeThreat
  | CannotAttack
  | -- | when attacking an enemy with the trait, attack is raised by this much
    AttackBonusAgainst Trait Int
  | -- | the hero may attack an enemy in the staging area when attacking alone
    CanAttackStagingAreaAlone
  | -- | on a player: the enemies engaged with them cannot attack them
    EngagedEnemiesCannotAttack
  | -- | on a player: they may declare any number of defenders against each attack
    MayDeclareMultipleDefenders
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
