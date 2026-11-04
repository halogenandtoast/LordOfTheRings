module LordOfTheRings.Enemy.Cards.CityGuard (cityGuard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (ulfastX)

newtype CityGuard = CityGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cityGuard :: EnemyCard CityGuard
cityGuard = enemy CityGuard Cards.cityGuard

{- | Immune to player card effects. Indestructible. X is 1 more than the number of resources
on the main quest: its threat.
City Guard cannot leave the staging area but is considered to be engaged with each player
whose threat is equal to or higher than its engagement cost. It attacks each of them as an
enemy engaged with him would, and they may attack it.
-}
instance HasModifiersFor CityGuard where
  getModifiersFor (CityGuard a) = do
    x <- ulfastX
    modifySelf
      a
      [ ImmuneToPlayerCardEffects
      , ThreatModifier x
      , CannotLeaveStagingArea
      , NoEngagementChecks
      , CannotBeOptionallyEngagedBy AnyPlayer
      , EngagedWithEachPlayer (PlayerWithThreatAtLeastEngagementCostOf a.id)
      ]

instance RunMessage CityGuard where
  runMessage msg (CityGuard attrs) = CityGuard <$> runMessage msg attrs
