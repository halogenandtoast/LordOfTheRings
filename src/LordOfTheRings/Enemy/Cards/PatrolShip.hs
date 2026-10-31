module LordOfTheRings.Enemy.Cards.PatrolShip (patrolShip) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

-- | Boarding 2. Its *Forced, as a Sailing test discards it, is The City of Corsairs'.
newtype PatrolShip = PatrolShip EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

patrolShip :: EnemyCard PatrolShip
patrolShip = enemy PatrolShip Cards.patrolShip

-- | Forced: After you shift your heading on-course, Patrol Ship gets -40 engagement cost
-- this round.
instance RunMessage PatrolShip where
  runMessage msg (PatrolShip attrs) = case msg of
    ShiftHeading _ ShiftOnCourse -> do
      roundModifier attrs attrs.id (EngagementCostModifier (-40))
      PatrolShip <$> runMessage msg attrs
    _ -> PatrolShip <$> runMessage msg attrs
