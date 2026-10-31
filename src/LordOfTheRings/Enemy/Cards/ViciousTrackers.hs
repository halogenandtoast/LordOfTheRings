module LordOfTheRings.Enemy.Cards.ViciousTrackers (viciousTrackers) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ViciousTrackers = ViciousTrackers EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

viciousTrackers :: EnemyCard ViciousTrackers
viciousTrackers = enemy ViciousTrackers Cards.viciousTrackers

-- | While Vicious Trackers is engaged with a player, that player is considered to have +20
-- threat for purposes of engagement checks only: each other enemy's engagement cost is 20
-- lower against him.
instance HasModifiersFor ViciousTrackers where
  getModifiersFor (ViciousTrackers a) = for_ [pid | EngagedWith pid <- [a.placement]] \pid -> do
    others <- select (not_ $ EnemyWithId a.id)
    for_ others \o -> modified a o [EngagementCostAgainst pid (-20)]

instance RunMessage ViciousTrackers where
  runMessage msg (ViciousTrackers attrs) = ViciousTrackers <$> runMessage msg attrs
