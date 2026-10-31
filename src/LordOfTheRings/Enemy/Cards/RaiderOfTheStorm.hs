module LordOfTheRings.Enemy.Cards.RaiderOfTheStorm (raiderOfTheStorm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype RaiderOfTheStorm = RaiderOfTheStorm EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

raiderOfTheStorm :: EnemyCard RaiderOfTheStorm
raiderOfTheStorm = enemy RaiderOfTheStorm Cards.raiderOfTheStorm

-- | While Raider of the Storm is engaged with you, each Ship enemy engaged with you gets +1
-- threat, +1 attack, +1 defense, and contributes its threat to the total threat in the
-- staging area.
instance HasModifiersFor RaiderOfTheStorm where
  getModifiersFor (RaiderOfTheStorm a) = case a.placement of
    EngagedWith pid -> do
      let ships = shipEnemy <> EnemyEngagedWith (PlayerWithId pid)
      engaged <- select ships
      for_ engaged \s -> modified a s [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]
      modified a GameTarget [EngagedEnemiesAddThreat ships]
    _ -> pure ()

instance RunMessage RaiderOfTheStorm where
  runMessage msg (RaiderOfTheStorm attrs) = RaiderOfTheStorm <$> runMessage msg attrs
