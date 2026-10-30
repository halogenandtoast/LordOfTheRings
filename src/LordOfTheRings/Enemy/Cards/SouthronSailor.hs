module LordOfTheRings.Enemy.Cards.SouthronSailor (southronSailor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype SouthronSailor = SouthronSailor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronSailor :: EnemyCard SouthronSailor
southronSailor = enemy SouthronSailor Cards.southronSailor

-- | While Southron Sailor is engaged with you, each Ship enemy engaged with you contributes
-- its threat to the total threat in the staging area.
instance HasModifiersFor SouthronSailor where
  getModifiersFor (SouthronSailor a) = case a.placement of
    EngagedWith pid -> modified a GameTarget [EngagedEnemiesAddThreat (shipEnemy <> EnemyEngagedWith (PlayerWithId pid))]
    _ -> pure ()
