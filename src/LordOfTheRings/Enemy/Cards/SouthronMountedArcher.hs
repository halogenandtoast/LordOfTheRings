module LordOfTheRings.Enemy.Cards.SouthronMountedArcher (southronMountedArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (InVictoryDisplay, Unplaced))
import LordOfTheRings.Trait (Trait (Battleground))

newtype SouthronMountedArcher = SouthronMountedArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronMountedArcher :: EnemyCard SouthronMountedArcher
southronMountedArcher = enemy SouthronMountedArcher Cards.southronMountedArcher

-- | Archery 5.
-- Damage from each round's archery total may be assigned to Battleground locations, as
-- well as characters.
instance HasModifiersFor SouthronMountedArcher where
  getModifiersFor (SouthronMountedArcher a) =
    when (a.placement `notElem` [InVictoryDisplay, Unplaced]) do
      battlegrounds <- select (LocationWithTrait Battleground)
      modified a GameTarget [ArcheryDamageMayGoTo (toTarget l) | l <- battlegrounds]

instance RunMessage SouthronMountedArcher where
  runMessage msg (SouthronMountedArcher attrs) = SouthronMountedArcher <$> runMessage msg attrs
