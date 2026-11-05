module LordOfTheRings.Enemy.Cards.WarriorOfDunland (warriorOfDunland) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WarriorOfDunland = WarriorOfDunland EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Archery 1, as its card def says.
warriorOfDunland :: EnemyCard WarriorOfDunland
warriorOfDunland = enemy WarriorOfDunland Cards.warriorOfDunland

-- | Forced: After Warrior of Dunland attacks and destroys a character, place 3 progress on
-- the active location.
instance HasAbilities WarriorOfDunland where
  getAbilities (WarriorOfDunland a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | When Revealed: Warrior of Dunland makes an immediate attack against you.
instance RunMessage WarriorOfDunland where
  runMessage msg e@(WarriorOfDunland attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      immediateAttack you attrs.id
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      traverse_ (\l -> placeProgress attrs l 3) =<< select IsActiveLocation
      pure e
    _ -> WarriorOfDunland <$> runMessage msg attrs
