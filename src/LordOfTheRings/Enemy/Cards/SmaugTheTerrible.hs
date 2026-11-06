module LordOfTheRings.Enemy.Cards.SmaugTheTerrible (smaugTheTerrible) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SmaugTheTerrible = SmaugTheTerrible EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

smaugTheTerrible :: EnemyCard SmaugTheTerrible
smaugTheTerrible = enemy SmaugTheTerrible Cards.smaugTheTerrible

instance HasModifiersFor SmaugTheTerrible where
  getModifiersFor (SmaugTheTerrible a) = modifySelf a [ImmuneToPlayerCardEffects]

instance RunMessage SmaugTheTerrible where
  runMessage msg e@(SmaugTheTerrible a) = case msg of
    WhenRevealed (isTarget a -> True) -> e <$ (resolveBurnDamage a)
    _ -> SmaugTheTerrible <$> runMessage msg a
