module LordOfTheRings.Enemy.Cards.SmaugTheMightyLakeTown (smaugTheMightyLakeTown) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SmaugTheMightyLakeTown = SmaugTheMightyLakeTown EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

smaugTheMightyLakeTown :: EnemyCard SmaugTheMightyLakeTown
smaugTheMightyLakeTown = enemy SmaugTheMightyLakeTown Cards.smaugTheMightyLakeTown

instance HasModifiersFor SmaugTheMightyLakeTown where
  getModifiersFor (SmaugTheMightyLakeTown a) = modifySelf a [ImmuneToPlayerCardEffects]

instance RunMessage SmaugTheMightyLakeTown where
  runMessage msg e@(SmaugTheMightyLakeTown a) = case msg of
    WhenRevealed (isTarget a -> True) -> e <$ (eachPlayer \pid -> immediateAttack pid a.id)
    _ -> SmaugTheMightyLakeTown <$> runMessage msg a
