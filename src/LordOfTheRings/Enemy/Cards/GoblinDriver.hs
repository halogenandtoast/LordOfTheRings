module LordOfTheRings.Enemy.Cards.GoblinDriver (goblinDriver) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinDriver = GoblinDriver EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinDriver :: EnemyCard GoblinDriver
goblinDriver = enemy GoblinDriver Cards.goblinDriver

-- | When Revealed: The first player exhausts 1 character he controls.
-- Shadow: Defending player exhausts 1 character he controls.
instance RunMessage GoblinDriver where
  runMessage msg e@(GoblinDriver attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseAndExhaust attrs lead 1
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure e
    _ -> GoblinDriver <$> runMessage msg attrs
