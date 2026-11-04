module LordOfTheRings.Enemy.Cards.DragonHatchling (dragonHatchling) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (dragonSignsInVictoryDisplay)

newtype DragonHatchling = DragonHatchling EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dragonHatchling :: EnemyCard DragonHatchling
dragonHatchling = enemy DragonHatchling Cards.dragonHatchling

-- | Dragon Hatchling gets +1 attack and +1 defense for each Dragon Sign in the victory
-- display.
instance HasModifiersFor DragonHatchling where
  getModifiersFor (DragonHatchling a) = do
    x <- dragonSignsInVictoryDisplay
    when (x > 0) $ modifySelf a [AttackModifier x, DefenseModifier x]

-- | Shadow: Until the end of the phase, attacking enemy gets +1 attack and +1 defense for
-- each Dragon Sign in the victory display, counted as the shadow resolves.
instance RunMessage DragonHatchling where
  runMessage msg e@(DragonHatchling attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- dragonSignsInVictoryDisplay
      when (x > 0) $ phaseModifiers (CardIdSource cid) eid [AttackModifier x, DefenseModifier x]
      pure e
    _ -> DragonHatchling <$> runMessage msg attrs
