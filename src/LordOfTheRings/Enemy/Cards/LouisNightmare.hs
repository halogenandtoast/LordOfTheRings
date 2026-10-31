module LordOfTheRings.Enemy.Cards.LouisNightmare (louisNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype LouisNightmare = LouisNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | While Louis is in the victory display, each Troll enemy gains, "Forced: After this
-- enemy attacks, the defending player must raise his threat by 1." (Conflict at the
-- Carrock reads the victory display.)
louisNightmare :: EnemyCard LouisNightmare
louisNightmare = enemy LouisNightmare Cards.louisNightmare

instance HasAbilities LouisNightmare where
  getAbilities (LouisNightmare a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Forced: After Louis attacks, the defending player must raise his threat by 3.
instance RunMessage LouisNightmare where
  runMessage msg e@(LouisNightmare attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyAttacking eid pid) <- windows, eid == attrs.id] \pid ->
        encounterRaisesThreat pid 3
      pure e
    _ -> LouisNightmare <$> runMessage msg attrs
