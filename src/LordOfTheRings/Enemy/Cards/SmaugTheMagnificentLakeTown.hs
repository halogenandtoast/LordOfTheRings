module LordOfTheRings.Enemy.Cards.SmaugTheMagnificentLakeTown (smaugTheMagnificentLakeTown) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (lakeTownObjectives)

newtype SmaugTheMagnificentLakeTown = SmaugTheMagnificentLakeTown EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

smaugTheMagnificentLakeTown :: EnemyCard SmaugTheMagnificentLakeTown
smaugTheMagnificentLakeTown = enemy SmaugTheMagnificentLakeTown Cards.smaugTheMagnificentLakeTown

instance HasModifiersFor SmaugTheMagnificentLakeTown where
  getModifiersFor (SmaugTheMagnificentLakeTown a) = modifySelf a [ImmuneToPlayerCardEffects]

instance HasAbilities SmaugTheMagnificentLakeTown where
  getAbilities (SmaugTheMagnificentLakeTown a) = [mkAbility a 1 $ forced $ EnemyDealtExcessDamageWindow After a.id]

instance RunMessage SmaugTheMagnificentLakeTown where
  runMessage msg e@(SmaugTheMagnificentLakeTown a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      towns <- lakeTownObjectives
      damage <- min 3 <$> getDamage (toTarget a)
      for_ towns \oid -> moveDamage a a oid damage
      pure e
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      towns <- lakeTownObjectives
      for_ [n | Window _ (AttackDealtExcessDamage eid _ n) <- windows, eid == a.id] \n ->
        for_ towns \oid -> dealDamage a oid n
      pure e
    _ -> SmaugTheMagnificentLakeTown <$> runMessage msg a
