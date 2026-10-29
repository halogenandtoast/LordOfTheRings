module LordOfTheRings.Enemy.Cards.OrcHound (orcHound) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcHound = OrcHound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcHound :: EnemyCard OrcHound
orcHound = enemy OrcHound Cards.orcHound

-- | Surge.
-- Forced: After Orc Hound engages a player, that player must exhaust a character he controls.
instance HasAbilities OrcHound where
  getAbilities (OrcHound a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage OrcHound where
  runMessage msg e@(OrcHound attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> chooseAndExhaust attrs pid 1
      pure e
    _ -> OrcHound <$> runMessage msg attrs
