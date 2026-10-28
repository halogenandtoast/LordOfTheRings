module LordOfTheRings.Enemy.Cards.ZealousTraitor (zealousTraitor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (wasOptionallyEngaged)

newtype ZealousTraitor = ZealousTraitor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

zealousTraitor :: EnemyCard ZealousTraitor
zealousTraitor = enemy ZealousTraitor Cards.zealousTraitor

-- | Forced: When Zealous Traitor engages a player, that player must deal 1 damage to each
-- ally he controls. (2 damage instead if Zealous Traitor was not optionally engaged.)
instance HasAbilities ZealousTraitor where
  getAbilities (ZealousTraitor a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage ZealousTraitor where
  runMessage msg e@(ZealousTraitor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        opted <- wasOptionallyEngaged attrs.id pid
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        for_ allies \ally -> dealDamage attrs ally (if opted then 1 else 2)
      pure e
    _ -> ZealousTraitor <$> runMessage msg attrs
