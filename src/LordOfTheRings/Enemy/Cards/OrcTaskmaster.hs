module LordOfTheRings.Enemy.Cards.OrcTaskmaster (orcTaskmaster) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (addResourceToEachGoblin)
import LordOfTheRings.Trait (Trait (Goblin))

newtype OrcTaskmaster = OrcTaskmaster EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcTaskmaster :: EnemyCard OrcTaskmaster
orcTaskmaster = enemy OrcTaskmaster Cards.orcTaskmaster

-- | Forced: When a Goblin enemy with at least 1 resource on it is dealt any amount of
-- damage, remove 1 resource from that enemy and cancel all damage just dealt to it.
instance HasAbilities OrcTaskmaster where
  getAbilities (OrcTaskmaster a) =
    [mkAbility a 1 $ forced $ MatchingEnemyDealtDamageWindow When (EnemyWithTrait Goblin <> EnemyWithResources)]

-- | When Revealed: Add 1 resource to each Goblin enemy in play.
instance RunMessage OrcTaskmaster where
  runMessage msg e@(OrcTaskmaster attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addResourceToEachGoblin attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [eid | Window _ (DealtDamage (EnemyTarget eid) _) <- ws] \eid -> do
        removeTokens attrs eid Resource 1
        cancelDamage eid
      pure e
    _ -> OrcTaskmaster <$> runMessage msg attrs
