module LordOfTheRings.Enemy.Cards.OldStoneTroll (oldStoneTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootControlled)

newtype OldStoneTroll = OldStoneTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

oldStoneTroll :: EnemyCard OldStoneTroll
oldStoneTroll = enemy OldStoneTroll Cards.oldStoneTroll

instance HasModifiersFor OldStoneTroll where
  getModifiersFor (OldStoneTroll a) = do
    loot <- lootControlled
    players <- getPlayerOrder
    let others = case a.placement of EngagedWith pid -> filter (/= pid) players; _ -> players
    modifySelf a $ [CannotHavePlayerAttachments, EngagementCostModifier (-10 * loot)]
      <> map CannotBeAttackedByCharactersOf others

instance RunMessage OldStoneTroll where
  runMessage msg (OldStoneTroll a) = OldStoneTroll <$> runMessage msg a
