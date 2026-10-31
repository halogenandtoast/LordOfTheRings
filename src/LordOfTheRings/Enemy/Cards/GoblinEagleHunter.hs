module LordOfTheRings.Enemy.Cards.GoblinEagleHunter (goblinEagleHunter) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel.Helpers (getWilyador)

newtype GoblinEagleHunter = GoblinEagleHunter EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinEagleHunter :: EnemyCard GoblinEagleHunter
goblinEagleHunter = enemy GoblinEagleHunter Cards.goblinEagleHunter

-- | Goblin Eagle-hunter gets +1 hit point per player.
instance HasModifiersFor GoblinEagleHunter where
  getModifiersFor (GoblinEagleHunter a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [HitPointModifier players]

instance HasAbilities GoblinEagleHunter where
  getAbilities (GoblinEagleHunter a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase]

-- | Forced: At the end of the combat phase, deal 1 damage to Wilyador.
-- Shadow: If Wilyador has 10 or less damage on him, deal 2 damage to Wilyador.
instance RunMessage GoblinEagleHunter where
  runMessage msg e@(GoblinEagleHunter attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getWilyador >>= traverse_ \w -> dealDamage attrs w 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      getWilyador >>= traverse_ \w -> do
        damage <- getDamage (toTarget w)
        when (damage <= 10) $ dealDamage (EnemySource eid) w 2
      pure e
    _ -> GoblinEagleHunter <$> runMessage msg attrs
