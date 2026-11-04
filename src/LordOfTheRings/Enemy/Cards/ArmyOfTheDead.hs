module LordOfTheRings.Enemy.Cards.ArmyOfTheDead (armyOfTheDead) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (attachOvercomeByFearOrDiscardAlly)

newtype ArmyOfTheDead = ArmyOfTheDead EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Phantom. Victory 6. Defeated in campaign mode, it earns its boon side (the scenario's
-- resolution).
armyOfTheDead :: EnemyCard ArmyOfTheDead
armyOfTheDead = enemy ArmyOfTheDead Cards.armyOfTheDead

-- | Immune to player card effects. X is twice the number of players in the game.
instance HasModifiersFor ArmyOfTheDead where
  getModifiersFor (ArmyOfTheDead a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ImmuneToPlayerCardEffects, ThreatModifier (2 * players)]

-- | Forced: When Army of the Dead attacks, you must either attach a set aside Overcome by
-- Fear to your threat dial, or discard an ally you control.
instance HasAbilities ArmyOfTheDead where
  getAbilities (ArmyOfTheDead a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage ArmyOfTheDead where
  runMessage msg e@(ArmyOfTheDead attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid ->
        attachOvercomeByFearOrDiscardAlly attrs pid "armyOfTheDead"
      pure e
    _ -> ArmyOfTheDead <$> runMessage msg attrs
