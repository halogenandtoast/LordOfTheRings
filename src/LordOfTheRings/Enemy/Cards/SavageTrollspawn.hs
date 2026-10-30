module LordOfTheRings.Enemy.Cards.SavageTrollspawn (savageTrollspawn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (damagedCharactersOf)

newtype SavageTrollspawn = SavageTrollspawn EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

savageTrollspawn :: EnemyCard SavageTrollspawn
savageTrollspawn = enemy SavageTrollspawn Cards.savageTrollspawn

-- | Savage Trollspawn gets +1 attack for each damaged character the engaged player controls.
instance HasModifiersFor SavageTrollspawn where
  getModifiersFor (SavageTrollspawn a) = for_ (placementController a.placement) \pid -> do
    damaged <- length <$> damagedCharactersOf pid
    when (damaged > 0) $ modified a a [AttackModifier damaged]

-- | Shadow: Attacking enemy gets +1 attack for each damaged character the defending player
-- controls.
instance RunMessage SavageTrollspawn where
  runMessage msg e@(SavageTrollspawn attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid . length =<< damagedCharactersOf pid
      pure e
    _ -> SavageTrollspawn <$> runMessage msg attrs
