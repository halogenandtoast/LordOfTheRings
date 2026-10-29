module LordOfTheRings.Enemy.Cards.SpiritOfTheWild (spiritOfTheWild) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (controlledKeys)

newtype SpiritOfTheWild = SpiritOfTheWild EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

spiritOfTheWild :: EnemyCard SpiritOfTheWild
spiritOfTheWild = enemy SpiritOfTheWild Cards.spiritOfTheWild

-- | Cannot have attachments. Spirit of the Wild gets +1 threat and +1 attack for each Key
-- objective the players control.
instance HasModifiersFor SpiritOfTheWild where
  getModifiersFor (SpiritOfTheWild a) = do
    held <- length <$> controlledKeys
    modified a a $ CannotHaveAttachments : [m | held > 0, m <- [ThreatModifier held, AttackModifier held]]

-- | Shadow: Attacking enemy gets +1 attack for each Key objective the players control.
instance RunMessage SpiritOfTheWild where
  runMessage msg e@(SpiritOfTheWild attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      held <- length <$> controlledKeys
      when (held > 0) $ enemyGetsAttack attrs eid held
      pure e
    _ -> SpiritOfTheWild <$> runMessage msg attrs
