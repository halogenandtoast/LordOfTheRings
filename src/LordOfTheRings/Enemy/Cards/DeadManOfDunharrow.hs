module LordOfTheRings.Enemy.Cards.DeadManOfDunharrow (deadManOfDunharrow) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (attachOvercomeByFearOr)

newtype DeadManOfDunharrow = DeadManOfDunharrow EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Phantom.
deadManOfDunharrow :: EnemyCard DeadManOfDunharrow
deadManOfDunharrow = enemy DeadManOfDunharrow Cards.deadManOfDunharrow

-- | Forced: After Dead Man of Dunharrow engages you, either attach a set aside Overcome by
-- Fear to your threat dial, or raise your threat by 2.
instance HasAbilities DeadManOfDunharrow where
  getAbilities (DeadManOfDunharrow a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack.
instance RunMessage DeadManOfDunharrow where
  runMessage msg e@(DeadManOfDunharrow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        attachOvercomeByFearOr attrs pid "deadManOfDunharrow" True $ encounterRaisesThreat pid 2
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      pure e
    _ -> DeadManOfDunharrow <$> runMessage msg attrs
