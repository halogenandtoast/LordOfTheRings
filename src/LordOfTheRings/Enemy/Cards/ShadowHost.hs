module LordOfTheRings.Enemy.Cards.ShadowHost (shadowHost) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (attachOvercomeByFearOr)

newtype ShadowHost = ShadowHost EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Phantom.
shadowHost :: EnemyCard ShadowHost
shadowHost = enemy ShadowHost Cards.shadowHost

-- | Forced: After Shadow Host engages you, either attach a set aside Overcome by Fear to your
-- threat dial, or it makes an immediate attack.
instance HasAbilities ShadowHost where
  getAbilities (ShadowHost a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage ShadowHost where
  runMessage msg e@(ShadowHost attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        attachOvercomeByFearOr attrs pid "shadowHost" True $ immediateAttack pid attrs.id
      pure e
    _ -> ShadowHost <$> runMessage msg attrs
