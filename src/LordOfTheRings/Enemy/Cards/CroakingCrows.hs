module LordOfTheRings.Enemy.Cards.CroakingCrows (croakingCrows) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (placeProgressOnSmaugTheGolden)

newtype CroakingCrows = CroakingCrows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge. Its burgle effect is read by burgle attempts only.
croakingCrows :: EnemyCard CroakingCrows
croakingCrows = enemy CroakingCrows Cards.croakingCrows

-- | Forced: When a location is revealed from the encounter deck, put 1 progress on Smaug
-- the Golden.
instance HasAbilities CroakingCrows where
  getAbilities (CroakingCrows a) = [mkAbility a 1 $ forced $ LocationRevealedWindow When]

instance RunMessage CroakingCrows where
  runMessage msg e@(CroakingCrows attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgressOnSmaugTheGolden attrs 1
      pure e
    _ -> CroakingCrows <$> runMessage msg attrs
