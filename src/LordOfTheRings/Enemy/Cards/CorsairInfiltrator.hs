module LordOfTheRings.Enemy.Cards.CorsairInfiltrator (corsairInfiltrator) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype CorsairInfiltrator = CorsairInfiltrator EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairInfiltrator :: EnemyCard CorsairInfiltrator
corsairInfiltrator = enemy CorsairInfiltrator Cards.corsairInfiltrator

-- | Forced: When Corsair Infiltrator attacks, deal it 1 additional shadow card for each
-- resource on it.
-- Forced: After the engaged player collects resources at the beginning of the resource
-- phase, that player moves 1 resource from one of his heroes' pools to Corsair
-- Infiltrator.
instance HasAbilities CorsairInfiltrator where
  getAbilities (CorsairInfiltrator a) =
    [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id | countTokens Resource a.enemyTokens > 0]
      <> [mkAbility a 2 $ forced $ PhaseBeginsWindow After ResourcePhase | EngagedWith _ <- [a.placement]]

instance RunMessage CorsairInfiltrator where
  runMessage msg e@(CorsairInfiltrator attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      replicateM_ (countTokens Resource attrs.enemyTokens) $ dealShadowCard attrs.id
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] (takeResourceFromAHero attrs)
      pure e
    _ -> CorsairInfiltrator <$> runMessage msg attrs
