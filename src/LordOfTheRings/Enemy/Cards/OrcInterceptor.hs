module LordOfTheRings.Enemy.Cards.OrcInterceptor (orcInterceptor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcInterceptor = OrcInterceptor EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcInterceptor :: EnemyCard OrcInterceptor
orcInterceptor = enemy OrcInterceptor Cards.orcInterceptor

-- | Orc Interceptor gets +1 threat and +1 attack for each resource token on it.
instance HasModifiersFor OrcInterceptor where
  getModifiersFor (OrcInterceptor a) = do
    let n = countTokens Resource a.enemyTokens
    when (n > 0) $ modifySelf a [ThreatModifier n, AttackModifier n]

instance HasAbilities OrcInterceptor where
  getAbilities (OrcInterceptor a) = [mkAbility a 1 $ forced $ AnyExploredWindow After]

-- | Forced: After a location leaves play as an explored location, place a resource token
-- on Orc Interceptor.
instance RunMessage OrcInterceptor where
  runMessage msg e@(OrcInterceptor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure e
    _ -> OrcInterceptor <$> runMessage msg attrs
