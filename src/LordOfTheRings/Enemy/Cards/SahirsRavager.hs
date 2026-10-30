module LordOfTheRings.Enemy.Cards.SahirsRavager (sahirsRavager) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SahirsRavager = SahirsRavager EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sahirsRavager :: EnemyCard SahirsRavager
sahirsRavager = enemy SahirsRavager Cards.sahirsRavager

-- | Forced: After Sahír's Ravager attacks, place 1 resource on it. Then, deal X damage to
-- the Aflame location in play with the lowest Aflame value, where X is the number of
-- resources on Sahír's Ravager.
instance HasAbilities SahirsRavager where
  getAbilities (SahirsRavager a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage SahirsRavager where
  runMessage msg e@(SahirsRavager attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      dealDamageToLowestAflame attrs (countTokens Resource attrs.enemyTokens + 1)
      pure e
    _ -> SahirsRavager <$> runMessage msg attrs
