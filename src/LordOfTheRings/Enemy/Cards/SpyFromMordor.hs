module LordOfTheRings.Enemy.Cards.SpyFromMordor (spyFromMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SpyFromMordor = SpyFromMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

spyFromMordor :: EnemyCard SpyFromMordor
spyFromMordor = enemy SpyFromMordor Cards.spyFromMordor

-- | Forced: When Spy from Mordor attacks, remove 1 time counter from the current quest.
instance HasAbilities SpyFromMordor where
  getAbilities (SpyFromMordor a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage SpyFromMordor where
  runMessage msg e@(SpyFromMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure e
    _ -> SpyFromMordor <$> runMessage msg attrs
