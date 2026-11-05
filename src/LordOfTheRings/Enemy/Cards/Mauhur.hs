module LordOfTheRings.Enemy.Cards.Mauhur (mauhur) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype Mauhur = Mauhur EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mauhur :: EnemyCard Mauhur
mauhur = enemy Mauhur Cards.mauhur

instance HasAbilities Mauhur where
  getAbilities (Mauhur a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage Mauhur where
  runMessage msg e@(Mauhur a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      increasePursuit 1
      pure e
    _ -> Mauhur <$> runMessage msg a
