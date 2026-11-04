module LordOfTheRings.Enemy.Cards.WarMumak (warMumak) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WarMumak = WarMumak EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Archery 2.
warMumak :: EnemyCard WarMumak
warMumak = enemy WarMumak Cards.warMumak

-- | Cannot have attachments or take more than 3 damage each round.
instance HasModifiersFor WarMumak where
  getModifiersFor (WarMumak a) = modifySelf a [CannotHaveAttachments, DamageLimitEachRound 3]

-- | Forced: When War Mûmak is defeated, reduce each player's threat by 4.
instance HasAbilities WarMumak where
  getAbilities (WarMumak a) = [mkAbility a 1 $ forced $ ThisEnemyDefeatedWindow When a.id]

instance RunMessage WarMumak where
  runMessage msg e@(WarMumak attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> lowerThreat p 4
      pure e
    _ -> WarMumak <$> runMessage msg attrs
