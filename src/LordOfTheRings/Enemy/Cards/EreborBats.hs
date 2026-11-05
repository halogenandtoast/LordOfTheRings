module LordOfTheRings.Enemy.Cards.EreborBats (ereborBats) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EreborBats = EreborBats EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborBats :: EnemyCard EreborBats
ereborBats = enemy EreborBats Cards.ereborBats

instance HasAbilities EreborBats where
  getAbilities (EreborBats a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage EreborBats where
  runMessage msg e@(EreborBats a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyAttacking eid pid) <- windows, eid == a.id] \pid -> discardRandomFromHand pid 1
      pure e
    _ -> EreborBats <$> runMessage msg a
