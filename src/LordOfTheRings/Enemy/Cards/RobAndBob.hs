module LordOfTheRings.Enemy.Cards.RobAndBob (robAndBob) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RobAndBob = RobAndBob EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

robAndBob :: EnemyCard RobAndBob
robAndBob = enemy RobAndBob Cards.robAndBob

instance HasAbilities RobAndBob where
  getAbilities (RobAndBob a) = [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Forced: After Rob & Bob makes its first attack each round, if there are two or more
-- players, it engages the next player and makes an additional attack (discard and deal a
-- new shadow card for this attack).
instance RunMessage RobAndBob where
  runMessage msg e@(RobAndBob attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyAttacking eid pid) <- windows, eid == attrs.id] \pid ->
        getNextPlayer pid >>= traverse_ \next -> do
          engageEnemy next attrs.id
          traverse_ (discardShadowCard attrs.id) attrs.shadowCards
          immediateAttack next attrs.id
      pure e
    _ -> RobAndBob <$> runMessage msg attrs
