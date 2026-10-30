module LordOfTheRings.Enemy.Cards.PackLeader (packLeader) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Warg))

newtype PackLeader = PackLeader EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

packLeader :: EnemyCard PackLeader
packLeader = enemy PackLeader Cards.packLeader

-- | Cannot have attachments. Cannot be optionally engaged.
instance HasModifiersFor PackLeader where
  getModifiersFor (PackLeader a) = modified a a [CannotHaveAttachments, CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: After Pack Leader engages you, exhaust a character you control for each Warg
-- enemy engaged with you.
instance HasAbilities PackLeader where
  getAbilities (PackLeader a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage PackLeader where
  runMessage msg e@(PackLeader attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        wargs <- length <$> select (EnemyWithTrait Warg <> EnemyEngagedWith (PlayerWithId pid))
        when (wargs > 0) $ chooseAndExhaust attrs pid wargs
      pure e
    _ -> PackLeader <$> runMessage msg attrs
