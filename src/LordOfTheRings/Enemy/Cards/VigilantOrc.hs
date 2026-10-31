module LordOfTheRings.Enemy.Cards.VigilantOrc (vigilantOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype VigilantOrc = VigilantOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

vigilantOrc :: EnemyCard VigilantOrc
vigilantOrc = enemy VigilantOrc Cards.vigilantOrc

-- | Forced: After Vigilant Orc enters the staging area, it makes an immediate attack
-- against the first player. If this attack destroys a character, remove 1 time counter
-- from the current quest.
instance HasAbilities VigilantOrc where
  getAbilities (VigilantOrc a) = [mkAbility a 1 $ forced $ CardEntersStagingAreaWindow After (toTarget a)]

instance RunMessage VigilantOrc where
  runMessage msg e@(VigilantOrc attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      immediateAttack lead attrs.id
      push $ IfAttackDestroyed attrs.id [DoStep 1 msg]
      pure e
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure e
    _ -> VigilantOrc <$> runMessage msg attrs
