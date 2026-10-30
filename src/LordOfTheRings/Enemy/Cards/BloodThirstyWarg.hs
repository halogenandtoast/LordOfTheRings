module LordOfTheRings.Enemy.Cards.BloodThirstyWarg (bloodThirstyWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype BloodThirstyWarg = BloodThirstyWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bloodThirstyWarg :: EnemyCard BloodThirstyWarg
bloodThirstyWarg = enemy BloodThirstyWarg Cards.bloodThirstyWarg

-- | While Blood-thirsty Warg is in the staging area, it gains: "Forced: When it becomes
-- Night, Blood-thirsty Warg engages the first player and makes an immediate attack."
instance HasAbilities BloodThirstyWarg where
  getAbilities (BloodThirstyWarg a) = [mkAbility a 1 $ forced $ BecomesWindow Nighttime | a.placement == StagingArea]

instance RunMessage BloodThirstyWarg where
  runMessage msg e@(BloodThirstyWarg attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      engageEnemy lead attrs.id
      immediateAttack lead attrs.id
      pure e
    _ -> BloodThirstyWarg <$> runMessage msg attrs
