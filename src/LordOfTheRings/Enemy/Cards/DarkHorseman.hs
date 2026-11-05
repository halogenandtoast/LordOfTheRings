module LordOfTheRings.Enemy.Cards.DarkHorseman (darkHorseman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Morgul))

newtype DarkHorseman = DarkHorseman EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkHorseman :: EnemyCard DarkHorseman
darkHorseman = enemy DarkHorseman Cards.darkHorseman

instance HasModifiersFor DarkHorseman where
  getModifiersFor (DarkHorseman a) = do
    modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]
    case a.placement of
      EngagedWith pid -> modified a pid [HideTestWillpower (-1)]
      _ -> pure ()

instance RunMessage DarkHorseman where
  runMessage msg (DarkHorseman a) = DarkHorseman <$> runMessage msg a
