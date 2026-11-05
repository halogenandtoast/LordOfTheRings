module LordOfTheRings.Enemy.Cards.MenacingWraith (menacingWraith) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Saga (oneRings)
import LordOfTheRings.Trait (Trait (Morgul))

newtype MenacingWraith = MenacingWraith EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

menacingWraith :: EnemyCard MenacingWraith
menacingWraith = enemy MenacingWraith Cards.menacingWraith

instance HasModifiersFor MenacingWraith where
  getModifiersFor (MenacingWraith a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]

instance HasAbilities MenacingWraith where
  getAbilities (MenacingWraith a) = [mkAbility a 1 $ forced $ AnyWindowOf [ObjectiveExhaustedWindow After code | code <- oneRings]]

instance RunMessage MenacingWraith where
  runMessage msg e@(MenacingWraith a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      engageEnemy lead a.id
      immediateAttack lead a.id
      pure e
    _ -> MenacingWraith <$> runMessage msg a
