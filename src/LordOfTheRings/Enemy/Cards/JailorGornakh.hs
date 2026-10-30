module LordOfTheRings.Enemy.Cards.JailorGornakh (jailorGornakh) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype JailorGornakh = JailorGornakh EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

jailorGornakh :: EnemyCard JailorGornakh
jailorGornakh = enemy JailorGornakh Cards.jailorGornakh

-- | Forced: When 1 or more captured cards are rescued, Jailor Gornákh engages the player
-- with the highest threat and makes an immediate attack.
instance HasAbilities JailorGornakh where
  getAbilities (JailorGornakh a) = [mkAbility a 1 $ forced $ CapturedCardsRescuedWindow When]

instance RunMessage JailorGornakh where
  runMessage msg e@(JailorGornakh attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      engageHighestThreatAndAttack attrs attrs.id
      pure e
    _ -> JailorGornakh <$> runMessage msg attrs
