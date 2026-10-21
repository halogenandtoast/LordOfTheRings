module LordOfTheRings.Ally.Cards.GaladhonArcher (galadhonArcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GaladhonArcher = GaladhonArcher AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

galadhonArcher :: AllyCard GaladhonArcher
galadhonArcher = ally GaladhonArcher Cards.galadhonArcher

notEngagedWith :: PlayerId -> EnemyMatcher
notEngagedWith = not_ . EnemyEngagedWith . PlayerWithId

-- | Ranged. Response: After Galadhon Archer enters play, deal 1 damage to an enemy not
-- engaged with you.
instance HasAbilities GaladhonArcher where
  getAbilities (GaladhonArcher a) =
    [restricted a 1 (exists $ notEngagedWith a.owner) $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage GaladhonArcher where
  runMessage msg s@(GaladhonArcher a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (notEngagedWith pid)
      chooseOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \e -> dealDamage a e 1
      pure s
    _ -> GaladhonArcher <$> runMessage msg a
