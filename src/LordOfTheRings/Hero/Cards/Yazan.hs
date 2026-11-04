module LordOfTheRings.Hero.Cards.Yazan (yazan) where

import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (eliminatedIfLeavesPlay)

newtype Yazan = Yazan HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Objective-hero. Ranged.
yazan :: HeroCard Yazan
yazan = hero Yazan Cards.yazan

-- | Response: After Yazan is declared as an attacker, deal 1 damage to a non-unique enemy
-- in play. (Limit once per phase.)
instance HasAbilities Yazan where
  getAbilities (Yazan a) =
    [limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ response $ AttackerDeclaredWindow After (Just $ toTarget a)]

-- | If Yazan leaves play, you are eliminated from the game.
instance RunMessage Yazan where
  runMessage msg h@(Yazan attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select (not_ UniqueEnemy)
      chooseOrRunOneM pid do
        questionLabeled "yazan.damage"
        causedBy attrs
        targets enemies \e -> dealDamage attrs e 1
      pure h
    _ -> do
      eliminatedIfLeavesPlay attrs msg
      Yazan <$> runMessage msg attrs
