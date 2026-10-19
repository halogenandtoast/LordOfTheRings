module LordOfTheRings.Hero.Cards.Legolas (legolas) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Legolas = Legolas HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

legolas :: HeroCard Legolas
legolas = hero Legolas Cards.legolas

-- | Ranged. Response: After Legolas participates in an attack that destroys an enemy,
-- place 2 progress tokens on the current quest.
instance HasAbilities Legolas where
  getAbilities (Legolas a) = [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just $ toTarget a)]

instance RunMessage Legolas where
  runMessage msg h@(Legolas attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      quest <- getCurrentQuest
      pushAll [PlaceTokens (toSource attrs) q Progress 2 | q <- toList quest]
      pure h
    _ -> Legolas <$> runMessage msg attrs
