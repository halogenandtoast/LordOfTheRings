module LordOfTheRings.Ally.Cards.GreenwoodArcher (greenwoodArcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GreenwoodArcher = GreenwoodArcher AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greenwoodArcher :: AllyCard GreenwoodArcher
greenwoodArcher = ally GreenwoodArcher Cards.greenwoodArcher

-- | Ranged. Response: After Greenwood Archer enters play, ready a hero.
instance HasAbilities GreenwoodArcher where
  getAbilities (GreenwoodArcher a) = [restricted a 1 (exists ExhaustedHero) $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage GreenwoodArcher where
  runMessage msg s@(GreenwoodArcher a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select ExhaustedHero
      chooseOneM pid do
        questionLabeled "chooseHeroToReady"
        causedBy a
        targets heroes ready
      pure s
    _ -> GreenwoodArcher <$> runMessage msg a
