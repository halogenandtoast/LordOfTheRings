module LordOfTheRings.Location.Cards.SecondDeep (secondDeep) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers

newtype SecondDeep = SecondDeep LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

secondDeep :: LocationCard SecondDeep
secondDeep = location SecondDeep Cards.secondDeep

-- | While Durin's Bane has no damage on it, progress tokens cannot be placed on Second Deep.
-- ponytail: the progress is lost, not passed on to the quest; revisit if a ruling says otherwise
instance RunMessage SecondDeep where
  runMessage msg l@(SecondDeep attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      bane <- getDurinsBane
      undamaged <- maybe (pure False) (fmap (== 0) . getDamage . toTarget) bane
      if undamaged then pure l else SecondDeep <$> runMessage msg attrs
    _ -> SecondDeep <$> runMessage msg attrs
