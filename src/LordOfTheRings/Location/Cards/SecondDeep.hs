module LordOfTheRings.Location.Cards.SecondDeep (secondDeep) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers

newtype SecondDeep = SecondDeep LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

secondDeep :: LocationCard SecondDeep
secondDeep = location SecondDeep Cards.secondDeep

-- | While Durin's Bane has no damage on it, progress tokens cannot be placed on Second Deep.
-- The progress questing would place on it is lost, not passed on to the quest: the active
-- location passes on only the progress past its quest points, once explored, and Second
-- Deep cannot be explored while it takes none.
instance HasModifiersFor SecondDeep where
  getModifiersFor (SecondDeep a) = whenM undamagedBane $ modified a a [ProgressLimit 0]

-- the limit bars the questing progress; this, any other placed on it
instance RunMessage SecondDeep where
  runMessage msg l@(SecondDeep attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      undamaged <- undamagedBane
      if undamaged then pure l else SecondDeep <$> runMessage msg attrs
    _ -> SecondDeep <$> runMessage msg attrs

undamagedBane :: HasGame m => m Bool
undamagedBane = do
  bane <- getDurinsBane
  maybe (pure False) (fmap (== 0) . getDamage . toTarget) bane
