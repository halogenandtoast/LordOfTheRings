module LordOfTheRings.Location.Cards.ShatteredSea (shatteredSea) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ShatteredSea = ShatteredSea LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shatteredSea :: LocationCard ShatteredSea
shatteredSea = location ShatteredSea Cards.shatteredSea

-- | While your heading is one setting off-course, Shattered Sea gets +2 threat and +2
-- quest points; two settings, +4 and +4; at the worst setting, +6 and +6.
instance HasModifiersFor ShatteredSea where
  getModifiersFor (ShatteredSea a) = do
    heading <- getHeading
    for_ heading \h -> do
      let n = 2 * fromEnum h
      when (n > 0) $ modified a a [ThreatModifier n, QuestPointModifier n]

instance RunMessage ShatteredSea where
  runMessage msg (ShatteredSea a) = ShatteredSea <$> runMessage msg a
