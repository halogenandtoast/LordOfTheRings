module LordOfTheRings.Location.Cards.TwistingPassage (twistingPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (locateTest, triggerLostEffects)

newtype TwistingPassage = TwistingPassage LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

twistingPassage :: LocationCard TwistingPassage
twistingPassage = location TwistingPassage Cards.twistingPassage

-- | Forced: Before placing progress tokens on Twisting Passage, the first player must make
-- a locate test. If this test is failed, do not place any progress tokens on Twisting
-- Passage and trigger all "Lost:" effects in play.
instance RunMessage TwistingPassage where
  runMessage msg l@(TwistingPassage attrs) = case msg of
    -- the progress placed again on a pass comes from Twisting Passage itself
    PlaceTokens source (isTarget attrs -> True) Progress n | n > 0, source /= toSource attrs -> do
      locateTest attrs (triggerLostEffects attrs) (placeProgress attrs attrs n)
      pure l
    _ -> TwistingPassage <$> runMessage msg attrs
