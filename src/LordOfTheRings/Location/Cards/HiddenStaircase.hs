module LordOfTheRings.Location.Cards.HiddenStaircase (hiddenStaircase) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (firstPlayersTensDigit)

newtype HiddenStaircase = HiddenStaircase LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hiddenStaircase :: LocationCard HiddenStaircase
hiddenStaircase = location HiddenStaircase Cards.hiddenStaircase

-- | Discover X. X is the tens digit of the first player's threat: its threat, printed X,
-- and its Discover value, printed as 0.
instance HasModifiersFor HiddenStaircase where
  getModifiersFor (HiddenStaircase a) = do
    x <- firstPlayersTensDigit
    when (x > 0) $ modifySelf a [ThreatModifier x, DiscoverValueModifier x]

-- | Forced: After resolving the Discover keyword on Hidden Staircase, reduce each player's
-- threat by X if no Hazard cards were revealed by it.
instance HasAbilities HiddenStaircase where
  getAbilities (HiddenStaircase a) = [mkAbility a 1 $ forced $ DiscoverResolvedWindow After a.id]

instance RunMessage HiddenStaircase where
  runMessage msg l@(HiddenStaircase a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      unless (or [hazard | Window After (DiscoverResolved _ hazard) <- ws]) do
        x <- firstPlayersTensDigit
        eachPlayer \p -> lowerThreatBy a p x
      pure l
    _ -> HiddenStaircase <$> runMessage msg a
