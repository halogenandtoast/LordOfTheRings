module LordOfTheRings.Location.Cards.StockBrook (stockBrook) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype StockBrook = StockBrook LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stockBrook :: LocationCard StockBrook
stockBrook = location StockBrook Cards.stockBrook

-- | When Revealed: Immediately travel to Stock-brook. If another location is currently
-- active, return it to the staging area.
instance RunMessage StockBrook where
  runMessage msg l@(StockBrook a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      -- one immune to card effects stays: there are two active locations (ruling 3.128)
      active <- filterM (fmap (notElem ImmuneToCardEffects) . getModifiers) =<< select IsActiveLocation
      for_ active \other -> placeLocation other StagingArea
      travelWithoutCost a.id
      pure l
    _ -> StockBrook <$> runMessage msg a
