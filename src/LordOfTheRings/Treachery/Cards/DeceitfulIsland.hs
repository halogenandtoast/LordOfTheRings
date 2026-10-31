module LordOfTheRings.Treachery.Cards.DeceitfulIsland (deceitfulIsland) where

import LordOfTheRings.Helpers.IslandMap (activePosition, adjacentLocations, locationAt, locationToThe)
import LordOfTheRings.Location.Grid (Direction (Above, Below, LeftOf))
import LordOfTheRings.Location.Types (isUncharted)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeceitfulIsland = DeceitfulIsland TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deceitfulIsland :: TreacheryCard DeceitfulIsland
deceitfulIsland = treachery DeceitfulIsland Cards.deceitfulIsland

-- | Doomed 3.
-- When Revealed: Switch the placement of the active location with the location above or
-- below it, or to its left (the active location does not change). Then, flip each adjacent
-- location to its "Lost Island" or "Edge of the Temple" side.
instance RunMessage DeceitfulIsland where
  runMessage msg t@(DeceitfulIsland attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- maybe (pure Nothing) locationAt =<< activePosition
      for_ active \l -> do
        others <- catMaybes <$> traverse (`locationToThe` l) [Above, Below, LeftOf]
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "deceitfulIsland.switch"
          causedBy attrs
          targets (map (.id) others) \other -> do
            push $ SwapIslandMapPositions l.id other
            doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      active <- maybe (pure Nothing) locationAt =<< activePosition
      for_ active \l -> do
        around <- adjacentLocations l
        for_ [x | x <- around, not (isUncharted x), isJust x.otherSide] flipOver
      pure t
    _ -> DeceitfulIsland <$> runMessage msg attrs
