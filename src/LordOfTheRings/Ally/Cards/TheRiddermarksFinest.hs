module LordOfTheRings.Ally.Cards.TheRiddermarksFinest (theRiddermarksFinest) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype TheRiddermarksFinest = TheRiddermarksFinest AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRiddermarksFinest :: AllyCard TheRiddermarksFinest
theRiddermarksFinest = ally TheRiddermarksFinest Cards.theRiddermarksFinest

-- | Action: Exhaust and discard The Riddermark's Finest to place 2 progress tokens on any
-- location.
instance HasAbilities TheRiddermarksFinest where
  getAbilities (TheRiddermarksFinest a) = [restricted a 1 (exists AnyLocation) $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage TheRiddermarksFinest where
  runMessage msg s@(TheRiddermarksFinest a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select AnyLocation
      chooseOrRunOneM pid do
        questionLabeled "theRiddermarksFinest.location"
        causedBy a
        targets locations \l -> placeProgress a l 2
      -- the discard is the cost, but what it leads to waits for the effect
      discard a
      pure s
    _ -> TheRiddermarksFinest <$> runMessage msg a
