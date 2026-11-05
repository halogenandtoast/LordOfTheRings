module LordOfTheRings.Location.Cards.DarkenedMine (darkenedMine) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DarkenedMine = DarkenedMine LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkenedMine :: LocationCard DarkenedMine
darkenedMine = location DarkenedMine Cards.darkenedMine

-- | While Darkened Mine is in the staging area, progress cannot be placed on locations in the
-- staging area.
instance HasModifiersFor DarkenedMine where
  getModifiersFor (DarkenedMine a) = when (a.placement == StagingArea) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | The shadow is resolved against the defending player's hand.
instance RunMessage DarkenedMine where
  runMessage msg l@(DarkenedMine a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardRandomFromHand pid 1
      pure l
    _ -> DarkenedMine <$> runMessage msg a
