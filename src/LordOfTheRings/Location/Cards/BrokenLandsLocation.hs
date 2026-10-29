module LordOfTheRings.Location.Cards.BrokenLandsLocation (brokenLandsLocation) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype BrokenLandsLocation = BrokenLandsLocation LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

brokenLandsLocation :: LocationCard BrokenLandsLocation
brokenLandsLocation = location BrokenLandsLocation Cards.brokenLandsLocation

-- | While Broken Lands is in the staging area, progress cannot be placed on locations in the
-- staging area.
instance HasModifiersFor BrokenLandsLocation where
  getModifiersFor (BrokenLandsLocation a) = when (a.placement == StagingArea) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | Shadow: Attacking enemy gets +1 attack for each location in the staging area.
instance RunMessage BrokenLandsLocation where
  runMessage msg l@(BrokenLandsLocation a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      n <- length <$> select LocationInStagingArea
      when (n > 0) $ enemyGetsAttack a eid n
      pure l
    _ -> BrokenLandsLocation <$> runMessage msg a
