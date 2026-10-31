module LordOfTheRings.Location.Cards.HighlandsOfDunland (highlandsOfDunland) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (removeTimeCountersFromLocations)

newtype HighlandsOfDunland = HighlandsOfDunland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

highlandsOfDunland :: LocationCard HighlandsOfDunland
highlandsOfDunland = location HighlandsOfDunland Cards.highlandsOfDunland

-- | Time 2. Forced: After the last time counter is removed from this location, return each
-- enemy in play to the staging area.
instance HasAbilities HighlandsOfDunland where
  getAbilities (HighlandsOfDunland a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Shadow: Either remove 1 time counter from a location in play, or return attacking enemy
-- to the staging area after this attack.
instance RunMessage HighlandsOfDunland where
  runMessage msg l@(HighlandsOfDunland a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnEngagedEnemiesToStaging
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      timed <- filterM (fmap (> 0) . getTimeCounters . toTarget) =<< select AnyLocation
      chooseOrRunOneM pid do
        questionLabeled "highlandsOfDunland.choose"
        causedBy a
        unless (null timed) $ labeled "highlandsOfDunland.removeTimeCounter" $ removeTimeCountersFromLocations a pid 1
        labeled "highlandsOfDunland.return" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> HighlandsOfDunland <$> runMessage msg a
