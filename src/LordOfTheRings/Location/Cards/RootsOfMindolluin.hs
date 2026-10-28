module LordOfTheRings.Location.Cards.RootsOfMindolluin (rootsOfMindolluin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RootsOfMindolluin = RootsOfMindolluin LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

rootsOfMindolluin :: LocationCard RootsOfMindolluin
rootsOfMindolluin = location RootsOfMindolluin Cards.rootsOfMindolluin

-- | Underworld 1.
-- Each location in the staging area gets +2 quest points.
instance HasModifiersFor RootsOfMindolluin where
  getModifiersFor (RootsOfMindolluin a) = do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [QuestPointModifier 2]

instance RunMessage RootsOfMindolluin where
  runMessage msg (RootsOfMindolluin a) = RootsOfMindolluin <$> runMessage msg a
