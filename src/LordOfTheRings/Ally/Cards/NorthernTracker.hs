module LordOfTheRings.Ally.Cards.NorthernTracker (northernTracker) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype NorthernTracker = NorthernTracker AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

northernTracker :: AllyCard NorthernTracker
northernTracker = ally NorthernTracker Cards.northernTracker

-- | Response: After Northern Tracker commits to a quest, place 1 progress token on each
-- location in the staging area.
instance HasAbilities NorthernTracker where
  getAbilities (NorthernTracker a) =
    [restricted a 1 (exists LocationInStagingArea) $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage NorthernTracker where
  runMessage msg s@(NorthernTracker a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      staged <- select LocationInStagingArea
      for_ staged \l -> placeProgress a l 1
      pure s
    _ -> NorthernTracker <$> runMessage msg a
