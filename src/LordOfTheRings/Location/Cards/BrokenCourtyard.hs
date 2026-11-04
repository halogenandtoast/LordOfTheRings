module LordOfTheRings.Location.Cards.BrokenCourtyard (brokenCourtyard) where

import LordOfTheRings.Card (cdQuestPoints, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.Types (isSideQuest)

newtype BrokenCourtyard = BrokenCourtyard LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

brokenCourtyard :: LocationCard BrokenCourtyard
brokenCourtyard = location BrokenCourtyard Cards.brokenCourtyard

-- | Surge. While Broken Courtyard is in the staging area, the main quest cannot have
-- progress tokens beyond its printed quest points value.
instance HasModifiersFor BrokenCourtyard where
  getModifiersFor (BrokenCourtyard a) = when (a.placement == StagingArea) do
    mains <- getsGame (filter (not . isSideQuest) . toList . (.entities.quests))
    for_ mains \q -> for_ (cdQuestPoints $ toCardDef q) \qp ->
      modified a (toTarget q) [ProgressLimit (max 0 (qp - q.progress))]

-- | Shadow: Either remove 2 progress from main quest, or return attacking enemy to staging
-- area after this attack. The removal is offered only when it can be made in full (FAQ
-- 1.44).
instance RunMessage BrokenCourtyard where
  runMessage msg l@(BrokenCourtyard a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseOrRunOneM pid do
        questionLabeled "brokenCourtyard.choose"
        causedBy a
        removeMainQuestProgressLabeled a 2
        -- the return waits on the game's queue for the attack to resolve, once chosen
        labeled "brokenCourtyard.return" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> BrokenCourtyard <$> runMessage msg a
