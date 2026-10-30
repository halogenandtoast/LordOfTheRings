module LordOfTheRings.Quest.Cards.SiftThroughTheDebris (siftThroughTheDebris) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (flipToClue)

newtype SiftThroughTheDebris = SiftThroughTheDebris QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
siftThroughTheDebris :: QuestCard SiftThroughTheDebris
siftThroughTheDebris = quest SiftThroughTheDebris Cards.siftThroughTheDebris

-- | While Sift through the Debris is the current quest, each location in the staging area
-- gets +1 threat.
instance HasModifiersFor SiftThroughTheDebris where
  getModifiersFor (SiftThroughTheDebris a) = do
    current <- (== Just (toTarget a)) <$> getCurrentQuest
    when current do
      locations <- select LocationInStagingArea
      for_ locations \l -> modified a l [ThreatModifier 1]

-- | Forced: After a location is explored, place 2 progress on Sift through the Debris.
instance HasAbilities SiftThroughTheDebris where
  getAbilities (SiftThroughTheDebris a) = [mkAbility a 1 $ forced $ AnyExploredWindow After]

-- | When this quest is defeated, flip it over.
instance RunMessage SiftThroughTheDebris where
  runMessage msg q@(SiftThroughTheDebris attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress attrs attrs 2
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      flipToClue attrs Objectives.heirloomOfIarchon
      SiftThroughTheDebris <$> runMessage msg attrs
    _ -> SiftThroughTheDebris <$> runMessage msg attrs
