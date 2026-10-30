module LordOfTheRings.Location.Cards.DecrepitRemains (decrepitRemains) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (removeProgressPerDamage)

newtype DecrepitRemains = DecrepitRemains LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

decrepitRemains :: LocationCard DecrepitRemains
decrepitRemains = location DecrepitRemains Cards.decrepitRemains

-- | While Decrepit Remains is in the staging area, each location and quest in the staging
-- area gets +2 quest points. The quests there are the side quests.
instance HasModifiersFor DecrepitRemains where
  getModifiersFor (DecrepitRemains a) = when (a.placement == StagingArea) do
    locations <- select LocationInStagingArea
    for_ locations \l -> modified a l [QuestPointModifier 2]
    sideQuests <- getSideQuests
    for_ sideQuests \q -> modified a q [QuestPointModifier 2]

-- | Shadow: For each point of damage dealt by this attack, remove 1 progress from each
-- quest in play.
instance RunMessage DecrepitRemains where
  runMessage msg l@(DecrepitRemains a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      removeProgressPerDamage a eid
      pure l
    _ -> DecrepitRemains <$> runMessage msg a
