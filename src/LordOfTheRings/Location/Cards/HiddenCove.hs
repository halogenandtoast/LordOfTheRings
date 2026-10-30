module LordOfTheRings.Location.Cards.HiddenCove (hiddenCove) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.Types (isSideQuest)
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (moveProgressToStormcaller)

newtype HiddenCove = HiddenCove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hiddenCove :: LocationCard HiddenCove
hiddenCove = location HiddenCove Cards.hiddenCove

-- | Response: After Hidden Cove becomes the active location, heal 4 damage from each Ship
-- objective.
instance HasAbilities HiddenCove where
  getAbilities (HiddenCove a) = [mkAbility a 1 $ response $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | Travel: Move 2 progress from the main quest to the Stormcaller's quest stage.
instance RunMessage HiddenCove where
  runMessage msg l@(HiddenCove a) = case msg of
    TravelTo lid | lid == a.id -> do
      quests <- getsGame (toList . (.entities.quests))
      for_ (take 1 [q | q <- quests, not (isSideQuest q)]) \q ->
        moveProgressToStormcaller a (toTarget $ toId q) (toAttrs q).progress 2
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ships <- select shipObjective
      for_ ships \s -> healDamage a s 4
      pure l
    _ -> HiddenCove <$> runMessage msg a
