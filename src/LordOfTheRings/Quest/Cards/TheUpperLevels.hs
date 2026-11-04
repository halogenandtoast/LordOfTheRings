module LordOfTheRings.Quest.Cards.TheUpperLevels (theUpperLevels) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype TheUpperLevels = TheUpperLevels QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theUpperLevels :: QuestCard TheUpperLevels
theUpperLevels = quest TheUpperLevels Cards.theUpperLevels

-- | Forced: After characters are committed to the quest, raise each player's threat by 1.
instance HasAbilities TheUpperLevels where
  getAbilities (TheUpperLevels a) =
    exploringGundabad a <> bypass a <> [mkAbility a 1 $ forced StagingStepBeginsWindow | a.flipped]

-- | Forced: When this stage is defeated, shuffle The First Forge into the Caves deck.
instance RunMessage TheUpperLevels where
  runMessage msg q@(TheUpperLevels attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure q
    _ -> TheUpperLevels <$> runExploringGundabad defeated msg attrs
   where
    defeated = traverse_ shuffleIntoCavesDeck =<< getSetAside (cardIs Locations.theFirstForge)
