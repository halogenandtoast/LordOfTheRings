module LordOfTheRings.Quest.Cards.SwiftDeparture (swiftDeparture) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (moveProgressToStormcaller)

newtype SwiftDeparture = SwiftDeparture QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

swiftDeparture :: QuestCard SwiftDeparture
swiftDeparture = quest SwiftDeparture Cards.swiftDeparture

-- | Sailing. Forced: At the end of the quest phase, if you are off-course, move 2 progress
-- from Swift Departure to the Stormcaller's quest stage.
instance HasAbilities SwiftDeparture where
  getAbilities (SwiftDeparture a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.flipped]

-- | Setup: Prepare the Corsair deck. Players prepare their fleet. Prepare the Stormcaller's
-- area with a second quest deck consisting of only stages 2C, 3C, and 4C, as well as the
-- Stormcaller. Search the encounter deck for 1 copy of Rolling Seas, and add it to the
-- staging area (2 copies instead if there are 3 or more players in the game). Shuffle the
-- encounter deck.
instance RunMessage SwiftDeparture where
  runMessage msg q@(SwiftDeparture attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      prepareCorsairDeck
      prepareFleet
      push $ ScenarioSpecific "stormcaller.prepare" Null
      players <- length <$> getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (take (if players >= 3 then 2 else 1) $ filter (cardMatches $ cardIs Locations.rollingSeas) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      pure $ SwiftDeparture $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      whenM isOffCourse $ moveProgressToStormcaller attrs (toTarget attrs) attrs.progress 2
      pure q
    _ -> SwiftDeparture <$> runMessage msg attrs
