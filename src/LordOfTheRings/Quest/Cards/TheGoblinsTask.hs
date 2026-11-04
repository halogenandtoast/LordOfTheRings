module LordOfTheRings.Quest.Cards.TheGoblinsTask (theGoblinsTask) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RoamAcrossRhovanion.Helpers (firstPlayerControls)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))

newtype TheGoblinsTask = TheGoblinsTask QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theGoblinsTask :: QuestCard TheGoblinsTask
theGoblinsTask = quest TheGoblinsTask Cards.theGoblinsTask

-- | Progress cannot be placed on locations in the staging area.
instance HasModifiersFor TheGoblinsTask where
  getModifiersFor (TheGoblinsTask a) = when a.flipped do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | Forced: When the active location is explored, if the first player controls Urdug,
-- advance to a random stage 2A (the scenario picks it). This stage has no quest points: it
-- is left only this way (Vision of the Palantir's Roam Across Rhovanion review).
instance HasAbilities TheGoblinsTask where
  getAbilities (TheGoblinsTask a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow When | a.flipped]

{- | Setup: The first player takes control of Urdug, objective side face-up. Set each other
objective and objective-ally aside, out of play. Shuffle the encounter deck and discard
cards from the top until X locations are discarded, where X is the number of players. Add
each discarded location to the staging area.

The scenario set every objective and objective-ally aside as it gathered the encounter
deck.
-}
instance RunMessage TheGoblinsTask where
  runMessage msg q@(TheGoblinsTask attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      lead <- getLead
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ (putObjectiveAllyIntoPlayUnder lead) (find (cardMatches $ cardIs Objectives.urdug) aside)
      shuffleEncounterDeck
      doStep 1 msg
      TheGoblinsTask <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (filter (cardMatches $ CardWithType LocationType) cards)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      whenM (firstPlayerControls Objectives.urdug) $ completeQuest attrs.id
      pure q
    _ -> TheGoblinsTask <$> runMessage msg attrs
