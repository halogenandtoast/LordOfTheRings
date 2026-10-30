module LordOfTheRings.Quest.Cards.FlightFromMountGram (flightFromMountGram) where

import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet (EncounterSet (AngmarOrcs))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))

newtype FlightFromMountGram = FlightFromMountGram QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

flightFromMountGram :: QuestCard FlightFromMountGram
flightFromMountGram = quest FlightFromMountGram Cards.flightFromMountGram

southernGate :: LocationMatcher
southernGate = LocationWithCardCode (toCardCode Locations.southernGate)

-- | Players cannot travel to Southern Gate unless there is at least 14 progress on this
-- stage.
instance HasModifiersFor FlightFromMountGram where
  getModifiersFor (FlightFromMountGram a) =
    when (a.progress < 14) do
      gates <- select southernGate
      for_ gates \g -> modified a g [CannotBeTraveledTo]

-- | When Revealed: Add Southern Gate and Jailor Gornákh to the staging area. Shuffle the
-- Angmar Orcs encounter set into the encounter deck. Then, reveal 1 encounter card per
-- player from the top of the encounter deck.
-- Players cannot defeat this stage while Southern Gate is in play. If the players defeat
-- this stage, they have escaped Mount Gram and win the game.
instance RunMessage FlightFromMountGram where
  runMessage msg q@(FlightFromMountGram attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      FlightFromMountGram <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.southernGate)
      addSetAsideToStaging (cardIs Enemies.jailorGornakh)
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      shuffleSetAsideIntoEncounterDeck [c | c <- aside, cdEncounterSet (toCardDef c) == Just AngmarOrcs]
      revealEncounterCards . length =<< getRemainingPlayers
      pure q
    _ -> FlightFromMountGram <$> runConditionalStage (selectNone southernGate) msg attrs
