module LordOfTheRings.Quest.Cards.ThroughTheMarshAnArduousJourney (throughTheMarshAnArduousJourney) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (runWanderingStage, wanderingStage)

newtype ThroughTheMarshAnArduousJourney = ThroughTheMarshAnArduousJourney QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheMarshAnArduousJourney :: QuestCard ThroughTheMarshAnArduousJourney
throughTheMarshAnArduousJourney = quest ThroughTheMarshAnArduousJourney Cards.throughTheMarshAnArduousJourney

instance HasAbilities ThroughTheMarshAnArduousJourney where
  getAbilities (ThroughTheMarshAnArduousJourney a) = wanderingStage a

-- | The Nightmare Deck's B side of Through the Marsh.
-- Allies enter play exhausted.
-- Forced: When the players defeat this stage, advance to a random stage 3A (the scenario
-- picks it).
instance HasModifiersFor ThroughTheMarshAnArduousJourney where
  getModifiersFor (ThroughTheMarshAnArduousJourney a) = when a.flipped $ modified a GameTarget [AlliesEnterPlayExhausted]

instance RunMessage ThroughTheMarshAnArduousJourney where
  runMessage msg (ThroughTheMarshAnArduousJourney attrs) = ThroughTheMarshAnArduousJourney <$> runWanderingStage msg attrs
