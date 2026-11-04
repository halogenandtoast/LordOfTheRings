module LordOfTheRings.Quest.Cards.ThroughTheMarshAnArduousJourney (throughTheMarshAnArduousJourney) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (runWanderingStage, wanderingStage)

newtype ThroughTheMarshAnArduousJourney = ThroughTheMarshAnArduousJourney QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheMarshAnArduousJourney :: QuestCard ThroughTheMarshAnArduousJourney
throughTheMarshAnArduousJourney = quest ThroughTheMarshAnArduousJourney Cards.throughTheMarshAnArduousJourney

instance HasAbilities ThroughTheMarshAnArduousJourney where
  getAbilities (ThroughTheMarshAnArduousJourney a) = wanderingStage a

-- | The Nightmare Deck's B side of Through the Marsh.
-- Allies enter play exhausted.
-- Forced: When the players defeat this stage, advance to a random stage 3A (the scenario
-- picks it).
instance RunMessage ThroughTheMarshAnArduousJourney where
  runMessage msg (ThroughTheMarshAnArduousJourney attrs) = case msg of
    CheckWindows ws | attrs.flipped -> do
      for_ [t | Window After (AllyEnteredPlay t _ _) <- ws] exhaust
      ThroughTheMarshAnArduousJourney <$> runWanderingStage msg attrs
    _ -> ThroughTheMarshAnArduousJourney <$> runWanderingStage msg attrs
