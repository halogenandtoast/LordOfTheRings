module LordOfTheRings.Quest.Cards.ThroughTheMarshAWearyPassage (throughTheMarshAWearyPassage) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (runWanderingStage, wanderingStage)

newtype ThroughTheMarshAWearyPassage = ThroughTheMarshAWearyPassage QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheMarshAWearyPassage :: QuestCard ThroughTheMarshAWearyPassage
throughTheMarshAWearyPassage = quest ThroughTheMarshAWearyPassage Cards.throughTheMarshAWearyPassage

instance HasAbilities ThroughTheMarshAWearyPassage where
  getAbilities (ThroughTheMarshAWearyPassage a) = wanderingStage a

-- | In order to commit characters to the quest, a player must first discard a card at
-- random from his hand.
instance HasModifiersFor ThroughTheMarshAWearyPassage where
  getModifiersFor (ThroughTheMarshAWearyPassage a) = when a.flipped do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [DiscardRandomToCommit]

-- | When Revealed: Raise each player's threat by 1.
-- Forced: When the players defeat this stage, advance to a random stage 3A (the scenario
-- picks it).
instance RunMessage ThroughTheMarshAWearyPassage where
  runMessage msg q@(ThroughTheMarshAWearyPassage attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure q
    _ -> ThroughTheMarshAWearyPassage <$> runWanderingStage msg attrs
