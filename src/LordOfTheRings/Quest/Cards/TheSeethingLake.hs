module LordOfTheRings.Quest.Cards.TheSeethingLake (theSeethingLake) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Trait (Trait (Tentacle))

newtype TheSeethingLake = TheSeethingLake QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSeethingLake :: QuestCard TheSeethingLake
theSeethingLake = quest TheSeethingLake Cards.theSeethingLake

-- | When Revealed: Add The Watcher to the staging area. Doors of Durin becomes the active
-- location, moving any previous active location to the staging area. Shuffle all Tentacle
-- cards in the encounter discard pile back into the encounter deck.
-- If the players have at least 3 victory points and defeat this stage, they have won the
-- game.
instance RunMessage TheSeethingLake where
  runMessage msg q@(TheSeethingLake attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheSeethingLake <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.theWatcher)
      traverse_ (`placeLocation` StagingArea) =<< select IsActiveLocation
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ putIntoPlayAsActiveLocation $ filter (cardMatches $ cardIs Locations.doorsOfDurin) aside
      shuffleEncounterDiscardIntoDeck (CardWithTrait Tentacle)
      pure q
    -- The Watcher destroyed goes to the victory display
    Destroy (EnemyTarget _) -> do
      checkStage attrs.id
      TheSeethingLake <$> runConditionalStage condition msg attrs
    _ -> TheSeethingLake <$> runConditionalStage condition msg attrs
   where
    condition = getsGame ((>= 3) . (.victoryPoints) . toAttrs . (.scenario))
