module LordOfTheRings.Quest.Cards.TheGatheringOfTheClouds (theGatheringOfTheClouds) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (battleStages)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEncounterDeck, scenarioQuestDeck))

newtype TheGatheringOfTheClouds = TheGatheringOfTheClouds QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGatheringOfTheClouds :: QuestCard TheGatheringOfTheClouds
theGatheringOfTheClouds = quest TheGatheringOfTheClouds Cards.theGatheringOfTheClouds

{- | Setup: Search the encounter deck for Bolg and add it to the staging area. Then, shuffle
the encounter deck and reveal 1 encounter card per player.

When Revealed: Put stage 2, stage 3, and stage 4 into play at the same time. Then, remove
this stage from play.
-}
instance RunMessage TheGatheringOfTheClouds where
  runMessage msg q@(TheGatheringOfTheClouds attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame (scenarioEncounterDeck . toAttrs . (.scenario))
      for_ (take 1 $ filter ((== toCardCode Enemies.bolg) . toCardCode) deck) \bolg -> do
        removeFromEncounterDeck bolg
        addToStaging bolg
      shuffleEncounterDeck
      eachPlayer $ const revealEncounterCard
      TheGatheringOfTheClouds <$> runMessage msg attrs
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheGatheringOfTheClouds <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      questDeck <- getsGame (scenarioQuestDeck . toAttrs . (.scenario))
      traverse_ putQuestIntoPlayFlipped [card | card <- questDeck, toCardCode card `elem` map toCardCode battleStages]
      removeStageFromPlay attrs.id
      pure q
    _ -> TheGatheringOfTheClouds <$> runMessage msg attrs
