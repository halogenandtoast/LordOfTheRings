module LordOfTheRings.Quest.Cards.OutOfTheFryingPan (outOfTheFryingPan) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (getRiddleStage)

newtype OutOfTheFryingPan = OutOfTheFryingPan QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

outOfTheFryingPan :: QuestCard OutOfTheFryingPan
outOfTheFryingPan = quest OutOfTheFryingPan Cards.outOfTheFryingPan

-- | Forced: Reveal 1 additional encounter card per player during the staging step.
instance HasModifiersFor OutOfTheFryingPan where
  getModifiersFor (OutOfTheFryingPan a) = when a.flipped do
    players <- length <$> getRemainingPlayers
    modified a GameTarget [AdditionalStagingReveals players]

{- | Setup: Add Lake in the Cavern to the staging area. Create a riddle area with stage 2A
and follow the setup instructions on that card.

Players cannot advance to stage 3A unless both 1B and 2B are complete: its progress piles
up until Riddles in the Dark has as much as its quest points, which has this stage checked
again. Both leave play as the players advance (Into the Fire takes stage 2 away).
-}
instance RunMessage OutOfTheFryingPan where
  runMessage msg q@(OutOfTheFryingPan attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      for_ (filter (cardMatches $ cardIs Locations.lakeInTheCavern) scenario.encounterDeck) \lake -> do
        removeFromEncounterDeck lake
        addToStaging lake
      for_ (filter ((== toCardCode Cards.riddlesInTheDark) . toCardCode) scenario.questDeck) \riddles -> do
        putQuestIntoPlay riddles
        push $ PerformSetup (CardIdTarget riddles.id)
      turnOver attrs
      pure q
    _ -> OutOfTheFryingPan <$> runConditionalStage riddlesComplete msg attrs

-- | Stage 2B is complete: Riddles in the Dark has as much progress as its quest points.
riddlesComplete :: HasGame m => m Bool
riddlesComplete =
  getRiddleStage >>= \case
    Nothing -> pure False
    Just qid -> do
      riddles <- getsGame (fmap toAttrs . lookup qid . (.entities.quests))
      maybe (pure False) hasEnoughProgress riddles
