-- | What the cards of Helm's Deep share: The Defense of Helm's Deep, the advance to stage
-- 5A, and the enemies stages 4A and 5A discard into the staging area.
module LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (
  getDefenseOfHelmsDeep,
  defenseTokens,
  advanceToStageFive,
  firstPlayerReveals,
  discardUntilEnemiesPerPlayer,
  addDiscardedEnemiesToStaging,
) where

import LordOfTheRings.Card (CardType (EnemyType), EncounterCard, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveTokens))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioQuestDeck))
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Resource), countTokens)

-- | The Defense of Helm's Deep, once stage 2A has added it to the staging area.
getDefenseOfHelmsDeep :: HasGame m => m (Maybe ObjectiveId)
getDefenseOfHelmsDeep =
  getsGame (fmap toId . find ((== toCardCode Objectives.theDefenseOfHelmsDeep) . toCardCode) . toList . entitiesObjectives . gameEntities)

-- | The resource tokens on The Defense of Helm's Deep, none before it is in play.
defenseTokens :: HasGame m => m Int
defenseTokens =
  getsGame (maybe 0 (countTokens Resource . objectiveTokens . toAttrs) . find ((== toCardCode Objectives.theDefenseOfHelmsDeep) . toCardCode) . toList . entitiesObjectives . gameEntities)

{- | "Advance to stage 5A": the main stage in play leaves it, undefeated, and stage 5 is
taken from the quest deck, the stages between left behind it. Nothing happens once stage
5 is no longer in the quest deck.
-}
advanceToStageFive :: ReverseQueue m => m ()
advanceToStageFive = do
  deck <- getsGame (scenarioQuestDeck . toAttrs . (.scenario))
  for_ (find ((== toCardCode Quests.theKingRidesOut) . toCardCode) deck) \stage -> do
    mains <- getMainQuests
    for_ [qid | QuestTarget qid <- mains] (push . LeaveQuestStage)
    advanceToQuest stage

-- | "The first player reveals an encounter card": he is its revealer, "you" on it.
firstPlayerReveals :: ReverseQueue m => m ()
firstPlayerReveals = revealEncounterCardAs =<< getLead

{- | "Discard cards from the encounter deck until X enemies are discarded. X is the number
of players.": the deck is read as this resolves, so a card shuffling the discard pile in
first does so in an earlier step.
-}
discardUntilEnemiesPerPlayer :: (ReverseQueue m, Sourceable a, Targetable a) => a -> m ()
discardUntilEnemiesPerPlayer a = do
  players <- length <$> getRemainingPlayers
  discardEncounterCardsUntilCount a players (CardWithType EnemyType)

-- | "Add each enemy discarded this way to the staging area."
addDiscardedEnemiesToStaging :: ReverseQueue m => [EncounterCard] -> m ()
addDiscardedEnemiesToStaging = traverse_ addToStagingFromDiscard . filter (cardMatches $ CardWithType EnemyType)
