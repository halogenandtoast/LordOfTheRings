module LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers
  ( victoryLocations, locationVictoryPoints, discardRandomVictoryLocation
  , forestStages, advanceForestStage
  ) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Message (Message (AddToEncounterDiscard))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Types (QuestAttrs)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioGainedVictoryPoints)

victoryLocations :: HasGame m => m [EncounterCard]
victoryLocations = getsGame \g -> [c | EncounterCard c <- (toAttrs g.scenario).victoryDisplay, cdCardType (toCardDef c) == LocationType]

locationVictoryPoints :: HasGame m => EncounterCard -> m Int
locationVictoryPoints card = getsGame \g ->
  fromMaybe 0 $ cdVictoryPoints (toCardDef card) <|> Map.lookup card.id (scenarioGainedVictoryPoints $ toAttrs g.scenario)

discardRandomVictoryLocation :: ReverseQueue m => m (Maybe EncounterCard)
discardRandomVictoryLocation = do
  selected <- headMay <$> (shuffleM =<< victoryLocations)
  for_ selected \card -> do
    removeFromVictoryDisplay (EncounterCard card)
    push $ AddToEncounterDiscard [card]
  pure selected

forestStages :: [CardDef]
forestStages = [Quests.darkBadPlace, Quests.chokedWithBrambles, Quests.shiftingTrees, Quests.closingInAroundThem]

advanceForestStage :: ReverseQueue m => QuestAttrs -> Bool -> m ()
advanceForestStage current final = do
  deck <- getsGame ((.questDeck) . toAttrs . (.scenario))
  let isForest card = any (\def -> toCardCode card == toCardCode def) forestStages
      candidates = filter (\card -> card.id /= current.cardId && if final then toCardCode card == toCardCode Quests.theWickedWillow else isForest card) deck
  chosen <- headMay <$> shuffleM candidates
  for_ chosen \card -> do
    removeStageFromPlay current.id
    if final then for_ (filter isForest deck) \unused -> do
      removeFromQuestDeck (toCardDef unused)
      setAside unused
    else when (isForest current) $ shuffleIntoQuestDeck current
    -- Old Forest rules insert, p. 3: the previous stage is excluded from this
    -- choice, then returned with the unused stages. Recreating it clears progress.
    advanceToQuest card
