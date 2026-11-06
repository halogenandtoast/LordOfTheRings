module LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers
  ( victoryLocations, locationVictoryPoints, discardRandomVictoryLocation ) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Message (Message (AddToEncounterDiscard))
import LordOfTheRings.Message.Lifted
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
