module LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret (celebrimborsSecret) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy (newEnemy)
import LordOfTheRings.Keyword (Keyword (Scour))
import LordOfTheRings.Location (newLocation)
import LordOfTheRings.Matcher (getLead, select)
import LordOfTheRings.Matcher.Types (LocationMatcher (AnyLocation))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (assignDamageAmongLocations)
import LordOfTheRings.Source (Source)

newtype CelebrimborsSecret = CelebrimborsSecret ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

celebrimborsSecret :: Scenario
celebrimborsSecret = Scenario $ scenario CelebrimborsSecret "celebrimbors-secret" "Celebrimbor's Secret"

{- | The encounter deck: Celebrimbor's Secret, Broken Lands and Misty Mountain Orcs.

The topmost Scour effect in the discard pile resolves from a card built for it, as a shadow
card's effect does: the card reads that it is not in play.
-}
instance RunMessage CelebrimborsSecret where
  runMessage msg (CelebrimborsSecret attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.CelebrimborsSecret, EncounterSet.BrokenLands, EncounterSet.MistyMountainOrcs]
      firstStage <- genEncounterCard Quests.theRuinsOfOstInEdhil
      laterStages <- traverse genEncounterCard [Quests.theEnemysServant]
      putQuestIntoPlay firstStage
      pure $ CelebrimborsSecret $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    ScenarioSpecific "scourFromDiscard" _ -> do
      for_ (find ((Scour `member`) . cdKeywords . toCardDef) attrs.encounterDiscard) \card ->
        case cdCardType (toCardDef card) of
          EnemyType -> do
            e <- newEnemy card
            void $ runMessage (scourOf e) e
          LocationType -> do
            l <- newLocation card
            void $ runMessage (scourOf l) l
          _ -> pure ()
      pure $ CelebrimborsSecret attrs
    ScenarioSpecific "assignLocationDamage" (parseMaybe parseJSON -> Just (source :: Source, n :: Int)) -> do
      locations <- select AnyLocation
      lead <- getLead
      chooseOneM lead do
        questionLabeledWith "celebrimborsSecret.assignDamage" [intVar "n" n]
        targets locations \l -> placeDamage source l 1
      assignDamageAmongLocations source (n - 1)
      pure $ CelebrimborsSecret attrs
    _ -> CelebrimborsSecret <$> runMessage msg attrs
   where
    scourOf :: Targetable a => a -> Message
    scourOf = ScenarioSpecific "scour" . toJSON . toTarget
