module LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret (celebrimborsSecret) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy (newEnemy)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Scour))
import LordOfTheRings.Location (newLocation)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLead, select)
import LordOfTheRings.Matcher.Types (LocationMatcher (AnyLocation, LocationInStagingArea))
import LordOfTheRings.Modifier (ModifierType (ProgressReducedTo))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (assignDamageAmongLocations)
import LordOfTheRings.Source (Source)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype CelebrimborsSecret = CelebrimborsSecret ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

celebrimborsSecret :: Scenario
celebrimborsSecret =
  Scenario
    $ scenario (CelebrimborsSecret . (nightmareDeckL ?~ nightmare)) "celebrimbors-secret" "Celebrimbor's Secret"
 where
  nightmare =
    nightmareDeck
      Nightmare.celebrimborsSecretNightmare
      EncounterSet.CelebrimborsSecretNightmare
      [ (Enemies.orcSkirmisher, 1)
      , (Enemies.orcHunter, 1)
      , (Enemies.prowlingWolf, 3)
      , (Locations.ruinedPlaza, 1)
      , (Locations.cityRemains, 1)
      , (Locations.ancientFoundation, 1)
      , (Locations.brokenLandsLocation, 3)
      , (Treacheries.spiesFromMordor, 1)
      , (Treacheries.orcHuntingParty, 2)
      ]

-- | Nightmare: "Forced: When any amount of progress would be placed on a location in the
-- staging area, reduce that amount to 1."
instance HasModifiersFor CelebrimborsSecret where
  getModifiersFor (CelebrimborsSecret a) = when a.nightmare do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressReducedTo 1]

{- | The encounter deck: Celebrimbor's Secret, Broken Lands and Misty Mountain Orcs.

The topmost Scour effect in the discard pile resolves from a card built for it, as a shadow
card's effect does: the card reads that it is not in play.
-}
instance RunMessage CelebrimborsSecret where
  runMessage msg (CelebrimborsSecret attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.CelebrimborsSecret, EncounterSet.BrokenLands, EncounterSet.MistyMountainOrcs]
      firstStage <- genEncounterCard Quests.theRuinsOfOstInEdhil
      laterStages <- traverse genEncounterCard [Quests.theEnemysServant]
      putQuestIntoPlay firstStage
      pure $ CelebrimborsSecret $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Scour: Deal 1 damage to a location in play."
    ScenarioSpecific "scour" (parseMaybe parseJSON -> Just ScenarioTarget) -> do
      locations <- select AnyLocation
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "celebrimborsSecret.nightmareScour"
        targets locations \l -> placeDamage attrs l 1
      pure $ CelebrimborsSecret attrs
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
