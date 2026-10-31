module LordOfTheRings.Scenario.Scenarios.TheDruadanForest (theDruadanForest) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (AnyHero), getHero, getMainStage, select)
import LordOfTheRings.Modifier (ModifierType (CannotCollectResources))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (AdvancedToStage))

newtype TheDruadanForest = TheDruadanForest ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDruadanForest :: Scenario
theDruadanForest =
  Scenario
    $ scenario (TheDruadanForest . (nightmareDeckL ?~ nightmare)) "the-druadan-forest" "The Drúadan Forest"
 where
  -- the nightmare Drû-buri-Drû takes the place of the standard one
  nightmare =
    nightmareDeck
      Nightmare.theDruadanForestNightmare
      EncounterSet.TheDruadanForestNightmare
      [ (Locations.overgrownTrail, 1)
      , (Locations.secludedGlade, 3)
      , (Treacheries.lostCompany, 2)
      , (Enemies.druBuriDru, 1)
      , (Enemies.druadanElite, 1)
      , (Enemies.druadanHunter, 1)
      , (Treacheries.menInTheDark, 1)
      ]

-- | Nightmare: "Heroes do not collect resources during the resource phase."
instance HasModifiersFor TheDruadanForest where
  getModifiersFor (TheDruadanForest a) = when a.nightmare do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotCollectResources]

instance RunMessage TheDruadanForest where
  runMessage msg (TheDruadanForest attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.TheDruadanForest, EncounterSet.BroodingForest]
      firstStage <- genEncounterCard Quests.thePursuit
      laterStages <- traverse genEncounterCard [Quests.anUntimelyEnd, Quests.thePassageOut]
      putQuestIntoPlay firstStage
      pure $ TheDruadanForest $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Setup: Add resources to each hero's resource pool until each hero has 5
    -- resources. Search the encounter deck for 1 copy of Garden of Poisons and add it to the
    -- staging area. Shuffle the encounter deck."
    DoStep 8 Setup | attrs.nightmare -> do
      topUpResources 5
      for_ (find ((== toCardCode Locations.gardenOfPoisons) . toCardCode) attrs.encounterDeck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      pure $ TheDruadanForest attrs
    -- Nightmare: "Forced: After stage 2B is revealed, each hero with fewer than 3 resources
    -- gains resources until it has 3."
    CheckWindows ws | attrs.nightmare, any ((== AdvancedToStage) . windowType) ws -> do
      stage <- getMainStage
      when (stage == 2) $ topUpResources 3
      TheDruadanForest <$> runMessage msg attrs
    _ -> TheDruadanForest <$> runMessage msg attrs
   where
    topUpResources n = do
      heroes <- traverse getHero =<< select AnyHero
      for_ heroes \h -> when (h.resources < n) $ placeResources ScenarioSource h.id (n - h.resources)
