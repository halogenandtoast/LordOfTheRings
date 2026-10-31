module LordOfTheRings.Scenario.Scenarios.JourneyAlongTheAnduin (journeyAlongTheAnduin) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithTrait), select)
import LordOfTheRings.Modifier (ModifierType (CannotHaveAttachments, EncounterDeckResetsWheneverEmpty))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype JourneyAlongTheAnduin = JourneyAlongTheAnduin ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyAlongTheAnduin :: Scenario
journeyAlongTheAnduin =
  Scenario
    $ scenario (JourneyAlongTheAnduin . (nightmareDeckL ?~ nightmare)) "journey-along-the-anduin" "Journey Along the Anduin"
 where
  nightmare =
    nightmareDeck
      Nightmare.journeyAlongTheAnduinNightmare
      EncounterSet.JourneyAlongTheAnduinNightmare
      [ (Enemies.dolGuldurOrcs, 3)
      , (Locations.necromancersPass, 2)
      , (Locations.enchantedStream, 2)
      , (Enemies.mistyMountainGoblins, 3)
      , (Locations.banksOfTheAnduin, 2)
      , (Treacheries.evilStorm, 3)
      , (Treacheries.treacherousFog, 2)
      , (Treacheries.despair, 2)
      ]

-- | Nightmare: "All Troll enemies gain the text: 'Cannot have attachments.' If the encounter
-- deck is empty (at any time), shuffle the encounter discard pile back into the encounter
-- deck."
instance HasModifiersFor JourneyAlongTheAnduin where
  getModifiersFor (JourneyAlongTheAnduin a) = when a.nightmare do
    modified a GameTarget [EncounterDeckResetsWheneverEmpty]
    trolls <- select (EnemyWithTrait Troll)
    for_ trolls \t -> modified a t [CannotHaveAttachments]

instance RunMessage JourneyAlongTheAnduin where
  runMessage msg (JourneyAlongTheAnduin attrs) = case msg of
    -- the first stage is out from the start, its setup followed at step 7 as ever
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.JourneyAlongTheAnduin, EncounterSet.SauronsReach, EncounterSet.DolGuldurOrcs, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.toTheRiver
      laterStages <- traverse genEncounterCard [Quests.anduinPassage, Quests.ambushOnTheShore]
      putQuestIntoPlay firstStage
      pure $ JourneyAlongTheAnduin $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> JourneyAlongTheAnduin <$> runMessage msg attrs
