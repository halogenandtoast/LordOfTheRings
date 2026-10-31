module LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel (aJourneyToRhosgobel) where

import LordOfTheRings.Ability (forced, limited, restricted)
import LordOfTheRings.Ability.Types (AbilityLimit (GroupLimit), AbilityLimitType (PerPhase))
import LordOfTheRings.Criteria (Criterion (DuringPhase))
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLead)
import LordOfTheRings.Matcher.Types (cardIs)
import LordOfTheRings.Matcher.Window (WindowMatcher (EnemyWithTraitRevealedWindow))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Trait (Trait (Creature))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype AJourneyToRhosgobel = AJourneyToRhosgobel ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aJourneyToRhosgobel :: Scenario
aJourneyToRhosgobel =
  Scenario
    $ scenario (AJourneyToRhosgobel . (nightmareDeckL ?~ nightmare)) "a-journey-to-rhosgobel" "A Journey to Rhosgobel"
 where
  nightmare =
    nightmareDeck
      Nightmare.aJourneyToRhosgobelNightmare
      EncounterSet.AJourneyToRhosgobelNightmare
      [ (Locations.forestGrove, 4)
      , (Treacheries.exhaustion, 1)
      , (Treacheries.swarmingInsect, 2)
      , (Treacheries.festeringWounds, 1)
      , (Enemies.chieftanUfthak, 1)
      , (Enemies.dolGuldurBeastmaster, 2)
      , (Treacheries.drivenByShadow, 1)
      , (Treacheries.theNecromancersReach, 1)
      , (Locations.necromancersPass, 2)
      , (Locations.enchantedStream, 2)
      , (Locations.mountainsOfMirkwood, 3)
      ]

-- | Nightmare: "Forced: The first Creature enemy revealed during each quest phase gains
-- surge."
instance HasAbilities AJourneyToRhosgobel where
  getAbilities (AJourneyToRhosgobel a) =
    [ limited (GroupLimit PerPhase 1) $ restricted a 1 (DuringPhase QuestPhase) $ forced $ EnemyWithTraitRevealedWindow After Creature
    | a.nightmare
    ]

instance RunMessage AJourneyToRhosgobel where
  runMessage msg (AJourneyToRhosgobel attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.AJourneyToRhosgobel, EncounterSet.SpidersOfMirkwood, EncounterSet.DolGuldurOrcs]
      firstStage <- genEncounterCard Quests.theWoundedEagle
      laterStages <- traverse genEncounterCard [Quests.radagastsRequest, Quests.returnToRhosgobel]
      putQuestIntoPlay firstStage
      pure $ AJourneyToRhosgobel $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Setup: Search the encounter deck for 1 copy of Goblin Eagle-hunter and add
    -- it to the staging area. Then shuffle the encounter deck."
    DoStep 8 Setup | attrs.nightmare -> do
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Enemies.goblinEagleHunter)
      pure $ AJourneyToRhosgobel attrs
    UseCardAbility _ ScenarioSource 1 _ -> do
      revealEncounterCard
      pure $ AJourneyToRhosgobel attrs
    -- Nightmare: "Response: After an Athelas card is placed in the discard pile, you may
    -- shuffle it into the encounter deck."
    AddToEncounterDiscard cards | attrs.nightmare, any ((== toCardCode Objectives.athelas) . toCardCode) cards -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "aJourneyToRhosgobelNightmare.athelas"
        labeled "aJourneyToRhosgobelNightmare.shuffle" $ shuffleEncounterDiscardIntoDeck (cardIs Objectives.athelas)
        labeled "pass" $ pure ()
      AJourneyToRhosgobel <$> runMessage msg attrs
    _ -> AJourneyToRhosgobel <$> runMessage msg attrs
