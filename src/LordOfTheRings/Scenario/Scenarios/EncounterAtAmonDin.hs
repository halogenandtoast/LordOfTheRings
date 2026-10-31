module LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin (encounterAtAmonDin) where

import LordOfTheRings.Ability (WindowMatcher (PhaseBeginsWindow), forced, mkAbility)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode), getLead, select)
import LordOfTheRings.Modifier (ModifierType (CannotTakeDamage))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Types (questTokens)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (deadVillagerCount, objectiveOf, rescuedVillagerCount, rescuedVillagerOr)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Token (Token (Progress, Villager), countTokens)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype EncounterAtAmonDin = EncounterAtAmonDin ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

encounterAtAmonDin :: Scenario
encounterAtAmonDin =
  Scenario
    $ scenario (EncounterAtAmonDin . (nightmareDeckL ?~ nightmare)) "encounter-at-amon-din" "Encounter at Amon Dîn"
 where
  nightmare =
    nightmareDeck
      Nightmare.encounterAtAmonDinNightmare
      EncounterSet.EncounterAtAmonDinNightmare
      [ (Enemies.maraudingOrc, 1)
      , (Enemies.orcRavager, 3)
      , (Enemies.cravenEagle, 2)
      , (Locations.burningFarmhouse, 2)
      , (Locations.gondorianHamlet, 2)
      , (Treacheries.trappedInside, 2)
      , (Treacheries.panicked, 2)
      , (Enemies.orcRabble, 1)
      , (Treacheries.scourgeOfMordor, 2)
      ]

-- | Nightmare: "Ghulat cannot take damage unless there are at least 15 progress tokens on
-- Protect the Villagers."
instance HasModifiersFor EncounterAtAmonDin where
  getModifiersFor (EncounterAtAmonDin a) = when a.nightmare do
    quests <- getsGame (toList . (.entities.quests))
    let protecting = sum [countTokens Progress (questTokens $ toAttrs q) | q <- quests, toCardCode q == toCardCode Quests.protectTheVillagers]
    when (protecting < 15) do
      ghulats <- select (EnemyWithCardCode $ toCardCode Enemies.ghulat)
      for_ ghulats \g -> modified a g [CannotTakeDamage]

-- | Nightmare: "Forced: At the beginning of the quest phase, either discard 1 villager token
-- from Rescued Villagers, or reveal an encounter card."
instance HasAbilities EncounterAtAmonDin where
  getAbilities (EncounterAtAmonDin a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.nightmare]

instance RunMessage EncounterAtAmonDin where
  runMessage msg (EncounterAtAmonDin attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- gatherEncounterSets attrs [EncounterSet.EncounterAtAmonDin, EncounterSet.RavagingOrcs]
      firstStage <- genEncounterCard Quests.savageryOfTheOrcs
      laterStages <- traverse genEncounterCard [Quests.protectTheVillagers]
      putQuestIntoPlay firstStage
      pure $ EncounterAtAmonDin $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Setup: Place 1 villager token on Rescued Villagers."
    DoStep 8 Setup | attrs.nightmare -> do
      rescued <- objectiveOf Objectives.rescuedVillagers
      for_ rescued \o -> placeTokens ScenarioSource (ObjectiveTarget o) Villager 1
      pure $ EncounterAtAmonDin attrs
    UseCardAbility _ ScenarioSource 1 _ -> do
      lead <- getLead
      rescuedVillagerOr attrs lead "amonDin.reveal" revealEncounterCard
      pure $ EncounterAtAmonDin attrs
    -- Protect the Villagers is defeated and the game ends: the players have won if there are
    -- more villager tokens on Rescued Villagers than damage tokens on Dead Villagers, and
    -- have lost otherwise
    AdvanceQuest | null attrs.questDeck -> do
      rescued <- rescuedVillagerCount
      dead <- deadVillagerCount
      gameOver $ if rescued > dead then Won else Lost
      pure $ EncounterAtAmonDin attrs
    _ -> EncounterAtAmonDin <$> runMessage msg attrs
