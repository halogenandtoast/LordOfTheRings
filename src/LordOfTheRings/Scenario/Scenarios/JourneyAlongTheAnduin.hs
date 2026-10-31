module LordOfTheRings.Scenario.Scenarios.JourneyAlongTheAnduin (journeyAlongTheAnduin) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesEnemies, entitiesHeroes)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MirkwoodPaths (attachedInPlay, eachPlayerAttaches, putMendorIntoPlay)
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (CharacterDestroyedByAttack, EnemyDestroyed))

newtype JourneyAlongTheAnduin = JourneyAlongTheAnduin ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyAlongTheAnduin :: Scenario
journeyAlongTheAnduin = Scenario $ scenario JourneyAlongTheAnduin "journey-along-the-anduin" "Journey Along the Anduin"

instance RunMessage JourneyAlongTheAnduin where
  runMessage msg (JourneyAlongTheAnduin attrs) = case msg of
    -- the first stage is out from the start, its setup followed at step 7 as ever
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.JourneyAlongTheAnduin, EncounterSet.SauronsReach, EncounterSet.DolGuldurOrcs, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.toTheRiver
      laterStages <- traverse genEncounterCard [Quests.anduinPassage, Quests.ambushOnTheShore]
      putQuestIntoPlay firstStage
      pure $ JourneyAlongTheAnduin $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Campaign: Setup: Put Mendor into play. Set each copy of Valor and Scarred aside, out
    -- of play (they come in as they are attached).
    DoStep 4 Setup | isJust attrs.campaign -> do
      putMendorIntoPlay
      JourneyAlongTheAnduin <$> runMessage msg attrs
    CheckWindows windows | isJust attrs.campaign -> do
      -- Response: After a Hill Troll is defeated, each player attaches 1 set-aside copy of
      -- Valor to a hero they control, if able.
      let trolls = [() | Window After (EnemyDestroyed card) <- windows, toCardCode card == toCardCode Enemies.hillTroll]
      for_ trolls \_ -> eachPlayerAttaches Attachments.valor 4
      -- Forced: After a character is destroyed by a Hill Troll, each player attaches 1
      -- set-aside copy of Scarred to a hero they control. (Limit once per game.)
      enemies <- getsGame (entitiesEnemies . gameEntities)
      let byTroll =
            or
              [ toCardCode e == toCardCode Enemies.hillTroll
              | Window After (CharacterDestroyedByAttack eid _ _) <- windows
              , Just e <- [Map.lookup eid enemies]
              ]
          scarredYet = attrs.meta == toJSON True
      if byTroll && not scarredYet
        then do
          eachPlayerAttaches Objectives.scarred 4
          JourneyAlongTheAnduin <$> runMessage msg (attrs {scenarioMeta = toJSON True})
        else JourneyAlongTheAnduin <$> runMessage msg attrs
    -- Resolution: Add each copy of Valor and Scarred in play to the Campaign Pool, recording
    -- the heroes they are on, to attach them to as each later scenario is set up. The hero
    -- with the most damage will be taken prisoner at the beginning of Escape from Dol
    -- Guldur (in case of a tie, randomly select among them).
    ResolveCampaign | Just l <- attrs.campaign -> do
      heroes <- getsGame (toList . entitiesHeroes . gameEntities)
      let codeOf hid = toCardCode <$> find ((== hid) . (.id)) heroes
      attached <- (<>) <$> attachedInPlay Attachments.valor <*> attachedInPlay Objectives.scarred
      let earned = [PoolCard code Nothing (codeOf hid) | (code, hid) <- attached]
          most = maximumMay (map (.damage) heroes)
      prisoner <- traverse uniform (nonEmpty [toCardCode h | h <- heroes, Just h.damage == most])
      let l' = maybe id (\p -> record Prisoner [p]) prisoner $ foldl' (flip addToPool) l earned
      JourneyAlongTheAnduin <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> JourneyAlongTheAnduin <$> runMessage msg attrs
