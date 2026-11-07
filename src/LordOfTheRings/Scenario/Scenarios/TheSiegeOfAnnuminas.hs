module LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas (theSiegeOfAnnuminas) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Ability
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Matcher (EnemyMatcher (..), getPlayer, getRemainingPlayers, select)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Source
import LordOfTheRings.Token (Token (Resource))
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (damageAnnuminas)

newtype TheSiegeOfAnnuminas = TheSiegeOfAnnuminas ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSiegeOfAnnuminas :: Scenario
theSiegeOfAnnuminas = Scenario $ scenario TheSiegeOfAnnuminas "the-siege-of-annuminas" "The Siege of Annúminas"

instance HasAbilities TheSiegeOfAnnuminas where
  getAbilities (TheSiegeOfAnnuminas a) =
    [ mkAbility a 1 $ forced RoundEndsWindow
    | isJust a.scenarioEpic, a.meta == toJSON (2 :: Int)
    ]

instance RunMessage TheSiegeOfAnnuminas where
  runMessage msg s@(TheSiegeOfAnnuminas a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.TheSiegeOfAnnuminas]
      let ordinal = maybe 0 (unGroupOrdinal . epicGroup) a.scenarioEpic
          stages = case ordinal of
            1 -> [Quests.raiseTheLevee, Quests.guardTheGateAnnuminas, Quests.rescueTheCaptivesAnnuminas]
            2 -> [Quests.delayTheEnemyAnnuminas, Quests.manTheWallsAnnuminas, Quests.stopTheAttack]
            _ -> [Quests.rebuildTheDefenses, Quests.defendTheCityAnnuminas, Quests.leadTheSortie]
          (cities, mainDeck) = partition ((== toCardCode Objectives.annuminas) . toCardCode) deck
      quests <- traverse genEncounterCard stages
      traverse_ putQuestIntoPlay $ take 1 quests
      -- Epic setup: only the AB team retains its physical City objective.
      when (ordinal == 0) do
        traverse_ addToStaging cities
        push $ ScenarioSpecific "annuminas.initializeCity" Null
      pure $ TheSiegeOfAnnuminas $ a & encounterDeckL .~ mainDeck & questDeckL .~ drop 1 quests & metaL .~ toJSON (1 :: Int)
    ScenarioSpecific "annuminas.initializeCity" _ -> do
      cities <- getsGame $ filter ((== toCardCode Objectives.annuminas) . toCardCode) . toList . (.entities.objectives)
      for_ cities $ \oid -> push $ DoStep 1 $ PlaceTokens GameSource (toTarget oid) Resource (if isJust a.scenarioEpic then 50 else 20)
      pure s
    AdvanceToQuest card -> do
      -- Standard setup reverse: these reinforcements precede stage 2A's
      -- When Revealed shuffle and enemy search. Epic leaves these cards aside.
      when (isNothing a.scenarioEpic && toCardCode card == toCardCode Quests.defendTheCityAnnuminas) do
        players <- length <$> getRemainingPlayers
        let dunedain = filter ((== toCardCode Objectives.dunedainOfAnnuminas) . toCardCode) a.scenarioSetAside
            hosts = take (max 1 $ players - 1) $ filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) a.scenarioSetAside
        shuffleSetAsideIntoEncounterDeck dunedain
        shuffleSetAsideIntoEncounterDeck hosts
      updated <- runMessage msg a
      pure $ TheSiegeOfAnnuminas $ updated & metaL .~ toJSON (fromMaybe 0 $ cdStage $ toCardDef card)
    EliminatePlayer pid | isNothing a.scenarioEpic -> do
      player <- getPlayer pid
      -- The reader still holds the state before this message's entity updates.
      -- Repeated elimination messages must not damage the city a second time.
      unless player.eliminated $ damageAnnuminas a 9
      TheSiegeOfAnnuminas <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      -- Epic setup reverse: effects resolve B,D,F; Hosts travel the opposite
      -- direction, so each physical Host moves only one stage this round.
      hosts <- select $ EnemyWithCardCode (toCardCode Enemies.hostOfAngmar) <> EnemyInStagingArea
      for_ a.scenarioEpic $ \replica -> for_ hosts $ \eid ->
        if unGroupOrdinal replica.epicGroup > 0
          then push $ MoveAnnuminasEnemy eid (GroupOrdinal $ unGroupOrdinal replica.epicGroup - 1) EpicStageEnemy
          else do
            discard a eid
            damageAnnuminas a 9
      pure s
    _ -> TheSiegeOfAnnuminas <$> runMessage msg a
