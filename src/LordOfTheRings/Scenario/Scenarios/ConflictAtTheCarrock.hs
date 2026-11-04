module LordOfTheRings.Scenario.Scenarios.ConflictAtTheCarrock (conflictAtTheCarrock) where

import LordOfTheRings.Ability (forced, mkAbility)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Matcher.Window (WindowMatcher (AnyEnemyAttacksWindow, PhaseEndsWindow))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After, When))
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (EnemyAttacking))

newtype ConflictAtTheCarrock = ConflictAtTheCarrock ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

conflictAtTheCarrock :: Scenario
conflictAtTheCarrock =
  Scenario
    $ scenario (ConflictAtTheCarrock . (nightmareDeckL ?~ nightmare)) "conflict-at-the-carrock" "Conflict at the Carrock"
 where
  -- the nightmare set brings its own Louis, Morris, Rupert and Stuart
  nightmare =
    nightmareDeck
      Nightmare.conflictAtTheCarrockNightmare
      EncounterSet.ConflictAtTheCarrockNightmare
      [ (Objectives.grimbeornTheOld, 1)
      , (Enemies.louis, 1)
      , (Enemies.morris, 1)
      , (Enemies.stuart, 1)
      , (Enemies.rupert, 1)
      , (Locations.beePastures, 3)
      , (Locations.oakWoodGrove, 3)
      , (Treacheries.roastedSlowly, 1)
      , (Enemies.mistyMountainGoblins, 3)
      , (Locations.banksOfTheAnduin, 2)
      , (Enemies.wolfRider, 1)
      , (Enemies.goblinSniper, 2)
      , (Enemies.wargs, 2)
      , (Treacheries.despair, 2)
      , (Locations.theBrownLands, 2)
      , (Locations.theEastBight, 2)
      ]

-- | The nightmare Trolls' text while they are in the victory display:
-- Savage Hill-troll: "each Troll enemy gets +1 hit point."
-- Morris: "each Troll enemy gets +1 attack and gains 'Immune to player events.'"
-- Rupert: "the defense of each Troll enemy cannot be reduced."
-- Stuart: "each Troll enemy gets +1 defense and gains 'Cannot have player attachments.'"
instance HasModifiersFor ConflictAtTheCarrock where
  getModifiersFor (ConflictAtTheCarrock a) = do
    let shown def = length $ filter ((== toCardCode def) . toCardCode) a.victoryDisplay
        mods =
          [HitPointModifier n | let n = shown Enemies.savageHillTroll, n > 0]
            <> (if shown Enemies.morrisNightmare > 0 then [AttackModifier 1, ImmuneToPlayerEvents] else [])
            <> [CannotHaveDefenseReduced | shown Enemies.rupertNightmare > 0]
            <> (if shown Enemies.stuartNightmare > 0 then [DefenseModifier 1, CannotHaveAttachments] else [])
    unless (null mods) do
      trolls <- select (EnemyWithTrait Troll)
      for_ trolls \t -> modified a t mods

-- | Nightmare: "Forced: At the end of the quest phase, if no progress was placed on the
-- current quest this phase, place 1 progress on the current quest (bypassing the active
-- location)."
-- Louis, in the victory display: "each Troll enemy gains, 'Forced: After this enemy
-- attacks, the defending player must raise his threat by 1.'"
instance HasAbilities ConflictAtTheCarrock where
  getAbilities (ConflictAtTheCarrock a) =
    [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.nightmare]
      <> [mkAbility a 2 $ forced $ AnyEnemyAttacksWindow After | any ((== toCardCode Enemies.louisNightmare) . toCardCode) a.victoryDisplay]

instance RunMessage ConflictAtTheCarrock where
  runMessage msg (ConflictAtTheCarrock attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.ConflictAtTheCarrock, EncounterSet.JourneyAlongTheAnduin, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.grimbeornsQuest
      -- Nightmare adds stage 3, Grimbeorn's Folly
      laterStages <- traverse genEncounterCard ([Quests.againstTheTrolls] <> [Quests.grimbeornsFolly | attrs.nightmare])
      putQuestIntoPlay firstStage
      pure $ ConflictAtTheCarrock $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    UseCardAbility _ ScenarioSource 1 _ -> do
      current <- getCurrentQuest
      placed <- maybe (pure 1) progressPlacedThisRound current
      when (placed == 0) $ placeProgressOnCurrentQuest attrs 1
      pure $ ConflictAtTheCarrock attrs
    UseCardAbility _ ScenarioSource 2 windows -> do
      for_ [(eid, pid) | Window _ (EnemyAttacking eid pid) <- windows] \(eid, pid) -> do
        troll <- selectAny (EnemyWithId eid <> EnemyWithTrait Troll)
        when troll $ encounterRaisesThreat pid 1
      pure $ ConflictAtTheCarrock attrs
    -- Louis, Morris, Stuart and Rupert: Response: After defeating this enemy, you may
    -- choose and discard 1 "Sacked!" card from play. Asked as the Troll goes, while it can
    -- still be told apart. Nightmare grants it for each unique Troll.
    Destroy _ (EnemyTarget eid) -> do
      present <- selectAny (EnemyWithId eid)
      troll <- if present then (`elem` uniqueTrolls) . toCardCode <$> getEnemy eid else pure False
      sacked <- getsGame \g -> [t | t <- toList g.entities.treacheries, toCardCode t == toCardCode Treacheries.sacked]
      lead <- getLead
      when (troll && notNull sacked) $ chooseOneM lead do
        questionLabeled "conflictAtTheCarrock.discardSacked"
        targets sacked discard
        labeled "pass" $ pure ()
      ConflictAtTheCarrock <$> runMessage msg attrs
    _ -> ConflictAtTheCarrock <$> runMessage msg attrs
   where
    uniqueTrolls =
      map
        toCardCode
        [ Enemies.louis
        , Enemies.morris
        , Enemies.stuart
        , Enemies.rupert
        , Enemies.louisNightmare
        , Enemies.morrisNightmare
        , Enemies.stuartNightmare
        , Enemies.rupertNightmare
        , Enemies.robAndBob
        ]
