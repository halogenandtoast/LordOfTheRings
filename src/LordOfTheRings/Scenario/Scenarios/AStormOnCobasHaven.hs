module LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven (aStormOnCobasHaven) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (AllyMatcher (AllyWithId), EnemyMatcher (EnemyEngagedWith), PlayerMatcher (AnyPlayer), cardIs, cardMatches, getCurrentQuest, getLead, selectAny, shipObjective)
import LordOfTheRings.Modifier (ModifierType (ProgressPerRoundLimit))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (raiderFlagship)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype AStormOnCobasHaven = AStormOnCobasHaven ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Nightmare: its random Dol Amroth objective is Outmaneuver the Enemy's setup, in
nightmare mode, and "Raider Flagship cannot guard objectives" is where an enemy picks one
to guard.
-}
aStormOnCobasHaven :: Scenario
aStormOnCobasHaven =
  Scenario
    $ scenario (AStormOnCobasHaven . (nightmareDeckL ?~ nightmare)) "a-storm-on-cobas-haven" "A Storm on Cobas Haven"
 where
  nightmare =
    nightmareDeck
      Nightmare.aStormOnCobasHavenNightmare
      EncounterSet.AStormOnCobasHavenNightmare
      [ (Locations.belfalasIslet, 2)
      , (Locations.cobasHaven, 2)
      , (Locations.fogBank, 2)
      , (Enemies.corsairSkirmisher, 2)
      , (Treacheries.rammingSpeed, 1)
      , (Enemies.corsairInfiltrator, 2)
      , (Enemies.cunningPirate, 1)
      , (Enemies.umbarRaider, 1)
      ]

-- | Nightmare: "While any player is engaged with Raider Flagship, no more than 5 progress can
-- be placed on the current quest each round."
instance HasModifiersFor AStormOnCobasHaven where
  getModifiersFor (AStormOnCobasHaven a) = when a.nightmare do
    whenM (selectAny $ raiderFlagship <> EnemyEngagedWith AnyPlayer) do
      getCurrentQuest >>= traverse_ \q -> modified a q [ProgressPerRoundLimit 5]

{- | The encounter deck is built from the A Storm on Cobas Haven, Voyage Across Belegaer,
Umbar Navy, Corsair Pirates and Corsair Raiders sets; The Dream-chaser's Fleet is prepared
by the first stage. Each copy of Dol Amroth Warship is set aside, out of play.
-}
instance RunMessage AStormOnCobasHaven where
  runMessage msg (AStormOnCobasHaven attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.AStormOnCobasHaven
          , EncounterSet.VoyageAcrossBelegaer
          , EncounterSet.UmbarNavy
          , EncounterSet.CorsairPirates
          , EncounterSet.CorsairRaiders
          ]
      let (warships, encounterDeck) = partition (cardMatches $ cardIs Objectives.dolAmrothWarship) cards
      firstStage <- genEncounterCard Quests.outmaneuverTheEnemy
      laterStages <- traverse genEncounterCard [Quests.battleInTheBay, Quests.breakThroughTheFleet]
      putQuestIntoPlay firstStage
      pure
        $ AStormOnCobasHaven
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ warships
    -- Broken Rudder: "During sailing tests, for each success symbol found, cancel it unless
    -- you deal 1 damage to attached objective." The first player keeps as many as he deals
    -- damage for, each Broken Rudder attached asking in turn for what the last left.
    SailingTestResult top found canceled | attrs.nightmare -> rudderAsks 0 top found canceled
    DoStep i (SailingTestResult top found canceled) | attrs.nightmare -> rudderAsks i top found canceled
    DoStep _ inner@(SailingTestResult {}) -> AStormOnCobasHaven <$> runMessage inner attrs
    -- Broken Rudder: "*Forced: After this Sailing test ends, attach Broken Rudder to a Ship
    -- objective committed to that test." The first player picks which, if several are.
    SailingTestEnded top committed | attrs.nightmare -> do
      ships <- filterM (\case AllyTarget aid -> selectAny (AllyWithId aid <> shipObjective); _ -> pure False) committed
      lead <- getLead
      for_ [c | c <- top, cardMatches (cardIs Treacheries.brokenRudder) c, c `elem` attrs.encounterDiscard] \c ->
        chooseOrRunOneM lead do
          questionLabeled "brokenRudder.attach"
          targets ships \s -> attachFromEncounterDiscard c s
      AStormOnCobasHaven <$> runMessage msg attrs
    -- Campaign: the Dream-chaser campaign card's setup, Forced effects and resolution
    _ | isJust attrs.campaign -> AStormOnCobasHaven <$> runDreamChaserCampaign msg attrs
    _ -> AStormOnCobasHaven <$> runMessage msg attrs
   where
    rudderAsks i top found canceled = do
      rudders <-
        getsGame \g ->
          [ aid
          | t <- toList g.entities.treacheries
          , toCardCode t == toCardCode Treacheries.brokenRudder
          , AttachedTo (AllyTarget aid) <- [t.placement]
          ]
      case drop i rudders of
        ship : _ | found > canceled -> do
          lead <- getLead
          chooseAmountM lead "brokenRudder.keep" (AllyTarget ship) 0 (found - canceled) \k -> do
            dealDamage ScenarioSource (AllyTarget ship) k
            push $ DoStep (i + 1) (SailingTestResult top found (found - k))
          pure $ AStormOnCobasHaven attrs
        _ -> AStormOnCobasHaven <$> runMessage (SailingTestResult top found canceled) attrs
