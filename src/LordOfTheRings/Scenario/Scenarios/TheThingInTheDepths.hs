module LordOfTheRings.Scenario.Scenarios.TheThingInTheDepths (theThingInTheDepths) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode, EnemyWithId), getRemainingPlayers, selectAny)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (EntersStagingArea))

newtype TheThingInTheDepths = TheThingInTheDepths ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Nightmare: stage 3B's two Forced effects the setup card gives it are Defend the Ship!'s,
in nightmare mode.
-}
theThingInTheDepths :: Scenario
theThingInTheDepths =
  Scenario
    $ scenario (TheThingInTheDepths . (nightmareDeckL ?~ nightmare)) "the-thing-in-the-depths" "The Thing in the Depths"
 where
  nightmare =
    nightmareDeck
      Nightmare.theThingInTheDepthsNightmare
      EncounterSet.TheThingInTheDepthsNightmare
      [ (Enemies.corsairInfiltrator, 2)
      , (Enemies.crushingArm, 1)
      , (Enemies.graspingArm, 1)
      , (Enemies.giantArm, 1)
      , (Locations.crewQuarters, 1)
      ]

-- | The nightmare cards of The Thing in the Depths' own set, out of play with it.
withTheThing :: EncounterCard -> Bool
withTheThing c =
  cdEncounterSet (toCardDef c) `elem` map Just [EncounterSet.TheThingInTheDepths, EncounterSet.TheStormcallerElite]
    || toCardCode c `elem` map toCardCode [Enemies.slitheringArm, Enemies.crashingArm, Treacheries.deepMaelstrom]

{- | The encounter deck is built from The Stormcaller and Corsair Raiders alone; The Thing
in the Depths and The Stormcaller Elite (enemy side faceup) are set aside, out of play, as
Take the Ship! reads. There is no Corsair deck and no fleet.
-}
instance RunMessage TheThingInTheDepths where
  runMessage msg (TheThingInTheDepths attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheStormcaller, EncounterSet.CorsairRaiders, EncounterSet.TheThingInTheDepths, EncounterSet.TheStormcallerElite]
      let (outOfPlay, encounterDeck) = partition withTheThing cards
      firstStage <- genEncounterCard Quests.takeTheShip
      laterStages <- traverse genEncounterCard [Quests.changeOfPlans, Quests.defendTheShip]
      putQuestIntoPlay firstStage
      pure
        $ TheThingInTheDepths
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ outOfPlay
    -- Nightmare: "Forced: After The Thing in the Depths enemy enters play, it attacks each
    -- player in turn order from the staging area (deal and resolve a shadow card for each
    -- attack)."
    CheckWindows ws | attrs.nightmare -> do
      for_ [eid | Window After (EntersStagingArea (EnemyTarget eid)) <- ws] \eid ->
        whenM (selectAny $ EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.theThingInTheDepths)) do
          players <- getRemainingPlayers
          for_ players \pid -> immediateAttack pid eid
      TheThingInTheDepths <$> runMessage msg attrs
    _ -> TheThingInTheDepths <$> runMessage msg attrs
