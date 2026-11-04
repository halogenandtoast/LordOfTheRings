{- | Foundations of Stone. At stage 4 each player has a staging area and a stage of his
own: see "LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers".
-}
module LordOfTheRings.Scenario.Scenarios.FoundationsOfStone (foundationsOfStone) where

import LordOfTheRings.Classes.HasGame (ReadGame (..), getGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (Area (..), Separation (..), areaLead, focusArea, gameSeparation, isFocusedArea)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.Types ()
import LordOfTheRings.Modifier (ModifierType (StagingThreatBonus))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype FoundationsOfStone = FoundationsOfStone ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Nightmare's Shivering River: "While Shivering River is in the staging area, it
-- contributes its threat to the total threat in each other player's staging area." One in
-- another area's staging area adds its threat there, as that area reads it, to this one's.
instance HasModifiersFor FoundationsOfStone where
  getModifiersFor (FoundationsOfStone a) = do
    g <- getGame
    let n =
          sum
            [ runReadGame (getLocationThreat l.id) held
            | Just s <- [gameSeparation g]
            , area <- separationAreas s
            , not (isFocusedArea g area)
            , let held = focusArea (areaLead g s area) g
            , l <- toList (entitiesLocations $ areaHeldEntities area)
            , l.placement == StagingArea
            , toCardCode l == toCardCode Locations.shiveringRiver
            ]
    when (n > 0) $ modified a GameTarget [StagingThreatBonus n]

foundationsOfStone :: Scenario
foundationsOfStone =
  Scenario
    $ scenario (FoundationsOfStone . (nightmareDeckL ?~ nightmare)) "foundations-of-stone" "Foundations of Stone"
 where
  -- the nightmare set waits with the Foundations of Stone set for Washed Away!
  nightmare =
    nightmareDeck
      Nightmare.foundationsOfStoneNightmare
      EncounterSet.FoundationsOfStoneNightmare
      [ (Locations.drownedTreasury, 3)
      , (Locations.mithrilLode, 1)
      , (Enemies.moriaBats, 4)
      , (Enemies.goblinScout, 3)
      , (Enemies.goblinSwordsman, 2)
      , (Treacheries.caveIn, 3)
      , (Enemies.goblinFollower, 1)
      , (Locations.branchingPaths, 1)
      , (Treacheries.manyRoads, 1)
      , (Treacheries.burningLow, 3)
      ]

-- | The Foundations of Stone set waits out of play until Washed Away! shuffles it into a
-- new encounter deck; Washed Away! draws the stage 4s, so only stage 5 waits behind it.
instance RunMessage FoundationsOfStone where
  runMessage msg (FoundationsOfStone attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TwistsAndTurns, EncounterSet.GoblinsOfTheDeep, EncounterSet.HazardsOfThePit, EncounterSet.FoundationsOfStone]
      let waiting c = cdEncounterSet (toCardDef c) `elem` map Just [EncounterSet.FoundationsOfStone, EncounterSet.FoundationsOfStoneNightmare]
          (foundations, encounterDeck) = partition waiting cards
      firstStage <- genEncounterCard Quests.theDrippingWalls
      laterStages <- traverse genEncounterCard [Quests.theWatersEdge, Quests.washedAway, Quests.outOfTheDepths]
      putQuestIntoPlay firstStage
      pure
        $ FoundationsOfStone
        $ attrs
        & encounterDeckL
        .~ encounterDeck
        & setAsideL
        .~ foundations
        & questDeckL
        .~ laterStages
    _ -> FoundationsOfStone <$> runMessage msg attrs
