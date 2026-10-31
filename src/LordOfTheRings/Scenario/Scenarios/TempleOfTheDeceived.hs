module LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived (templeOfTheDeceived) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Grid (Pos (..))
import LordOfTheRings.Matcher (LocationMatcher (IsActiveLocation, LocationWithId), selectAny)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (Explored))

newtype TempleOfTheDeceived = TempleOfTheDeceived ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The removals are the setup card's; Hall of Beorn keeps 2 Faithless Desecrators, the
-- card none.
templeOfTheDeceived :: Scenario
templeOfTheDeceived =
  Scenario
    $ scenario (TempleOfTheDeceived . (nightmareDeckL ?~ nightmare)) "temple-of-the-deceived" "Temple of the Deceived"
 where
  nightmare =
    nightmareDeck
      Nightmare.templeOfTheDeceivedNightmare
      EncounterSet.TempleOfTheDeceivedNightmare
      [ (Locations.junglePath, 1)
      , (Locations.cursedTemple, 2)
      , (Treacheries.deeperIntoTheRuins, 1)
      , (Treacheries.sunkenRuins, 3)
      , (Treacheries.aimlessWandering, 2)
      , (Treacheries.curseOfTheDownfallen, 2)
      , (Treacheries.dwindlingSupplies, 1)
      , (Enemies.faithlessDesecrator, 3)
      ]

{- | The encounter deck is built from Temple of the Deceived, Ruins of Numenor and Drowned
Dead; The Stormcaller Elite's Captain Sahír and Na'asiyah come into play under the players,
objective-ally side faceup, so no card of that set is gathered. Their locations are the
Island Map's 15 Uncharted locations, laid out in a 3x5 grid without looking at their
facedown sides: the twelve Lost Islands, shuffled, fill the four columns on the left, and
the three Temple of the Deceived cards, shuffled, the rightmost one (Dream-chaser rules
insert).

Nightmare: "Forced: When preparing the Island Map, instead of the normal map placement,
create the first three leftmost columns using the nine "Lost Island" cards, then create two
middle columns using the six new "Edge of the Temple" cards, and finally one rightmost
column using the three "Temple of the Deceived" cards."
-}
instance RunMessage TempleOfTheDeceived where
  runMessage msg (TempleOfTheDeceived attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TempleOfTheDeceived, EncounterSet.RuinsOfNumenor, EncounterSet.DrownedDead]
      let (locations, encounterDeck) = partition ((== LocationType) . cdCardType . toCardDef) cards
          (temple, rest) = partition (cardIsAny [Locations.theGrottosEntrance, Locations.templeHalls]) locations
          (edges, islands) = partition ((== Just EncounterSet.TempleOfTheDeceivedNightmare) . cdEncounterSet . toCardDef) rest
          islandColumns = if attrs.nightmare then 3 else 4
      lostIslands <- shuffleM islands
      edgesOfTheTemple <- shuffleM edges
      temples <- shuffleM temple
      push
        $ PrepareIslandMap
        $ zip [Pos c r | c <- [0 .. islandColumns - 1], r <- [0 .. 2]] lostIslands
        <> zip [Pos c r | c <- [islandColumns .. islandColumns + 1], r <- [0 .. 2]] edgesOfTheTemple
        <> zip [Pos (islandColumns + if null edges then 0 else 2) r | r <- [0 .. 2]] temples
      putQuestIntoPlay =<< genEncounterCard Quests.searchingTheIsland
      pure $ TempleOfTheDeceived $ attrs & encounterDeckL .~ encounterDeck
    -- Nightmare: "Forced: After the active location is explored, discard all player cards
    -- attached to that location." As it is explored, still in play.
    CheckWindows ws | attrs.nightmare -> do
      for_ [lid | Window When (Explored lid) <- ws] \lid ->
        whenM (selectAny $ LocationWithId lid <> IsActiveLocation) do
          attached <- getsGame (filter ((== AttachedTo (LocationTarget lid)) . (.placement)) . toList . (.entities.attachments))
          for_ attached (discard . toTarget)
      TempleOfTheDeceived <$> runMessage msg attrs
    _ -> TempleOfTheDeceived <$> runMessage msg attrs
   where
    cardIsAny defs c = toCardCode c `elem` map toCardCode defs
