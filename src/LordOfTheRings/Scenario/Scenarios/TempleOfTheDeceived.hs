module LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived (templeOfTheDeceived) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Grid (Pos (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TempleOfTheDeceived = TempleOfTheDeceived ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

templeOfTheDeceived :: Scenario
templeOfTheDeceived = Scenario $ scenario TempleOfTheDeceived "temple-of-the-deceived" "Temple of the Deceived"

{- | The encounter deck is built from Temple of the Deceived, Ruins of Numenor and Drowned
Dead; The Stormcaller Elite's Captain Sahír and Na'asiyah come into play under the players,
objective-ally side faceup, so no card of that set is gathered. Their locations are the
Island Map's 15 Uncharted locations, laid out in a 3x5 grid without looking at their
facedown sides: the twelve Lost Islands, shuffled, fill the four columns on the left, and
the three Temple of the Deceived cards, shuffled, the rightmost one (Dream-chaser rules
insert).
-}
instance RunMessage TempleOfTheDeceived where
  runMessage msg (TempleOfTheDeceived attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TempleOfTheDeceived, EncounterSet.RuinsOfNumenor, EncounterSet.DrownedDead]
      let (locations, encounterDeck) = partition ((== LocationType) . cdCardType . toCardDef) cards
          (temple, islands) = partition (cardIsAny [Locations.theGrottosEntrance, Locations.templeHalls]) locations
      lostIslands <- shuffleM islands
      temples <- shuffleM temple
      push
        $ PrepareIslandMap
        $ zip [Pos c r | c <- [0 .. 3], r <- [0 .. 2]] lostIslands
        <> zip [Pos 4 r | r <- [0 .. 2]] temples
      putQuestIntoPlay =<< genEncounterCard Quests.searchingTheIsland
      pure $ TempleOfTheDeceived $ attrs & encounterDeckL .~ encounterDeck
    _ -> TempleOfTheDeceived <$> runMessage msg attrs
   where
    cardIsAny defs c = toCardCode c `elem` map toCardCode defs
