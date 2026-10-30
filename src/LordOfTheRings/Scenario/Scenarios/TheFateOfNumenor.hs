module LordOfTheRings.Scenario.Scenarios.TheFateOfNumenor (theFateOfNumenor) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheFateOfNumenor = TheFateOfNumenor ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFateOfNumenor :: Scenario
theFateOfNumenor = Scenario $ scenario TheFateOfNumenor "the-fate-of-numenor" "The Fate of Númenor"

{- | The encounter deck is built from The Fate of Númenor, Ruins of Numenor and Drowned
Dead. Their locations are the Uncharted locations: Shrine to Morgoth is set aside, and the
rest, shuffled, are the Uncharted deck (The Grey Havens rules insert).
-}
instance RunMessage TheFateOfNumenor where
  runMessage msg (TheFateOfNumenor attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheFateOfNumenor, EncounterSet.RuinsOfNumenor, EncounterSet.DrownedDead]
      let (locations, encounterDeck) = partition ((== LocationType) . cdCardType . toCardDef) cards
          (shrine, uncharted) = partition ((== toCardCode Locations.shrineToMorgoth) . toCardCode) locations
      unchartedDeck <- shuffleM uncharted
      firstStage <- genEncounterCard Quests.exploreTheIsland
      laterStages <- traverse genEncounterCard [Quests.aFatefulDiscovery]
      putQuestIntoPlay firstStage
      pure
        $ TheFateOfNumenor
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ shrine
        & unchartedDeckL .~ unchartedDeck
    _ -> TheFateOfNumenor <$> runMessage msg attrs
