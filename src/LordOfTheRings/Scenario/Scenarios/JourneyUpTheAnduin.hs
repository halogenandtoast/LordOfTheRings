module LordOfTheRings.Scenario.Scenarios.JourneyUpTheAnduin (journeyUpTheAnduin) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype JourneyUpTheAnduin = JourneyUpTheAnduin ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyUpTheAnduin :: Scenario
journeyUpTheAnduin = Scenario $ scenario JourneyUpTheAnduin "journey-up-the-anduin" "Journey Up the Anduin"

{- | The encounter deck is built from Journey Up the Anduin, Grey Mountain Goblins, Hills of
Wilderland, Lost in Wilderland, Journey Along the Anduin and Wilderlands. Wilderlands'
enemies, shuffled, are the Evil Creatures deck, and its locations and treacheries are
removed from the game (MEC65 rules); Woodmen Village and The Old Ford are set aside, as
stage 1A's setup reads.
-}
instance RunMessage JourneyUpTheAnduin where
  runMessage msg (JourneyUpTheAnduin attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.JourneyUpTheAnduin
          , EncounterSet.GreyMountainGoblins
          , EncounterSet.HillsOfWilderland
          , EncounterSet.LostInWilderland
          , EncounterSet.JourneyAlongTheAnduin
          , EncounterSet.Wilderlands
          ]
      let wilderlands c = cdEncounterSet (toCardDef c) == Just EncounterSet.Wilderlands
          (fromWilderlands, rest) = partition wilderlands cards
          evilCreatures = filter ((== EnemyType) . cdCardType . toCardDef) fromWilderlands
          asideCard c = any (`cardMatches` c) [cardIs Locations.woodmenVillage, cardIs Locations.theOldFordJourneyUpTheAnduin]
          (aside, encounterDeck) = partition asideCard rest
      evilCreaturesDeck <- shuffleM evilCreatures
      firstStage <- genEncounterCard Quests.travelingNorth
      laterStages <- traverse genEncounterCard [Quests.woodmenUnderAttack, Quests.thePassageOfTheFord]
      putQuestIntoPlay firstStage
      pure
        $ JourneyUpTheAnduin
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
        & evilCreaturesDeckL ?~ evilCreaturesDeck
        & evilCreaturesDiscardL .~ []
    _ -> JourneyUpTheAnduin <$> runMessage msg attrs
