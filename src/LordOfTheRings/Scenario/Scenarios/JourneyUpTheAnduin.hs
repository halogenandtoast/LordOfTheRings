module LordOfTheRings.Scenario.Scenarios.JourneyUpTheAnduin (journeyUpTheAnduin) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EredMithrinCampaign (eredMithrinAbilities, eredMithrinModifiers, runEredMithrinCampaign)

newtype JourneyUpTheAnduin = JourneyUpTheAnduin ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyUpTheAnduin :: Scenario
journeyUpTheAnduin = Scenario $ scenario JourneyUpTheAnduin "journey-up-the-anduin" "Journey Up the Anduin"

-- | The Ered Mithrin campaign card's abilities and modifiers, in campaign mode.
instance HasAbilities JourneyUpTheAnduin where
  getAbilities (JourneyUpTheAnduin a) = eredMithrinAbilities a

instance HasModifiersFor JourneyUpTheAnduin where
  getModifiersFor (JourneyUpTheAnduin a) = eredMithrinModifiers a

{- | The encounter deck is built from Journey Up the Anduin, Grey Mountain Goblins, Hills of
Wilderland, Lost in Wilderland, Journey Along the Anduin and Wilderlands. Wilderlands'
enemies, shuffled, are the Evil Creatures deck, and its locations and treacheries are
removed from the game (MEC65 rules); Woodmen Village and The Old Ford are set aside, as
stage 1A's setup reads.
In easy mode the cards with the difficulty indicator are removed as the encounter sets are
gathered, "when building the encounter deck" (Easy Mode rules), before the Evil Creatures
deck is taken from them: it is built from the Wilderlands cards the scenario uses, so it
loses a Hill Troll and the Marsh Adder as Wilderlands does in any scenario. No ruling
speaks to it.
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
    _ -> JourneyUpTheAnduin <$> runEredMithrinCampaign msg attrs
