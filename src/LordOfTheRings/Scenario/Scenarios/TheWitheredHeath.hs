module LordOfTheRings.Scenario.Scenarios.TheWitheredHeath (theWitheredHeath) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheWitheredHeath = TheWitheredHeath ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWitheredHeath :: Scenario
theWitheredHeath = Scenario $ scenario TheWitheredHeath "the-withered-heath" "The Withered Heath"

{- | The encounter deck is built from The Withered Heath, Lost Caves, Wild Creatures and Lost
in Wilderland; the Lost Caves cards are the Caves deck (MEC66 rules). Stage 1A's "Set
Cold-Drake and all four copies of Dragon Sign aside, out of play. Create the Caves deck"
is done as the deck is gathered.
Its meta is the damage on Cold-Drake while stage 3A has it set aside.
-}
instance RunMessage TheWitheredHeath where
  runMessage msg (TheWitheredHeath attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheWitheredHeath, EncounterSet.LostCaves, EncounterSet.WildCreatures, EncounterSet.LostInWilderland]
      let (caves, rest) = partition ((== Just EncounterSet.LostCaves) . cdEncounterSet . toCardDef) cards
          (aside, encounterDeck) = partition (cardMatches $ CardMatchAny [cardIs Enemies.coldDrake, cardIs Objectives.dragonSign]) rest
      cavesDeck <- shuffleM caves
      firstStage <- genEncounterCard Quests.searchingForDragonSign
      laterStages <- traverse genEncounterCard [Quests.theColdDrakeAttacks, Quests.huntingTheBeast, Quests.aCorneredDragon]
      putQuestIntoPlay firstStage
      pure
        $ TheWitheredHeath
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
        & cavesDeckL .~ cavesDeck
    ScenarioSpecific "witheredHeath.drakeSetAside" v -> pure $ TheWitheredHeath $ attrs & metaL .~ v
    ScenarioSpecific "witheredHeath.drakeHealed" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      let damage = fromMaybe 0 (parseMaybe parseJSON attrs.meta)
      pure $ TheWitheredHeath $ attrs & metaL .~ toJSON (max 0 (damage - n))
    _ -> TheWitheredHeath <$> runMessage msg attrs
