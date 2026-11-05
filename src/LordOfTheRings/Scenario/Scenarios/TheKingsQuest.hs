module LordOfTheRings.Scenario.Scenarios.TheKingsQuest (theKingsQuest) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EredMithrinCampaign (eredMithrinAbilities, eredMithrinModifiers, runEredMithrinCampaign)

newtype TheKingsQuest = TheKingsQuest ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

theKingsQuest :: Scenario
theKingsQuest = Scenario $ scenario TheKingsQuest "the-kings-quest" "The King's Quest"

-- | The Ered Mithrin campaign card's abilities and modifiers, in campaign mode.
instance HasAbilities TheKingsQuest where
  getAbilities (TheKingsQuest a) = eredMithrinAbilities a

instance HasModifiersFor TheKingsQuest where
  getModifiersFor (TheKingsQuest a) = eredMithrinModifiers a

{- | The encounter deck is built from The King's Quest, Fell Beasts, Wild Creatures, Deep
Underground, Afraid of the Dark, Dragon Might and Lost Caves (MEC65 rules). The Lost
Caves cards are shuffled into the Caves deck; Fire-drake, Dragon Hoard and the Dragon
Might set are set aside, as stage 1A's setup reads, for stage 2A to bring in.
-}
instance RunMessage TheKingsQuest where
  runMessage msg (TheKingsQuest attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.TheKingsQuest
          , EncounterSet.FellBeasts
          , EncounterSet.WildCreatures
          , EncounterSet.DeepUnderground
          , EncounterSet.AfraidOfTheDark
          , EncounterSet.DragonMight
          , EncounterSet.LostCaves
          ]
      let inSet s c = cdEncounterSet (toCardDef c) == Just s
          (caves, rest) = partition (inSet EncounterSet.LostCaves) cards
          asideCard c = cardMatches (cardIs Enemies.fireDrake) c || cardMatches (cardIs Locations.dragonHoard) c || inSet EncounterSet.DragonMight c
          (aside, encounterDeck) = partition asideCard rest
      cavesDeck <- shuffleM caves
      firstStage <- genEncounterCard Quests.theIronHills
      laterStages <- traverse genEncounterCard [Quests.theLowerDepths, Quests.theFireWorm]
      putQuestIntoPlay firstStage
      pure
        $ TheKingsQuest
        $ attrs
        & encounterDeckL .~ encounterDeck
        & cavesDeckL .~ cavesDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    _ -> TheKingsQuest <$> runEredMithrinCampaign msg attrs
