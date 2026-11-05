module LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland (theFateOfWilderland) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EredMithrinCampaign (eredMithrinAbilities, eredMithrinModifiers, runEredMithrinCampaign)

newtype TheFateOfWilderland = TheFateOfWilderland ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

theFateOfWilderland :: Scenario
theFateOfWilderland = Scenario $ scenario TheFateOfWilderland "the-fate-of-wilderland" "The Fate of Wilderland"

-- | The Ered Mithrin campaign card's abilities and modifiers, in campaign mode.
instance HasAbilities TheFateOfWilderland where
  getAbilities (TheFateOfWilderland a) = eredMithrinAbilities a

instance HasModifiersFor TheFateOfWilderland where
  getModifiersFor (TheFateOfWilderland a) = eredMithrinModifiers a

{- | The encounter deck is built from The Fate of Wilderland, Grey Mountain Goblins,
Gathering Gloom and Hills of Wilderland (MEC71 rules). The Goblins' Assault, The Heroes'
Defense and Urdug are set aside as it is gathered; stage 1A's setup adds them to the
staging area. No stage has quest points: the players win as Urdug is destroyed, and lose
as stage 3B finds enough progress on The Goblins' Assault.
-}
instance RunMessage TheFateOfWilderland where
  runMessage msg (TheFateOfWilderland attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheFateOfWilderland, EncounterSet.GreyMountainGoblins, EncounterSet.GatheringGloom, EncounterSet.HillsOfWilderland]
      let (aside, encounterDeck) =
            partition (cardMatches $ CardMatchAny [cardIs Objectives.theGoblinsAssault, cardIs Objectives.theHeroesDefense, cardIs Enemies.urdug]) cards
      firstStage <- genEncounterCard Quests.urdugsAssault
      laterStages <- traverse genEncounterCard [Quests.outflanked, Quests.theFinalPush]
      putQuestIntoPlay firstStage
      pure
        $ TheFateOfWilderland
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    _ -> TheFateOfWilderland <$> runEredMithrinCampaign msg attrs
