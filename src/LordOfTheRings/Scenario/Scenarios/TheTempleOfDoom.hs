module LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom (theTempleOfDoom) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheTempleOfDoom = TheTempleOfDoom ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTempleOfDoom :: Scenario
theTempleOfDoom = Scenario $ scenario TheTempleOfDoom "the-temple-of-doom" "The Temple of Doom"

{- | The Temple of Doom, Ulchor's Guard, Twisted Tunnels and Easterling Raiders make up the
encounter deck; The Power of Mordor is its own deck, which stage 1A creates (MEC77 rules).
Thane Ulchor and Tribute to Mordor are set aside for stage 1A to add to the staging area.
The four stage 2 cards are shuffled into the quest deck, under stage 1, and each is
revealed side 2A as the topmost card when the players advance.
-}
instance RunMessage TheTempleOfDoom where
  runMessage msg (TheTempleOfDoom attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.TheTempleOfDoom
          , EncounterSet.UlchorsGuard
          , EncounterSet.TwistedTunnels
          , EncounterSet.EasterlingRaiders
          ]
      let (aside, deck) =
            partition (cardMatches $ CardMatchAny [cardIs Enemies.thaneUlchorTheTempleOfDoom, cardIs Quests.tributeToMordor]) cards
      firstStage <- genEncounterCard Quests.theForgottenTemple
      laterStages <-
        shuffleM
          =<< traverse
            genEncounterCard
            [Quests.theLegacyOfMordor, Quests.memoryOfTheDarkYears, Quests.shrineToSauron, Quests.theLordOfMiddleEarth]
      putQuestIntoPlay firstStage
      pure
        $ TheTempleOfDoom
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ aside
        & questDeckL .~ laterStages
    _ -> TheTempleOfDoom <$> runMessage msg attrs
