module LordOfTheRings.Quest.Cards.TheDesertOfHarad (theDesertOfHarad) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (increaseTemperature)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype TheDesertOfHarad = TheDesertOfHarad QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDesertOfHarad :: QuestCard TheDesertOfHarad
theDesertOfHarad = quest TheDesertOfHarad Cards.theDesertOfHarad

-- | Forced: After an ally enters play, either exhaust it, or deal 1 damage to it.
-- Forced: At the end of the round, increase the temperature by 4. If the temperature is
-- 60 or higher, the players lose the game (the scenario checks it as it moves).
instance HasAbilities TheDesertOfHarad where
  getAbilities (TheDesertOfHarad a)
    | a.flipped =
        [ mkAbility a 1 $ forced $ AllyEnteredPlayWindow After Nothing Nothing
        , mkAbility a 2 $ forced RoundEndsWindow
        ]
    | otherwise = []

{- | Setup: Each player searches the encounter deck for a copy of Burnt Sands and adds it to
the staging area. Shuffle the encounter deck. Place the heat tracker next to the quest
deck and set the temperature to 10: the scenario set it as it built the deck. The copies
are alike, so they are taken without asking.
The ally's controller chooses between exhausting it and damaging it, as Barren Moorland's.
-}
instance RunMessage TheDesertOfHarad where
  runMessage msg q@(TheDesertOfHarad attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      players <- length <$> getRemainingPlayers
      deck <- getsGame ((.scenarioEncounterDeck) . toAttrs . gameScenario)
      for_ (take players $ filter (cardMatches $ cardIs Locations.burntSands) deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      shuffleEncounterDeck
      TheDesertOfHarad <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [(ally, pid) | Window _ (AllyEnteredPlay ally pid _) <- ws] \(ally, pid) ->
        chooseOneM pid do
          questionLabeled "theDesertOfHarad.choose"
          causedBy attrs
          labeled "theDesertOfHarad.exhaust" $ exhaust ally
          labeled "theDesertOfHarad.damage" $ dealDamage attrs ally 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      increaseTemperature 4
      pure q
    _ -> TheDesertOfHarad <$> runMessage msg attrs
