module LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark (aKnifeInTheDark) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches, select)
import LordOfTheRings.Matcher.Types (EnemyMatcher (AnyEnemy), CardMatcher (CardMatchAny))
import LordOfTheRings.Modifier (ModifierType (ThreatModifier))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype AKnifeInTheDark = AKnifeInTheDark ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aKnifeInTheDark :: Scenario
aKnifeInTheDark = Scenario $ scenario AKnifeInTheDark "a-knife-in-the-dark" "A Knife in the Dark"

-- | While Weathertop is in the victory display, each enemy in play gets -1 threat.
instance HasModifiersFor AKnifeInTheDark where
  getModifiersFor (AKnifeInTheDark a) =
    when (toCardCode Locations.weathertop `elem` map toCardCode a.victoryDisplay) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [ThreatModifier (-1)]

{- | A Knife in the Dark, Hunted, The Ring and The Nazgûl make up the encounter deck (The
Black Riders rules insert). Stage 1A sets aside all copies of Ringwraith, The Witch-king,
Midgewater and Weathertop, out of play: the scenario does it as it gathers.

The campaign card has no setup; its resolution is chosen as the players win (see
'winAKnifeInTheDark').
-}
instance RunMessage AKnifeInTheDark where
  runMessage msg (AKnifeInTheDark attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.AKnifeInTheDark, EncounterSet.Hunted, EncounterSet.TheRing, EncounterSet.TheNazgul]
      let aside = CardMatchAny $ map cardIs [Enemies.ringwraith, Enemies.theWitchKing, Locations.midgewater, Locations.weathertop]
          (outOfPlay, deck) = partition (cardMatches aside) cards
      putQuestIntoPlay =<< genEncounterCard Quests.troubleInBree
      laterStages <- traverse genEncounterCard [Quests.intoTheWild, Quests.theRingwraithsAttack]
      pure
        $ AKnifeInTheDark
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ outOfPlay
        & questDeckL .~ laterStages
    _ -> AKnifeInTheDark <$> runMessage msg attrs
