module LordOfTheRings.Quest.Cards.ThreeIsCompany (threeIsCompany) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (nazgulEngage)

newtype ThreeIsCompany = ThreeIsCompany QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

threeIsCompany :: QuestCard ThreeIsCompany
threeIsCompany = quest ThreeIsCompany Cards.threeIsCompany

{- | Setup: Set Bucklebury Ferry aside, out of play. Add 1 Black Rider to the staging area
and make Bag End the active location. Shuffle the encounter deck. (The scenario set
Bucklebury Ferry aside as it gathered the encounter deck.)

When a player fails a Hide test, each Nazgûl enemy in the staging area engages that
player. (Trigger Forced effects after resolving this effect.)

The players cannot advance unless Bag End is in the victory display.
-}
instance RunMessage ThreeIsCompany where
  runMessage msg q@(ThreeIsCompany attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (find (cardMatches $ cardIs Enemies.blackRider) deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      for_ (find (cardMatches $ cardIs Locations.bagEnd) deck) \c -> do
        removeFromEncounterDeck c
        putIntoPlayAsActiveLocation c
      shuffleEncounterDeck
      ThreeIsCompany <$> runMessage msg attrs
    FailHideTest pid | attrs.flipped -> do
      nazgulEngage pid
      pure q
    _ -> ThreeIsCompany <$> runConditionalStage bagEndInVictoryDisplay msg attrs
   where
    bagEndInVictoryDisplay = getsGame (any (cardMatches $ cardIs Locations.bagEnd) . (.victoryDisplay) . toAttrs . (.scenario))
