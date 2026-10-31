module LordOfTheRings.Scenario.Scenarios.TheMorgulVale (theMorgulVale) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (HeroWithTitle), getPlayer, getRemainingPlayers, select)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheMorgulVale = TheMorgulVale ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMorgulVale :: Scenario
theMorgulVale =
  Scenario
    $ scenario (TheMorgulVale . (nightmareDeckL ?~ nightmare)) "the-morgul-vale" "The Morgul Vale"
 where
  nightmare =
    nightmareDeck
      Nightmare.theMorgulValeNightmare
      EncounterSet.TheMorgulValeNightmare
      [ (Enemies.forestBat, 3)
      , (Treacheries.watcherInTheWoods, 1)
      , (Enemies.orcArbalesters, 3)
      , (Treacheries.thePowerOfMordor, 1)
      , (Enemies.morgulSorcerer, 2)
      , (Enemies.morgulTracker, 2)
      , (Locations.morgulRoad, 3)
      ]

instance RunMessage TheMorgulVale where
  runMessage msg (TheMorgulVale attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheMorgulVale, EncounterSet.CreaturesOfTheForest, EncounterSet.MordorElite]
      firstStage <- genEncounterCard Quests.theRearguardTheMorgulVale
      laterStages <- traverse genEncounterCard [Quests.theBetrayal, Quests.theMorgulRider]
      putQuestIntoPlay firstStage
      -- To the Tower: "Remove Faramir from the game." Each copy leaves its deck before the
      -- starting hands are drawn
      players <- getRemainingPlayers
      for_ players \pid -> do
        deck <- (.deck) <$> getPlayer pid
        for_ [c | c <- deck, isFaramir (cdName $ toCardDef c)] \c -> removeFromDeck pid c.id
      pure $ TheMorgulVale $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- and a Faramir hero leaves play as the scenario is set up, after the starting threat
    -- counted him: no ruling says otherwise
    DoStep 7 Setup -> do
      traverse_ removeFromGame =<< select (HeroWithTitle "Faramir")
      TheMorgulVale <$> runMessage msg attrs
    -- Nightmare: "Setup: Place 1 progress on To the Tower."
    DoStep 8 Setup | attrs.nightmare -> do
      placeOnTower ScenarioSource 1
      pure $ TheMorgulVale attrs
    _ -> TheMorgulVale <$> runMessage msg attrs
   where
    isFaramir (Name title _) = title == "Faramir"
