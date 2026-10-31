module LordOfTheRings.Scenario.Scenarios.VoyageAcrossBelegaer (voyageAcrossBelegaer) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLead, isOnCourse)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype VoyageAcrossBelegaer = VoyageAcrossBelegaer ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

voyageAcrossBelegaer :: Scenario
voyageAcrossBelegaer =
  Scenario
    $ scenario (VoyageAcrossBelegaer . (nightmareDeckL ?~ nightmare)) "voyage-across-belegaer" "Voyage Across Belegaer"
 where
  -- its setup card has no text beyond "You are playing Nightmare mode"
  nightmare =
    nightmareDeck
      Nightmare.voyageAcrossBelegaerNightmare
      EncounterSet.VoyageAcrossBelegaerNightmare
      [ (Enemies.southronSailor, 2)
      , (Locations.starlitSea, 1)
      , (Locations.fogBank, 1)
      , (Locations.calmWaters, 2)
      , (Treacheries.thrownOffCourse, 2)
      , (Enemies.scoutingShip, 1)
      ]

{- | The encounter deck is built from the Voyage Across Belegaer, Vast Oceans, Stormy
Weather, Umbar Navy and Corsair Pirates sets; The Dream-chaser's Fleet is prepared by the
first stage. The stage 2 cards are shuffled so their order is random, stage 3 left on the
bottom. In nightmare mode Corsair Confrontation and Lost at Sea join the stage 2 cards.
-}
instance RunMessage VoyageAcrossBelegaer where
  runMessage msg (VoyageAcrossBelegaer attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [ EncounterSet.VoyageAcrossBelegaer
          , EncounterSet.VastOceans
          , EncounterSet.StormyWeather
          , EncounterSet.UmbarNavy
          , EncounterSet.CorsairPirates
          ]
      firstStage <- genEncounterCard Quests.departureFromTheHavens
      secondStages <- shuffleM =<< traverse genEncounterCard ([Quests.intoTheStorm, Quests.cursedMists, Quests.calphonsDivination, Quests.theStarsGuidance] <> [q | attrs.nightmare, q <- [Quests.corsairConfrontation, Quests.lostAtSea]])
      lastStage <- genEncounterCard Quests.corsairPursuit
      putQuestIntoPlay firstStage
      pure $ VoyageAcrossBelegaer $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ secondStages <> [lastStage]
    -- When a stage is defeated, if you are on-course, look at the top 2 stages of the quest
    -- deck and advance to one of your choice, placing the other on the bottom of the quest
    -- deck. Otherwise, advance to the top stage of the quest deck.
    AdvanceQuest | top@(_ : _ : _) <- take 2 attrs.questDeck -> do
      onCourse <- isOnCourse
      if onCourse
        then do
          lead <- getLead
          chooseOneM lead do
            questionLabeled "voyageAcrossBelegaer.chooseStage"
            showing (map EncounterCard top)
            targets top \chosen -> do
              for_ (filter (/= chosen) top) (putOnBottomOfQuestDeck . EncounterCard)
              advanceToQuest chosen
          pure $ VoyageAcrossBelegaer attrs
        else VoyageAcrossBelegaer <$> runMessage msg attrs
    _ -> VoyageAcrossBelegaer <$> runMessage msg attrs
