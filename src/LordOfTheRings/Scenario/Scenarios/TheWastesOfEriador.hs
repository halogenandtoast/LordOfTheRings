module LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador (theWastesOfEriador) where

import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (angmarSetup, earnAtSetup, seatOf)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheWastesOfEriador = TheWastesOfEriador ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWastesOfEriador :: Scenario
theWastesOfEriador =
  Scenario
    $ scenario (TheWastesOfEriador . (nightmareDeckL ?~ nightmare)) "the-wastes-of-eriador" "The Wastes of Eriador"
 where
  nightmare =
    nightmareDeck
      Nightmare.theWastesOfEriadorNightmare
      EncounterSet.TheWastesOfEriadorNightmare
      [ (Enemies.northernWarg, 2)
      , (Enemies.whiteWarg, 1)
      , (Treacheries.suddenDarkness, 1)
      , (Locations.northDowns, 1)
      , (Locations.eriadorWastes, 2)
      , (Treacheries.coldFromAngmar, 1)
      , (Treacheries.bitingWind, 3)
      , (Treacheries.freezingBlast, 1)
      , (Locations.shroudedHills, 1)
      , (Locations.ruggedCountry, 3)
      , (Treacheries.pressingNeeds, 2)
      ]

-- | Built from The Wastes of Eriador, Eriador Wilds and Foul Weather encounter sets. The
-- Time objective, Daybreak or Nightfall faceup, says whether it is Day or Night.
instance RunMessage TheWastesOfEriador where
  runMessage msg (TheWastesOfEriador attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheWastesOfEriador, EncounterSet.EriadorWilds, EncounterSet.FoulWeather]
      firstStage <- genEncounterCard Quests.acrossTheWastes
      laterStages <- traverse genEncounterCard [Quests.howlingAtNight, Quests.battleWithThePack]
      putQuestIntoPlay firstStage
      pure $ TheWastesOfEriador $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Campaign: Setup: Each player may change hero cards without incurring the +1 threat
    -- penalty (see 'freeHeroChange'). Shuffle the Fend Off Despair side quest into the
    -- encounter deck: in the Campaign Pool by step 2, where its resolution adds it anyway.
    DoStep 2 Setup | isJust attrs.campaign -> TheWastesOfEriador <$> runMessage msg (earnAtSetup [Quests.fendOffDespair] attrs)
    -- Each player adds 1 copy of Determination to their starting hand. Add each of those
    -- events to the Campaign Pool.
    DoStep 9 Setup | isJust attrs.campaign -> do
      angmarSetup attrs
      eachPlayer \p -> do
        card <- genPlayerCard Events.determination (Just p)
        seat <- seatOf p
        putInHand card
        updateCampaignLog $ AddToPool $ PoolCard (toCardCode card) seat Nothing
      TheWastesOfEriador <$> runMessage msg attrs
    _ -> TheWastesOfEriador <$> runMessage msg attrs
