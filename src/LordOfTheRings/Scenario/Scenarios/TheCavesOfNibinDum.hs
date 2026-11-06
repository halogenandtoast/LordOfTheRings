module LordOfTheRings.Scenario.Scenarios.TheCavesOfNibinDum (theCavesOfNibinDum) where

import LordOfTheRings.Campaign
import LordOfTheRings.Matcher (getPlayerOrder, getThreat)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (earnAtSetup, seatOf)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheCavesOfNibinDum = TheCavesOfNibinDum ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCavesOfNibinDum :: Scenario
theCavesOfNibinDum = Scenario $ scenario TheCavesOfNibinDum "the-caves-of-nibin-dum" "The Caves of Nibin-Dûm"

instance RunMessage TheCavesOfNibinDum where
  runMessage msg (TheCavesOfNibinDum attrs) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets attrs [EncounterSet.TheCavesOfNibinDum, EncounterSet.TheGoblins]
      firstStage <- genEncounterCard Quests.theDarkOfNibinDum
      later <-
        traverse genEncounterCard [Quests.surpriseAttackNibinDum, Quests.theChasm, Quests.oathkeepers]
      putQuestIntoPlay firstStage
      pure
        $ TheCavesOfNibinDum
        $ attrs
        & encounterDeckL
        .~ deck
        & questDeckL
        .~ later
        & campaignL
        %~ fmap (\l -> l {campaignLogCardResources = 0})
    DoStep 2 Setup
      | isJust attrs.campaign ->
          TheCavesOfNibinDum <$> runMessage msg (earnAtSetup [Treacheries.shinyDistraction] attrs)
    -- Step 4 follows starting hands and mulligans.
    DoStep 4 Setup | isJust attrs.campaign -> do
      eachPlayer $ \pid -> do
        seat <- seatOf pid
        card <- genPlayerCard Events.noTimeToWaste (Just pid)
        push $ PutInHand card
        updateCampaignLog (AddToPool $ PoolCard (toCardCode Events.noTimeToWaste) seat Nothing)
      TheCavesOfNibinDum <$> runMessage msg attrs
    ResolveCampaign | Just _ <- attrs.campaign -> do
      players <- getPlayerOrder
      threats <- traverse getThreat players
      when (all (< 40) threats)
        $ updateCampaignLog (Record RangerSenseNextCampaign [toCardCode Events.rangerSense])
      when (all (< 40) threats) $ for_ players $ \pid -> do
        seat <- seatOf pid
        chooseOneM pid do
          questionLabeled "campaign.rangerReward"
          labeled "campaign.rangerSense" do
            updateCampaignLog (AddToPool $ PoolCard (toCardCode Events.rangerSense) seat Nothing)
          labeled "done" $ pure ()
      TheCavesOfNibinDum <$> runMessage msg attrs
    _ -> TheCavesOfNibinDum <$> runMessage msg attrs
