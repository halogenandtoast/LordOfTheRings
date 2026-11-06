module LordOfTheRings.Scenario.Scenarios.TheOath (theOath) where

import Data.Aeson (Result (Success))
import LordOfTheRings.Ability
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (earnAtSetup)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheOath = TheOath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOath :: Scenario
theOath = Scenario $ scenario TheOath "the-oath" "The Oath"

instance HasAbilities TheOath where
  getAbilities (TheOath attrs) =
    [mkAbility attrs 1 $ forced RoundEndsWindow | isJust attrs.campaign]

instance RunMessage TheOath where
  runMessage msg (TheOath attrs) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets attrs [EncounterSet.TheOath, EncounterSet.TheGoblins]
      firstStage <- genEncounterCard Quests.afterTheRaid
      later <- traverse genEncounterCard [Quests.mirkwoodForestTheOath, Quests.theRearguardTheOath]
      putQuestIntoPlay firstStage
      pure
        $ TheOath
        $ attrs
        & encounterDeckL
        .~ deck
        & questDeckL
        .~ later
        & campaignL
        %~ fmap (\l -> l {campaignLogCardResources = 0})
    DoStep 2 Setup
      | isJust attrs.campaign ->
          TheOath <$> runMessage msg (earnAtSetup [Enemies.stalkingGoblin] attrs)
    DoStep 4 Setup | isJust attrs.campaign -> do
      eachPlayer $ \pid -> push $ ScenarioSpecific "oath.chooseAdaptation" (toJSON pid)
      TheOath <$> runMessage msg attrs
    ScenarioSpecific "oath.chooseAdaptation" value
      | Just l <- attrs.campaign
      , Success pid <- fromJSON value -> do
          let boons =
                filter
                  (\def -> toCardCode def `notElem` map (.code) l.pool)
                  [ Attachments.theFervorOfLore
                  , Attachments.theMightOfTactics
                  , Attachments.thePowerOfSpirit
                  , Attachments.theSecretOfLeadership
                  ]
          heroes <- select (HeroControlledBy $ PlayerWithId pid)
          cards <- traverse (`genPlayerCard` Just pid) boons
          chooseOneM pid do
            questionLabeled "campaign.chooseAdaptation"
            showing (map PlayerCard cards)
            for_ cards $ \card -> targeting (CardIdTarget card.id) do
              chooseOneM pid do
                questionLabeled "campaign.attachTo"
                targets heroes $ \hid -> do
                  chosenHero <- getHero hid
                  attachOutOfPlay pid card hid
                  updateCampaignLog (AddToPool $ PoolCard (toCardCode card) Nothing (Just $ toCardCode chosenHero))
          pure $ TheOath attrs
    UseCardAbility _ (isSource attrs -> True) 1 _
      | Just l <- attrs.campaign ->
          pure $ TheOath $ attrs {scenarioCampaign = Just l {campaignLogCardResources = l.cardResources + 1}}
    ResolveCampaign | Just l <- attrs.campaign -> do
      let l' =
            if l.cardResources >= 6
              then addToPool (PoolCard (toCardCode Treacheries.guiltyConscience) Nothing Nothing) l
              else l
      TheOath <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> TheOath <$> runMessage msg attrs
