module LordOfTheRings.Scenario.Scenarios.TheOldForest (theOldForest) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Matcher (characterController, select, EnemyMatcher (EnemyWithCardCode), getDamage, getLead)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (forestStages)
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)

newtype TheOldForest = TheOldForest ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldForest :: Scenario
theOldForest = Scenario $ scenario TheOldForest "the-old-forest" "The Old Forest"

instance RunMessage TheOldForest where
  runMessage msg (TheOldForest a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.TheOldForest]
      putQuestIntoPlay =<< genEncounterCard Quests.farewellToTheShire
      later <- traverse genEncounterCard (forestStages <> [Quests.theWickedWillow])
      pure $ TheOldForest $ a & encounterDeckL .~ deck & questDeckL .~ later
    DoStep 4 Setup | isJust a.campaign -> do
      lead <- getLead
      hosts <- cardHosts lead (toCardCode Attachments.oldBogeyStories)
      chooseOneM lead do
        questionLabeled "oldForest.boonHero"
        targets hosts \host -> do
          controller <- characterController host
          for_ controller \pid -> do
            card <- genPlayerCard Attachments.oldBogeyStories (Just pid)
            attachOutOfPlay pid card host
      TheOldForest <$> runMessage msg a
    ResolveCampaign | isJust a.campaign -> do
      willows <- select $ EnemyWithCardCode (toCardCode Enemies.oldManWillow)
      damage <- sum <$> traverse (getDamage . toTarget) willows
      when (damage >= 12) do
        lead <- getLead
        choosePlayerM lead a \pid -> do
          seat <- seatOf pid
          updateCampaignLog $ AddToPool $ PoolCard (toCardCode Attachments.oldBogeyStories) seat Nothing
      TheOldForest <$> runMessage msg a
    _ -> TheOldForest <$> runMessage msg a
