module LordOfTheRings.DarkOfMirkwood.BoonsSpec (spec) where

import Data.Aeson qualified as Aeson
import LordOfTheRings.Classes.HasAbilities (getAbilities)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), CampaignLogUpdate (AddToCount), newCampaignLog, recordedCount)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (PlayerMatcher (PlayerWithCampaignCardUsesBelow), select)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioCampaign))
import TestImport

inCampaign :: GameT ()
inCampaign = putGame . (scenarioL %~ overAttrs (\a -> a {scenarioCampaign = Just (newCampaignLog "dark-of-mirkwood")})) =<< getGame

spec :: Spec
spec = describe "Dark of Mirkwood Adaptation boons" do
  it "heals a damaged hero and records a card-wide campaign use" . gameTest $ \pid -> do
    inCampaign
    hero <- getHeroWithCode "01012"
    boon <- putAttachmentOn pid Attachments.theFervorOfLore (toTarget hero)
    run $ PlaceTokens GameSource (toTarget hero) Damage 3
    -- Resolve the effect directly; opening planning would leave an unanswered action window.
    let aid = case boon of
          AttachmentTarget attachmentId -> attachmentId
          _ -> error "expected an attachment"
    run $ UseCardAbility pid (AttachmentSource aid) 1 []
    getCharacterDamage (toTarget hero) `shouldReturn` 0
    recordedCount CardUses (toCardCode Attachments.theFervorOfLore) <$> getCampaignLog `shouldReturn` 1
    select (PlayerWithCampaignCardUsesBelow (toCardCode Attachments.theFervorOfLore) 1) `shouldReturn` []

  it "does not offer healing with no damaged hero" . gameTest $ \pid -> do
    inCampaign
    hero <- getHeroWithCode "01012"
    boon <- putAttachmentOn pid Attachments.theFervorOfLore (toTarget hero)
    run $ BeginPhase PlanningPhase
    offersAbilityOf pid boon `shouldReturn` False

  it "preserves Spirit’s spent campaign use across copies" . gameTest $ \pid -> do
    inCampaign
    hero <- getHeroWithCode "01012"
    boon <- putAttachmentOn pid Attachments.thePowerOfSpirit (toTarget hero)
    run $ UpdateCampaignLog (AddToCount CardUses (toCardCode Attachments.thePowerOfSpirit) 1)
    run $ BeginPhase PlanningPhase
    offersAbilityOf pid boon `shouldReturn` False

  it "prevents reuse of Tactics even when a new copy is attached" . gameTest $ \pid -> do
    inCampaign
    hero <- getHeroWithCode "01012"
    run $ UpdateCampaignLog (AddToCount CardUses (toCardCode Attachments.theMightOfTactics) 1)
    boon <- putAttachmentOn pid Attachments.theMightOfTactics (toTarget hero)
    run . AddToStaging =<< genEncounterCard Enemies.goblinSniper
    run $ BeginPhase PlanningPhase
    select (PlayerWithCampaignCardUsesBelow (toCardCode Attachments.theMightOfTactics) 1) `shouldReturn` []
    offersAbilityOf pid boon `shouldReturn` False

  it "resolves Tactics after its damage cost defeats the attached hero, including across a save" . gameTest $ \pid -> do
    inCampaign
    hero <- getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.theMightOfTactics (toTarget hero)
    run $ PlaceTokens GameSource (toTarget hero) Damage 3
    run . AddToStaging =<< genEncounterCard Enemies.goblinSniperDarkOfMirkwood
    sniper <- getsGame (fromJustNote "no sniper" . fmap toId . headMay . toList . (.entities.enemies))
    run $ EngageEnemy pid sniper
    attachment <- getsGame (fromJustNote "no boon" . headMay . toList . (.entities.attachments))
    ability <- maybe (error "no Tactics action") pure $ headMay $ getAbilities attachment
    run $ UseAbility pid ability []
    -- A question paying the cost can be saved before the hero leaves play.
    stored <- getGame
    case Aeson.fromJSON (toJSON stored) :: Aeson.Result Game of
      Aeson.Success restored -> putGame restored
      Aeson.Error err -> error err
    chooseTarget pid hero
    getsGame (any ((== toCardCode Attachments.theMightOfTactics) . toCardCode) . toList . (.entities.attachments)) `shouldReturn` False
    getsGame (any ((== "01012") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` False
    getsGame (any ((== toCardCode Enemies.goblinSniperDarkOfMirkwood) . toCardCode) . toList . (.entities.enemies)) `shouldReturn` False
    recordedCount CardUses (toCardCode Attachments.theMightOfTactics) <$> getCampaignLog `shouldReturn` 1
    getsGame (null . gameResolvingAttachments) `shouldReturn` True
