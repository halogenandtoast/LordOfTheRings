module LordOfTheRings.DreamChaser.CampaignCardsSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario (raidOnTheGreyHavens, voyageAcrossBelegaer)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

dc :: CampaignLog
dc = (newCampaignLog "dream-chaser") {campaignLogHeroes = [["01001", "01002", "01003"]]}

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

victoryCodes :: GameT [CardCode]
victoryCodes = getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))

spec :: Spec
spec = describe "Dream-chaser campaign cards" do
  describe "Ominous Fate" do
    it "raises threat by the campaign card's part number, and may go to the victory display" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      threat <- (.threat) <$> getPlayer pid
      run . RevealCard =<< genEncounterCard Treacheries.ominousFate
      resolveRevealed
      chooseOptionMatching "raise threat" pid \case
        Label l _ -> "$label.ominousFate.threat" `isPrefixOf` l
        _ -> False
      (.threat) <$> getPlayer pid `shouldReturn` threat + 1
      chooseLabel "$label.ominousFate.addToVictory" pid
      victoryCodes `shouldReturn` [toCardCode Treacheries.ominousFate]

    it "adds 1 to X for each boon chosen during setup" . campaignTestIn raidOnTheGreyHavens dc {campaignLogNotes = mapFromList [(ChosenDuringSetup, [toCardCode Events.galdorsCounselTactics])]} starterDeck $ \pid -> do
      threat <- (.threat) <$> getPlayer pid
      run . RevealCard =<< genEncounterCard Treacheries.ominousFate
      resolveRevealed
      chooseOptionMatching "raise threat" pid \case
        Label l _ -> "$label.ominousFate.threat" `isPrefixOf` l
        _ -> False
      -- Raid on the Grey Havens is part 3
      (.threat) <$> getPlayer pid `shouldReturn` threat + 4

  describe "Crowley" do
    it "takes a resource as he engages, growing with it" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      crowley <- staged Enemies.crowley
      run $ EngageEnemy pid crowley
      e <- getEnemy crowley
      countTokens Resource (enemyTokens $ toAttrs e) `shouldBe` 1
      mods <- getModifiers crowley
      mods `shouldSatisfy` elem (AttackModifier 1)

  describe "Revenant" do
    it "takes no more than 2 damage at once, and grows with the campaign card's resources" . campaignTestIn voyageAcrossBelegaer dc {campaignLogCardResources = 1} starterDeck $ \_ -> do
      revenant <- staged Enemies.revenant
      run $ DealDamage GameSource (toTarget revenant) 3
      (.damage) <$> getEnemy revenant `shouldReturn` 2
      mods <- getModifiers revenant
      mods `shouldSatisfy` elem (AttackModifier 2)

  describe "Thunderstruck" do
    it "takes no more than 4 damage each round" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \_ -> do
      ship <- staged Enemies.thunderstruck
      run $ PlaceTokens GameSource (toTarget ship) Damage 3
      run $ PlaceTokens GameSource (toTarget ship) Damage 3
      (.damage) <$> getEnemy ship `shouldReturn` 4

  describe "Black Key (burden)" do
    it "gives the enemy it is attached to +1 threat, attack and defense" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \_ -> do
      crowley <- staged Enemies.crowley
      run . (`PutObjectiveIntoPlayAttached` toTarget crowley) =<< genEncounterCard Objectives.blackKeyBurden
      mods <- getModifiers crowley
      mods `shouldSatisfy` \ms -> all (`elem` ms) [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, ImmuneToPlayerCardEffects]

  describe "Galdor's Counsel" do
    it "adds half its level in resources, then goes to the victory display" . campaignTestIn voyageAcrossBelegaer dc {campaignLogMarked = mapFromList [(toCardCode Events.galdorsCounselLeadership, 3)]} starterDeck $ \pid -> do
      _ <- playWith pid "01001" 0 Events.galdorsCounselLeadership
      aragorn <- getHeroWithCode "01001"
      chooseTarget pid aragorn.id
      getHeroResources aragorn.id `shouldReturn` 2
      victoryCodes `shouldReturn` [toCardCode Events.galdorsCounselLeadership]

  describe "Parrying Cutlass" do
    it "gives +1 defense, +2 with 2 resources in the pool" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      gloin <- getHeroWithCode "01003"
      _ <- putAttachmentOn pid Attachments.parryingCutlass (toTarget gloin.id)
      (`shouldSatisfy` elem (DefenseModifier 1)) =<< getModifiers gloin.id
      run $ PlaceTokens GameSource (toTarget gloin.id) Resource 2
      (`shouldSatisfy` elem (DefenseModifier 2)) =<< getModifiers gloin.id

  describe "Upgraded sides" do
    it "has each upgradable card's Upgraded side among the cards" $ (id :: IO () -> IO ()) $
      for_ ["DCC172U", "DCC173U", "DCC174U", "DCC175U", "DCC180U", "DCC183U", "DCC189U", "DCC190U", "DCC192U", "DCC193U", "DCC136U", "DCC137U", "DCC140U", "DCC141U", "DCC078U"] \code ->
        toCardCode (toCardDef (code :: CardCode)) `shouldBe` code
