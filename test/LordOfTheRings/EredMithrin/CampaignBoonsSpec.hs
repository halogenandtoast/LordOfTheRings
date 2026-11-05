module LordOfTheRings.EredMithrin.CampaignBoonsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), CampaignLogUpdate (AddToCount), newCampaignLog, recordedCount, seatUseCode)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithId))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Modifier (ModifierType (AttackBonusAgainstMatching, QuestedUnsuccessfullyThisRound))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase, QuestPhase, TravelPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioCampaign))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (CommittedToQuest, PhaseBegins), mkAfter)
import TestImport

beravorCode, theodredCode :: CardCode
beravorCode = "01012"
theodredCode = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  eid <- stagedEnemy def
  run $ EngageEnemy pid eid
  pure eid

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` existing) . map toId . toList . (.entities.enemies))

inPhase :: Phase -> GameT ()
inPhase phase = putGame . (phaseL .~ phase) =<< getGame

inCampaign :: GameT ()
inCampaign = putGame . (scenarioL %~ overAttrs (\a -> a {scenarioCampaign = Just (newCampaignLog "ered-mithrin")})) =<< getGame

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

spec :: Spec
spec = describe "Ered Mithrin campaign boons" do
  describe "Turayn" do
    it "cancels 1 of the next damage dealt to another character this round as he exhausts" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      turayn <- putAllyIntoPlay pid Allies.turayn
      run $ Exhaust turayn
      chooseAbilityOf pid turayn
      chooseTarget pid beravor
      run $ DealDamage GameSource beravor 2
      getCharacterDamage beravor `shouldReturn` 1
      run $ DealDamage GameSource beravor 2
      getCharacterDamage beravor `shouldReturn` 3

  describe "Melanwar" do
    it "places 3 progress on the active location as travel begins, the players having quested unsuccessfully" . gameTest $ \pid -> do
      melanwar <- putAllyIntoPlay pid Allies.melanwar
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ TravelTo lid
      run $ CreateEffect (Effect GameSource GameTarget [QuestedUnsuccessfullyThisRound] UntilEndOfRound)
      chooseLabel "$label.pass" pid
      run $ CheckWindows [mkAfter (PhaseBegins TravelPhase)]
      chooseAbilityOf pid melanwar
      -- its 2 quest points are met, and it is explored
      getsGame (length . (.entities.locations)) `shouldReturn` 0

  describe "Isolde" do
    it "gets +2 willpower with an Item attachment, and readies as her player engages an enemy" . gameTest $ \pid -> do
      isolde <- putAllyIntoPlay pid Allies.isolde
      _ <- putAttachmentOn pid Attachments.bladeOfGondolin isolde
      getWillpower isolde `shouldReturn` 3
      run $ Exhaust isolde
      _ <- engagedEnemy pid Enemies.kingSpider
      chooseAbilityOf pid isolde
      offersAbilityOf pid isolde `shouldReturn` False

  describe "Beldis" do
    it "spends a resource and marks a use in the campaign log to heal 3 damage" . gameTest $ \pid -> do
      inCampaign
      beravor <- hero beravorCode
      beldis <- putAllyIntoPlay pid Allies.beldis
      run $ PlaceTokens GameSource beravor Damage 3
      run $ PlaceTokens GameSource beravor Resource 1
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid beldis
      getCharacterDamage beravor `shouldReturn` 0
      recordedCount CardUses (seatUseCode (toCardCode Allies.beldis) 0) <$> getCampaignLog `shouldReturn` 1

    it "cannot be used by a player who has marked all 4 uses" . gameTest $ \pid -> do
      inCampaign
      beravor <- hero beravorCode
      beldis <- putAllyIntoPlay pid Allies.beldis
      run $ UpdateCampaignLog (AddToCount CardUses (seatUseCode (toCardCode Allies.beldis) 0) 4)
      run $ PlaceTokens GameSource beravor Damage 3
      run $ PlaceTokens GameSource beravor Resource 1
      run $ BeginPhase PlanningPhase
      offersAbilityOf pid beldis `shouldReturn` False

  describe "Master of the Hunt" do
    it "gives +1 hit point, and keeps 1 of a player's top 3 cards on top, the rest to the bottom" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      hunt <- putAttachmentOn pid Attachments.masterOfTheHunt beravor
      cards@[a, b, c, d] <- traverse (\def -> PlayerCard <$> genPlayerCard def (Just pid)) [Allies.gondorianSpearman, Allies.gondorianSpearman, Allies.gondorianSpearman, Allies.gondorianSpearman]
      putGame . (playersL . ix pid . deckL .~ cards) =<< getGame
      run $ CheckWindows [mkAfter (CommittedToQuest beravor)]
      chooseAbilityOf pid hunt
      chooseTarget pid (CardIdTarget c.id)
      chooseTarget pid (CardIdTarget a.id)
      chooseTarget pid (CardIdTarget b.id)
      map (.id) . (.deck) <$> getPlayer pid `shouldReturn` map (.id) [c, d, a, b]

  describe "Unbroken" do
    it "gives +1 defense, and a card discarded from hand heals 1 damage from its hero" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      unbroken <- putAttachmentOn pid Attachments.unbroken beravor
      getDefense beravor `shouldReturn` 3
      spare <- putCardIntoHand pid Allies.gondorianSpearman
      run $ PlaceTokens GameSource beravor Damage 2
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid unbroken
      chooseTarget pid (CardIdTarget spare.id)
      getCharacterDamage beravor `shouldReturn` 1

  describe "The Steadfast" do
    it "places 1 progress on the current quest as its hero commits, its player's threat 40 or more" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      steadfast <- putAttachmentOn pid Attachments.theSteadfast beravor
      q <- putQuestStage Quests.fliesAndSpiders
      getWillpower beravor `shouldReturn` 3
      setThreat pid 40
      run $ CheckWindows [mkAfter (CommittedToQuest beravor)]
      chooseAbilityOf pid steadfast
      _ <- pure q
      (.progress) <$> getActiveQuest `shouldReturn` 1

  describe "Dragon-slayer" do
    it "engages a non-unique enemy added to the staging area, which attacks at once" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      slayer <- putAttachmentOn pid Attachments.dragonSlayer beravor
      getAttack beravor `shouldReturn` 3
      _ <- setEncounterDeck [Locations.oldForestRoad]
      eid <- stagedEnemy Enemies.kingSpider
      chooseAbilityOf pid slayer
      getsGame (fmap (.placement) . find ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` Just (EngagedWith pid)

  describe "A Tale of Victories" do
    it "draws a card for each engaged enemy and heals 3 from the Unbroken hero, canceling 3 of its next damage" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.unbroken beravor
      _ <- engagedEnemy pid Enemies.kingSpider
      _ <- engagedEnemy pid Enemies.forestSpider
      run $ PlaceTokens GameSource beravor Damage 3
      handSize <- length . (.hand) <$> getPlayer pid
      card <- putCardIntoHand pid Events.aTaleOfVictories
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource theodred Resource 1
      inPhase CombatPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      length . (.hand) <$> getPlayer pid `shouldReturn` handSize + 2
      getCharacterDamage beravor `shouldReturn` 0
      passActions pid
      run $ DealDamage GameSource beravor 2
      getCharacterDamage beravor `shouldReturn` 0
      run $ DealDamage GameSource beravor 2
      getCharacterDamage beravor `shouldReturn` 2

  describe "Rhyme of the Slayer" do
    it "gives the enemy -4 attack, and the Dragon-slayer hero +4 attack against it" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.dragonSlayer beravor
      eid <- engagedEnemy pid Enemies.hillTroll
      chooseLabel "$label.pass" pid
      card <- putCardIntoHand pid Events.rhymeOfTheSlayer
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource theodred Resource 1
      inPhase CombatPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid eid
      -- the troll's 6, less 4
      getEnemyAttack eid `shouldReturn` 2
      getModifiers beravor >>= (`shouldSatisfy` elem (AttackBonusAgainstMatching (EnemyWithId eid) 4))

  describe "Lay of Courage" do
    it "cancels a When Revealed effect, readies the Steadfast hero and goes to the victory display" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.theSteadfast beravor
      _ <- setEncounterDeck [Treacheries.theNecromancersReach, Locations.oldForestRoad]
      run $ Exhaust beravor
      card <- putCardIntoHand pid Events.layOfCourage
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource theodred Resource 1
      inPhase QuestPhase
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget card.id)
      getCharacterDamage beravor `shouldReturn` 0
      heroExhausted . toAttrs <$> getHeroWithCode beravorCode `shouldReturn` False
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) >>= (`shouldSatisfy` elem (toCardCode Events.layOfCourage))

  describe "The Canny Huntsman" do
    it "puts the encounter deck's top cards back on top or bottom and gives the Master of the Hunt hero +3 willpower" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.masterOfTheHunt beravor
      [spider, road] <- setEncounterDeck [Enemies.kingSpider, Locations.oldForestRoad]
      card <- putCardIntoHand pid Events.theCannyHuntsman
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource theodred Resource 1
      inPhase QuestPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid (CardIdTarget spider.id)
      chooseLabel "$label.theCannyHuntsman.onBottom" pid
      chooseTarget pid (CardIdTarget road.id)
      chooseLabel "$label.theCannyHuntsman.onTop" pid
      map (.id) <$> getEncounterDeck `shouldReturn` [road.id, spider.id]
      getWillpower beravor `shouldReturn` 5
