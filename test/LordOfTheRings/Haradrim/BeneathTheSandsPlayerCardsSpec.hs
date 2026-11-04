module LordOfTheRings.Haradrim.BeneathTheSandsPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (DoesNotExhaustToCommit, GainTrait))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (scenarioVictoryDisplay)
import LordOfTheRings.Trait (Trait (..))
import LordOfTheRings.Window (WindowType (Revealed, StagingStepBegins), mkAfter, mkWhen)
import TestImport

beravorCode, eowynCode, theodredCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- enemyIds
  run . AddToStaging =<< genEncounterCard def
  fromJustNote "no enemy" . find (`notElem` existing) <$> enemyIds

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  eid <- stagedEnemy def
  run $ EngageEnemy pid eid
  pure eid

attachmentCodes :: GameT [CardCode]
attachmentCodes = getsGame (map toCardCode . toList . (.entities.attachments))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

-- | The player's side quest put into play and defeated, into the victory display.
defeatSideQuest :: PlayerId -> CardDef -> GameT CardId
defeatSideQuest pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PutSideQuestIntoPlay pid card
  qid <- getsGame (fromJustNote "no side quest" . fmap toId . find ((== card.id) . questCardId . toAttrs) . toList . (.entities.quests))
  run $ CompleteQuest qid
  pure card.id

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

spec :: Spec
spec = describe "Beneath the Sands player cards" do
  describe "Haradrim Spear" do
    it "gives a Harad character +1 attack, +3 more for an attack it is declared in, then is discarded" . gameTest $ \pid -> do
      firyal <- putAllyIntoPlay pid Allies.firyal
      _ <- putAttachmentOn pid Attachments.haradrimSpear firyal
      getAttack firyal `shouldReturn` 2
      troll <- engagedEnemy pid Enemies.hillTroll
      run $ PlayerAttacks pid
      chooseTarget pid firyal
      spear <- getsGame (toTarget . fromJustNote "no spear" . headMay . toList . (.entities.attachments))
      chooseAbilityOf pid spear
      getAttack firyal `shouldReturn` 5
      chooseLabel "$label.doneAttacking" pid
      -- the action window before the attack resolves
      passActions pid
      passResponses pid
      -- 5 attack against the troll's 3 defense
      getDamage (toTarget troll) `shouldReturn` 2
      attachmentCodes `shouldReturn` []

  describe "Hunting Party" do
    it "discards a non-unique enemy in the staging area as the staging step begins, then reveals a card" . gameTestWith (deckList ["01005", "01002", "01007"] testDeck.cards) $ \pid -> do
      -- Legolas (a Warrior) and Firyal (a unique Scout)
      _ <- putAllyIntoPlay pid Allies.firyal
      spider <- stagedEnemy Enemies.forestSpider
      [_] <- setEncounterDeck [Locations.oldForestRoad]
      party <- putCardIntoHand pid Events.huntingParty
      give theodredCode 1
      run $ CheckWindows [mkAfter StagingStepBegins]
      chooseTarget pid (CardIdTarget party.id)
      enemyIds >>= (`shouldSatisfy` notElem spider)
      getEncounterDeck `shouldReturn` []

    it "is not offered without a unique Scout" . gameTestWith (deckList ["01005", "01002", "01007"] testDeck.cards) $ \pid -> do
      _ <- stagedEnemy Enemies.forestSpider
      party <- putCardIntoHand pid Events.huntingParty
      give theodredCode 1
      run $ CheckWindows [mkAfter StagingStepBegins]
      q <- getQuestion pid
      let offered = maybe False (any \case TargetLabel t _ -> t == CardIdTarget party.id; _ -> False) (questionChoices <$> q)
      offered `shouldBe` False

  describe "Keep Watch" do
    it "gives each non-unique engaged enemy -1 attack from the victory display" . gameTest $ \pid -> do
      spider <- engagedEnemy pid Enemies.forestSpider
      base <- getEnemyAttack spider
      _ <- defeatSideQuest pid Quests.keepWatch
      victoryDisplay `shouldReturn` [toCardCode Quests.keepWatch]
      getEnemyAttack spider `shouldReturn` base - 1

    it "sends a second copy defeated to its owner's discard pile" . gameTest $ \pid -> do
      _ <- defeatSideQuest pid Quests.keepWatch
      copy <- defeatSideQuest pid Quests.keepWatch
      victoryDisplay `shouldReturn` [toCardCode Quests.keepWatch]
      map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem copy)

  describe "Rider of Rohan" do
    it "does not exhaust to quest while a side quest is in the victory display" . gameTest $ \pid -> do
      rider <- putAllyIntoPlay pid Allies.riderOfRohan
      getModifiers rider >>= (`shouldSatisfy` notElem DoesNotExhaustToCommit)
      _ <- defeatSideQuest pid Quests.keepWatch
      getModifiers rider >>= (`shouldSatisfy` elem DoesNotExhaustToCommit)

  describe "Fireside Song" do
    it "gives its Hobbit hero +1 willpower for each Song attached" . gameTestWith (deckList ["02025", "01011", "01007"] testDeck.cards) $ \pid -> do
      frodo <- hero "02025"
      _ <- putAttachmentOn pid Attachments.firesideSong frodo
      getWillpower frodo `shouldReturn` 3

  describe "Dúnedain Lookout" do
    it "is discarded to cancel the when revealed effects of an enemy" . gameTest $ \pid -> do
      lookout <- putAllyIntoPlay pid Allies.dunedainLookout
      eid <- stagedEnemy Enemies.forestSpider
      run $ CheckWindows [mkWhen $ Revealed (toTarget eid)]
      -- Forest Spider has no When Revealed effect: nothing to cancel
      offersAbilityOf pid lookout `shouldReturn` False
      king <- stagedEnemy Enemies.kingSpider
      run $ CheckWindows [mkWhen $ Revealed (toTarget king)]
      chooseAbilityOf pid lookout
      getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` []

  describe "Doughty Ranger" do
    it "gives its hero the Ranger trait, and draws a card as it is played" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      handBefore <- length . (.hand) <$> getPlayer pid
      _ <- playWith pid beravorCode 1 Attachments.doughtyRanger
      chooseTarget pid beravor
      ranger <- getsGame (toTarget . fromJustNote "no attachment" . headMay . toList . (.entities.attachments))
      chooseAbilityOf pid ranger
      length . (.hand) <$> getPlayer pid `shouldReturn` handBefore + 1
      getModifiers beravor >>= (`shouldSatisfy` elem (GainTrait Ranger))

  describe "The Free Peoples" do
    it "needs 9 different traits among your characters" . gameTest $ \pid -> do
      -- Beravor (Dúnedain, Ranger), Éowyn (Noble, Rohan), Théodred (Noble, Rohan, Warrior)
      peoples <- putCardIntoHand pid Events.theFreePeoples
      give theodredCode 5
      run $ ActionWindow pid
      q <- getQuestion pid
      let offered = maybe False (any \case TargetLabel t _ -> t == CardIdTarget peoples.id; _ -> False) (questionChoices <$> q)
      offered `shouldBe` False

    it "readies each character and gives yours +1 willpower" . gameTest $ \pid -> do
      -- with Firyal (Harad, Scout), Booming Ent (Ent) and Dwarven Sellsword (Dwarf, Warrior)
      traverse_ (putAllyIntoPlay pid) [Allies.firyal, Allies.boomingEnt, Allies.dwarvenSellsword]
      eowyn <- hero eowynCode
      run $ Exhaust eowyn
      peoples <- putCardIntoHand pid Events.theFreePeoples
      give theodredCode 5
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget peoples.id)
      exhausted <- heroExhausted . toAttrs <$> getHeroWithCode eowynCode
      exhausted `shouldBe` False
      getWillpower eowyn `shouldReturn` 5
