module LordOfTheRings.VengeanceOfMordor.ChallengeOfTheWainridersPlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (EnemyAttrs (enemyCardId))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message.Lifted (lowerThreatBy)
import LordOfTheRings.Modifier (ModifierType (NoEngagementChecksAgainst))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

sarumanCode, grimaCode, beravorCode, eowynCode, theodredCode :: CardCode
sarumanCode = "22054"
grimaCode = "07002"
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"

-- | Saruman (lore), Gríma (lore) and Éowyn (spirit).
sarumanTest :: (PlayerId -> GameT ()) -> IO ()
sarumanTest = gameTestWith (deckList [sarumanCode, grimaCode, eowynCode] testDeck.cards)

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  AttachmentTarget aid -> maybe False (attachmentExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.attachments)
  _ -> pure False

onTopOfDeck :: PlayerId -> [CardDef] -> GameT [Card]
onTopOfDeck pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (cards <>)
  pure cards

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies)) <&> toId

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

allyCodes :: GameT [CardCode]
allyCodes = getsGame (map toCardCode . toList . (.entities.allies))

spec :: Spec
spec = describe "Challenge of the Wainriders player cards" do
  describe "Saruman" do
    it "reduces a player card's threat reduction to 1, not an encounter card's" . sarumanTest $ \pid -> do
      saruman <- getHeroWithCode sarumanCode
      run $ RaiseThreat pid 10
      runQueueT $ lowerThreatBy (HeroSource saruman.id) pid 3
      _ <- mainLoop
      threat pid `shouldReturn` 9
      runQueueT $ lowerThreatBy GameSource pid 3
      _ <- mainLoop
      threat pid `shouldReturn` 6

    it "pays for an event with printed Doomed of another sphere" . sarumanTest $ \pid -> do
      -- Reckless Attack is tactics: only Saruman's gained icon matches it
      _ <- playWith pid sarumanCode 0 Events.recklessAttack
      threat pid `shouldReturn` 2

    it "readies after you play a card with Doomed" . sarumanTest $ \pid -> do
      saruman <- hero sarumanCode
      run $ Exhaust saruman
      _ <- playWith pid sarumanCode 0 Events.deepKnowledge
      chooseAbilityOf pid saruman
      isExhausted saruman `shouldReturn` False

    it "readies after you play a card that gained Doomed from Gríma" . sarumanTest $ \pid -> do
      saruman <- hero sarumanCode
      run $ Exhaust saruman
      scout <- putCardIntoHand pid Allies.rohirrimScout
      grima <- hero grimaCode
      -- the scout's 2 less Gríma's 1
      eowyn <- hero eowynCode
      run $ PlaceTokens GameSource eowyn Resource 1
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid grima
      chooseTarget pid (CardIdTarget scout.id)
      threat pid `shouldReturn` 1
      chooseAbilityOf pid saruman
      isExhausted saruman `shouldReturn` False

  describe "Horns! Horns! Horns!" do
    it "puts an ally into play from hand, shuffled into the deck at the end of the phase" . gameTest $ \pid -> do
      lancer <- putCardIntoHand pid Allies.westfoldLancer
      _ <- playWith pid theodredCode 0 Events.hornsHornsHorns
      threat pid `shouldReturn` 2
      chooseTarget pid (CardIdTarget lancer.id)
      passResponses pid
      allyCodes `shouldReturn` [toCardCode Allies.westfoldLancer]
      passActions pid
      run $ EndPhase PlanningPhase
      allyCodes `shouldReturn` []
      elem lancer.id . map (.id) . (.deck) <$> getPlayer pid `shouldReturn` True

  describe "Soldier of Isengard" do
    it "passes to a player who raises his threat from Doomed" . twoPlayerTest testDeck $ \one two -> do
      soldier <- putAllyIntoPlay one Allies.soldierOfIsengard
      passResponses one
      run $ CheckWindows [mkAfter $ ThreatRaisedByDoomed one]
      offersAbilityOf one soldier `shouldReturn` False
      run $ CheckWindows [mkAfter $ ThreatRaisedByDoomed two]
      chooseAbilityOf one soldier
      getsGame (fmap (placementController . (.placement)) . find ((== soldier) . toTarget) . toList . (.entities.allies))
        `shouldReturn` Just (Just two)

  describe "Far-sighted" do
    it "shows the top 5 cards of the encounter deck and leaves them in order" . gameTest $ \pid -> do
      deck <- setEncounterDeck (replicate 6 Enemies.dolGuldurOrcs)
      _ <- playWith pid beravorCode 0 Events.farSighted
      threat pid `shouldReturn` 1
      chooseLabel "$label.farSighted.done" pid
      map (.id) <$> getEncounterDeck `shouldReturn` map (.id) deck

  describe "Nori" do
    it "puts the top card of the discard pile on the bottom of the deck after his attack destroys an enemy" . gameTest $ \pid -> do
      nori <- putAllyIntoPlay pid Allies.noriTactics
      card <- PlayerCard <$> genPlayerCard Events.deepKnowledge (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . discardL .~ [card]
      eid <- stagedEnemy Enemies.dolGuldurOrcs
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [nori] mempty]
      chooseAbilityOf pid nori
      fmap (.id) . lastMay . (.deck) <$> getPlayer pid `shouldReturn` Just card.id
      (.discard) <$> getPlayer pid `shouldReturn` []

  describe "Golden Belt" do
    it "lets its hero keep a third restricted attachment, until it leaves" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      belt <- putAttachmentOn pid Attachments.goldenBelt eowyn
      for_ [Attachments.warAxe, Attachments.valiantSword, Attachments.hornOfTheMark] \def ->
        putAttachmentOn pid def eowyn
      run $ CheckRestricted eowyn
      getQuestion pid >>= (`shouldSatisfy` isNothing)
      run $ Discard belt
      getQuestion pid >>= \case
        Just (QuestionLabel l _ _ _) -> l `shouldBe` "$label.discardRestricted"
        q -> error $ "expected the restricted discard, got " <> show q

  describe "Rohirrim Scout" do
    it "is discarded so a non-unique staged enemy makes no engagement checks against you" . gameTest $ \pid -> do
      scout <- putAllyIntoPlay pid Allies.rohirrimScout
      eid <- stagedEnemy Enemies.dolGuldurOrcs
      run $ ActionWindow pid
      chooseAbilityOf pid scout
      allyCodes `shouldReturn` []
      getModifiers eid >>= (`shouldSatisfy` elem (NoEngagementChecksAgainst pid))

  describe "The Muster of Rohan" do
    it "is paid by a Rohan hero as spirit and puts Rohan allies into play" . gameTest $ \pid -> do
      [lancer] <- onTopOfDeck pid [Allies.westfoldLancer]
      _ <- playWith pid theodredCode 4 Events.theMusterOfRohan
      chooseTarget pid (CardIdTarget lancer.id)
      passResponses pid
      chooseLabel "$label.done" pid
      allyCodes `shouldReturn` [toCardCode Allies.westfoldLancer]

  describe "Saruman's Staff" do
    it "lowers the next Doomed event's X by 2" . sarumanTest $ \pid -> do
      staff <- putAttachmentOn pid Attachments.sarumansStaff =<< hero sarumanCode
      run $ ActionWindow pid
      chooseAbilityOf pid staff
      chooseLabel "$label.sarumansStaff.reduce" pid
      isExhausted staff `shouldReturn` True
      passActions pid
      _ <- playWith pid sarumanCode 0 Events.deepKnowledge
      threat pid `shouldReturn` 0

    it "searches the top 5 cards for a Doomed event" . sarumanTest $ \pid -> do
      staff <- putAttachmentOn pid Attachments.sarumansStaff =<< hero sarumanCode
      [knowledge] <- onTopOfDeck pid [Events.deepKnowledge]
      run $ ActionWindow pid
      chooseAbilityOf pid staff
      chooseLabel "$label.sarumansStaff.search" pid
      chooseTarget pid (CardIdTarget knowledge.id)
      elem knowledge.id . map (.id) . (.hand) <$> getPlayer pid `shouldReturn` True
