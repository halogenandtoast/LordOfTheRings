module LordOfTheRings.Haradrim.TheMumakilPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (AdditionalResourcePhaseDraws, CannotAttackPlayer))
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (victoryDisplayL)
import TestImport

kahlielCode, beravorCode, theodredCode :: CardCode
kahlielCode = "17001"
beravorCode = "01012"
theodredCode = "01002"

withKahliel :: DeckList
withKahliel = deckList [kahlielCode, beravorCode, theodredCode] testDeck.cards

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

allyIn :: Card -> GameT (Maybe Target)
allyIn card = getsGame (fmap toTarget . find ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  _ -> pure False

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  existing <- getsGame (Map.keys . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  eid <- getsGame (fromJustNote "no enemy" . find (`notElem` existing) . Map.keys . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

spec :: Spec
spec = describe "The Mûmakil player cards" do
  describe "Kahliel" do
    it "pays for a Harad ally of any sphere" . gameTestWith withKahliel $ \pid -> do
      card <- playWith pid kahlielCode 5 Allies.yazan
      allyIn card >>= (`shouldSatisfy` isJust)

    it "discards a Harad ally from hand to ready a Harad character" . gameTestWith withKahliel $ \pid -> do
      yazan <- putAllyIntoPlay pid Allies.yazan
      run $ Exhaust yazan
      card <- putCardIntoHand pid Allies.kahlielsTribesman
      run $ ActionWindow pid
      chooseAbilityOf pid =<< hero kahlielCode
      isExhausted yazan `shouldReturn` False
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [card.id]

  describe "Andrath Guardsman" do
    it "keeps a non-unique engaged enemy from attacking you this round" . gameTest $ \pid -> do
      eid <- engagedEnemy pid Enemies.forestSpider
      card <- playWith pid theodredCode 2 Allies.andrathGuardsman
      guardsman <- fromJustNote "not in play" <$> allyIn card
      chooseAbilityOf pid guardsman
      getModifiers eid >>= (`shouldSatisfy` elem (CannotAttackPlayer pid))

  describe "Prepare for Battle" do
    it "has the first player draw an additional card in the resource phase from the victory display" . gameTest $ \pid -> do
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem (AdditionalResourcePhaseDraws 1))
      card <- PlayerCard <$> genPlayerCard Quests.prepareForBattle (Just pid)
      putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (card :))) =<< getGame
      run $ CheckWindows []
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (AdditionalResourcePhaseDraws 1))

  describe "Kahliel's Headdress" do
    it "gives each Harad character +1 willpower" . gameTestWith withKahliel $ \pid -> do
      kahliel <- hero kahlielCode
      yazan <- putAllyIntoPlay pid Allies.yazan
      _ <- putAttachmentOn pid Attachments.kahlielsHeaddress kahliel
      getWillpower kahliel `shouldReturn` 3
      getWillpower yazan `shouldReturn` 3
      (getWillpower =<< hero beravorCode) `shouldReturn` 2

    it "shuffles the topmost Harad ally in your discard pile into your deck in the refresh phase" . gameTestWith withKahliel $ \pid -> do
      headdress <- putAttachmentOn pid Attachments.kahlielsHeaddress =<< hero kahlielCode
      firyal <- PlayerCard <$> genPlayerCard Allies.firyal (Just pid)
      jubayr <- PlayerCard <$> genPlayerCard Allies.jubayr (Just pid)
      guard' <- PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid)
      putGame . (playersL . ix pid . discardL .~ [guard', firyal, jubayr]) =<< getGame
      run $ BeginPhase RefreshPhase
      chooseAbilityOf pid headdress
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [guard'.id, jubayr.id]
      elem firyal.id . map (.id) . (.deck) <$> getPlayer pid `shouldReturn` True
