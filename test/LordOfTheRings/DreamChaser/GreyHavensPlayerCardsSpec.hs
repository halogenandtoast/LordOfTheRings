module LordOfTheRings.DreamChaser.GreyHavensPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getCommittedWillpower, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Window (WindowType (PhaseBegins), mkAfter)
import TestImport

cirdanCode, galdorCode, legolasCode, beravorCode, theodredCode, eowynCode :: CardCode
cirdanCode = "11001"
galdorCode = "11002"
legolasCode = "01005"
beravorCode = "01012"
theodredCode = "01002"
eowynCode = "01007"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | Círdan (spirit), Beravor (lore) and Théodred (leadership).
cirdanTest :: (PlayerId -> GameT ()) -> IO ()
cirdanTest = gameTestWith (deckList [cirdanCode, beravorCode, theodredCode] testDeck.cards)

-- | Galdor (lore), Beravor and Théodred.
galdorTest :: (PlayerId -> GameT ()) -> IO ()
galdorTest = gameTestWith (deckList [galdorCode, beravorCode, theodredCode] testDeck.cards)

-- | Legolas (tactics, ranged), Beravor and Théodred.
legolasTest :: (PlayerId -> GameT ()) -> IO ()
legolasTest = gameTestWith (deckList [legolasCode, beravorCode, theodredCode] testDeck.cards)

hand :: PlayerId -> GameT [Card]
hand pid = (.hand) <$> getPlayer pid

discardPile :: PlayerId -> GameT [Card]
discardPile pid = (.discard) <$> getPlayer pid

-- | A copy of the card on top of the player's discard pile.
topOfDiscard :: PlayerId -> CardDef -> GameT ()
topOfDiscard pid def = run . PutInDiscard =<< genPlayerCard def (Just pid)

setPhase :: Phase -> GameT ()
setPhase p = putGame . (phaseL .~ p) =<< getGame

resources :: CardCode -> Int -> GameT ()
resources code n = hero code >>= \t -> run (PlaceTokens GameSource t Resource n)

exhausted :: Target -> GameT Bool
exhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  _ -> pure False

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- getsGame (Map.keys . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` existing) . Map.keys . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  existing <- getsGame (Map.keys . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . find (`notElem` existing) . Map.keys . (.entities.locations))

progressOn :: LocationId -> GameT Int
progressOn lid = (.progress) <$> getLocation lid

-- | Play the card from hand in an action window.
playFromHand :: PlayerId -> Card -> GameT ()
playFromHand pid card = do
  run $ ActionWindow pid
  chooseTarget pid (CardIdTarget card.id)

targetsOf :: Question Message -> [Target]
targetsOf q = [t | TargetLabel t _ <- questionChoices q]

offered :: PlayerId -> GameT [Target]
offered pid = maybe [] targetsOf <$> getQuestion pid

spec :: Spec
spec = describe "The Grey Havens player cards" do
  describe "Círdan the Shipwright" do
    it "draws 1 additional card in the resource phase, then discards 1 of those drawn" . cirdanTest $ \pid -> do
      flipFirstQuest pid
      handSize <- length <$> hand pid
      drawn <- take 2 . (.deck) <$> getPlayer pid
      run $ BeginPhase ResourcePhase
      cirdan <- hero cirdanCode
      whenM (offersAbilityOf pid cirdan) $ chooseAbilityOf pid cirdan
      offered pid >>= (`shouldBe` map (CardIdTarget . (.id)) drawn)
      for_ (take 1 drawn) \c -> chooseTarget pid (CardIdTarget c.id)
      length <$> hand pid `shouldReturn` handSize + 1
      map (.id) <$> discardPile pid `shouldReturn` map (.id) (take 1 drawn)

  describe "Galdor of the Havens" do
    it "discards any number from the setup hand instead of a mulligan, drawing as many" . galdorTest $ \pid -> do
      run Setup
      galdor <- hero galdorCode
      chooseAbilityOf pid galdor
      setup <- hand pid
      for_ (take 2 setup) \c -> chooseTarget pid (CardIdTarget c.id)
      chooseLabel "$label.doneDiscarding" pid
      length <$> hand pid `shouldReturn` 6
      length <$> discardPile pid `shouldReturn` 2
      getQuestion pid >>= (`shouldSatisfy` \case
        Just (QuestionLabel "$label.keepStartingHand" _ _ _) -> False
        _ -> True)

    it "draws 6 cards with no cards in hand, once per game" . galdorTest $ \pid -> do
      putGame . (playersL . ix pid . handL .~ []) =<< getGame
      galdor <- hero galdorCode
      run $ ActionWindow pid
      chooseAbilityOf pid galdor
      length <$> hand pid `shouldReturn` 6

  describe "Mithlond Sea-watcher" do
    it "gets +2 attack and ranged while an ally tops your discard pile" . gameTest $ \pid -> do
      watcher <- putAllyIntoPlay pid Allies.mithlondSeaWatcher
      getAttack watcher `shouldReturn` 1
      topOfDiscard pid Allies.mithlondSeaWatcher
      getAttack watcher `shouldReturn` 3
      getModifiers watcher >>= (`shouldSatisfy` elem (GainKeyword Ranged))
      topOfDiscard pid Events.theEveningStar
      getAttack watcher `shouldReturn` 1

  describe "Warden of the Havens" do
    it "gets +2 defense and sentinel while an attachment tops your discard pile" . gameTest $ \pid -> do
      warden <- putAllyIntoPlay pid Allies.wardenOfTheHavens
      topOfDiscard pid Attachments.grapplingHook
      getDefense warden `shouldReturn` 3
      getModifiers warden >>= (`shouldSatisfy` elem (GainKeyword Sentinel))

  describe "Sailor of Lune" do
    it "gets +1 willpower, and cannot be damaged committed, while an event tops your discard pile" . gameTest $ \pid -> do
      sailor <- putAllyIntoPlay pid Allies.sailorOfLune
      topOfDiscard pid Events.theEveningStar
      getWillpower sailor `shouldReturn` 2
      getModifiers sailor >>= (`shouldSatisfy` notElem CannotTakeDamage)
      run $ CommitToQuest sailor
      getModifiers sailor >>= (`shouldSatisfy` elem CannotTakeDamage)

  describe "Lindon Navigator" do
    it "commits while exhausted, without exhausting" . gameTest $ \pid -> do
      navigator <- putAllyIntoPlay pid Allies.lindonNavigator
      run $ Exhaust navigator
      run $ CommitCharacters pid
      offered pid >>= (`shouldSatisfy` elem navigator)

    it "is discarded, or a card from hand, after the quest it was committed to resolves" . gameTest $ \pid -> do
      navigator <- putAllyIntoPlay pid Allies.lindonNavigator
      _ <- putCardIntoHand pid Events.theEveningStar
      run $ CommitToQuest navigator
      exhausted navigator `shouldReturn` False
      run QuestResolution
      chooseLabel "$label.lindonNavigator.discardSelf" pid
      getsGame (length . (.entities.allies)) `shouldReturn` 0

  describe "Skyward Volley" do
    it "exhausts a ranged character to deal 2 damage, again for each copy in your discard pile" . legolasTest $ \pid -> do
      legolas <- hero legolasCode
      troll <- stagedEnemy Enemies.hillTroll
      run $ EngageEnemy pid troll
      topOfDiscard pid Events.skywardVolley
      card <- putCardIntoHand pid Events.skywardVolley
      resources legolasCode 2
      setPhase CombatPhase
      playFromHand pid card
      chooseTarget pid troll
      chooseTarget pid troll
      getCharacterDamage (toTarget troll) `shouldReturn` 4
      exhausted legolas `shouldReturn` True

  describe "Anchor Watch" do
    it "declares exhausted characters as the defenders, one more for each copy in your discard pile" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      theodred <- hero theodredCode
      spider <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid spider
      run $ Exhaust beravor
      run $ Exhaust theodred
      topOfDiscard pid Events.anchorWatch
      card <- putCardIntoHand pid Events.anchorWatch
      resources theodredCode 2
      run $ EnemyAttack pid spider
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid beravor
      chooseTarget pid theodred
      passActions pid
      -- the attack is defended by both, its damage given to one of them
      offered pid >>= (`shouldBe` [beravor, theodred])

  describe "Grappling Hook" do
    it "commits the attached character to the quest with its attack" . legolasTest $ \pid -> do
      legolas <- hero legolasCode
      hook <- putAttachmentOn pid Attachments.grapplingHook legolas
      setPhase QuestPhase
      run $ ActionWindow pid
      chooseAbilityOf pid hook
      exhausted legolas `shouldReturn` True
      getCommittedWillpower `shouldReturn` 3
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

  describe "Mariner's Compass" do
    it "switches a location in the top 5 of the encounter deck with one in the staging area" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      compass <- putAttachmentOn pid Attachments.marinersCompass theodred
      staged <- stagedLocation Locations.mountainsOfMirkwood
      [_, road] <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad]
      run $ CheckWindows [mkAfter $ PhaseBegins TravelPhase]
      chooseAbilityOf pid compass
      chooseTarget pid (CardIdTarget road.id)
      chooseTarget pid staged
      exhausted theodred `shouldReturn` True
      locations <- getsGame (toList . (.entities.locations))
      map toCardCode locations `shouldBe` [toCardCode Locations.oldForestRoad]
      deck <- getEncounterDeck
      sort (map toCardCode deck) `shouldBe` sort [toCardCode Enemies.forestSpider, toCardCode Locations.mountainsOfMirkwood]

  describe "The Evening Star" do
    it "places 2 progress on a location, again for each copy in your discard pile" . gameTest $ \pid -> do
      one <- stagedLocation Locations.mountainsOfMirkwood
      two <- stagedLocation Locations.oldForestRoad
      topOfDiscard pid Events.theEveningStar
      card <- putCardIntoHand pid Events.theEveningStar
      resources beravorCode 2
      playFromHand pid card
      chooseTarget pid one
      chooseTarget pid two
      progressOn one `shouldReturn` 2
      progressOn two `shouldReturn` 2

  describe "Explorer's Almanac" do
    it "lets questing progress be placed on its location before the quest" . gameTest $ \pid -> do
      flipFirstQuest pid
      road <- stagedLocation Locations.oldForestRoad
      _ <- putAttachmentOn pid Attachments.explorersAlmanac (toTarget road)
      run $ PlaceProgressOnQuest 5
      chooseTarget pid road
      -- explored by the 3 it took, the rest on the quest
      getsGame (Map.member road . (.entities.locations)) `shouldReturn` False
      (.progress) <$> getActiveQuest `shouldReturn` 2

  describe "Elwing's Flight" do
    it "readies a questing character with +1 willpower, again for each copy in your discard pile" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      theodred <- hero theodredCode
      run $ CommitToQuest beravor
      run $ CommitToQuest theodred
      topOfDiscard pid Events.elwingsFlight
      card <- putCardIntoHand pid Events.elwingsFlight
      resources eowynCode 2
      setPhase QuestPhase
      playFromHand pid card
      chooseTarget pid beravor
      chooseTarget pid theodred
      exhausted beravor `shouldReturn` False
      exhausted theodred `shouldReturn` False
      getWillpower beravor `shouldReturn` 3
      getWillpower theodred `shouldReturn` 2

  describe "To the Sea, to the Sea!" do
    it "discards X cards to lower the next Noldor ally's cost by X" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      song <- putAttachmentOn pid Attachments.toTheSeaToTheSea beravor
      replicateM_ 3 $ putCardIntoHand pid Events.theEveningStar
      run $ ActionWindow pid
      chooseAbilityOf pid song
      -- X = 2
      void $ answerQuestion pid 1
      cards <- hand pid
      for_ (take 2 cards) \c -> chooseTarget pid (CardIdTarget c.id)
      length <$> discardPile pid `shouldReturn` 2
      getModifiers GameTarget >>= (`shouldSatisfy` any \case ReduceNextCostOf _ 2 -> True; _ -> False)

  describe "Narya" do
    it "gives a leadership icon, and readies up to 2 allies with +1 attack and defense" . cirdanTest $ \pid -> do
      cirdan <- hero cirdanCode
      narya <- putAttachmentOn pid Attachments.narya cirdan
      getModifiers cirdan >>= (`shouldSatisfy` elem (GainResourceIcon Leadership))
      one <- putAllyIntoPlay pid Allies.wardenOfTheHavens
      two <- putAllyIntoPlay pid Allies.sailorOfLune
      run $ Exhaust one
      run $ Exhaust two
      run $ ActionWindow pid
      chooseAbilityOf pid narya
      chooseTarget pid one
      chooseTarget pid two
      exhausted one `shouldReturn` False
      exhausted two `shouldReturn` False
      exhausted cirdan `shouldReturn` True
      getAttack one `shouldReturn` 2
      getDefense two `shouldReturn` 1
