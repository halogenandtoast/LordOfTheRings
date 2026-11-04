module LordOfTheRings.AngmarAwakened.CarnDumAndDreadRealmPlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (AllyMatcher (AllyControlledBy), HeroMatcher (HeroControlledBy), PlayerMatcher (PlayerWithId), select)
import LordOfTheRings.Modifier (ModifierType (AttackModifier, DoesNotExhaustToAttack, GainResourceIcon, WillpowerModifier))
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Window
import TestImport

amarthiulCode, beravorCode, eowynCode :: CardCode
amarthiulCode = "10115"
beravorCode = "01012"
eowynCode = "01007"

-- | Amarthiúl (leadership, Dúnedain Ranger), Beravor (lore) and Éowyn (spirit).
amarthiulTest :: (PlayerId -> GameT ()) -> IO ()
amarthiulTest = gameTestWith (deckList [amarthiulCode, beravorCode, eowynCode] testDeck.cards)

legolasCode, theodredCode :: CardCode
legolasCode = "01005"
theodredCode = "01002"

-- | Legolas (tactics), Beravor (lore) and Éowyn (spirit).
tacticsTest :: (PlayerId -> GameT ()) -> IO ()
tacticsTest = gameTestWith (deckList [legolasCode, beravorCode, eowynCode] testDeck.cards)

-- | Give the hero resources to pay with.
fund :: CardCode -> Int -> GameT ()
fund code n = do
  h <- getHeroWithCode code
  run $ PlaceTokens GameSource (toTarget h) Resource n

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

-- | Read a stat once the modifiers are brought up to date.
stat :: (Target -> GameT Int) -> Target -> GameT Int
stat f t = run (Run []) >> f t

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  eid <- getsGame (toId . fromJustNote "no enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

putInDiscard :: PlayerId -> CardDef -> GameT Card
putInDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . discardL %~ (card :)
  pure card

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

-- | The player's attachment of the card, wherever it is.
attachmentOf :: CardDef -> GameT (Maybe Placement)
attachmentOf def = getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

spec :: Spec
spec = describe "The Battle of Carn Dûm and The Dread Realm player cards" do
  describe "Amarthiúl" do
    it "gains the tactics icon engaged with an enemy, and collects 2 resources engaged with 2" . amarthiulTest $ \pid -> do
      amarthiul <- hero amarthiulCode
      _ <- engagedEnemy pid Enemies.forestSpider
      run (Run [])
      getModifiers amarthiul >>= (`shouldSatisfy` elem (GainResourceIcon Tactics))
      _ <- engagedEnemy pid Enemies.forestSpider
      was <- getHeroResources . toId =<< getHeroWithCode amarthiulCode
      run $ BeginPhase ResourcePhase
      now <- getHeroResources . toId =<< getHeroWithCode amarthiulCode
      now - was `shouldBe` 2

  describe "Guardian of Arnor and Fornost Bowman" do
    it "get +1 defense and +1 attack for each enemy engaged with you" . gameTest $ \pid -> do
      guardian <- putAllyIntoPlay pid Allies.guardianOfArnor
      bowman <- putAllyIntoPlay pid Allies.fornostBowman
      _ <- engagedEnemy pid Enemies.forestSpider
      _ <- engagedEnemy pid Enemies.forestSpider
      stat getDefense guardian `shouldReturn` 3
      stat getAttack bowman `shouldReturn` 3

  describe "Doom Hangs Still" do
    it "keeps questing unsuccessfully from raising threat this round" . gameTest $ \pid -> do
      setThreat pid 30
      card <- putCardIntoHand pid Events.doomHangsStill
      fund theodredCode 5
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget card.id)
      _ <- engagedEnemy pid Enemies.forestSpider
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      run QuestResolution
      threat pid `shouldReturn` 30

    it "with valour, raises each threat by 2 to skip the quest phase" . gameTest $ \pid -> do
      setThreat pid 40
      card <- putCardIntoHand pid Events.doomHangsStill
      fund theodredCode 5
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget card.id)
      chooseLabel "$label.valour" pid
      threat pid `shouldReturn` 42
      passActions pid
      run $ BeginPhase QuestPhase
      getQuestion pid >>= (`shouldSatisfy` isNothing)

  describe "Beechbone" do
    it "enters play exhausted and deals the damage on him to the enemy he attacks" . gameTest $ \pid -> do
      beechbone <- putAllyIntoPlay pid Allies.beechbone
      isExhausted beechbone `shouldReturn` True
      eid <- engagedEnemy pid Enemies.hillTroll
      run $ DealDamage GameSource beechbone 1
      run $ CheckWindows [mkAfter $ DeclaredAsAttacker beechbone eid]
      chooseAbilityOf pid beechbone
      getDamage beechbone `shouldReturn` 2
      getDamage (toTarget eid) `shouldReturn` 2

  describe "Hold Your Ground!" do
    it "readies a sentinel character" . tacticsTest $ \pid -> do
      guardian <- putAllyIntoPlay pid Allies.guardianOfArnor
      run $ Exhaust guardian
      card <- putCardIntoHand pid Events.holdYourGround
      fund legolasCode 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      isExhausted guardian `shouldReturn` False

  describe "Lindir" do
    it "draws until you have 3 cards in hand as he enters play" . gameTest $ \pid -> do
      lindir <- putAllyIntoPlay pid Allies.lindir
      handSize pid `shouldReturn` 0
      chooseAbilityOf pid lindir
      handSize pid `shouldReturn` 3

  describe "Lords of the Eldar" do
    it "goes from the discard pile to the bottom of the deck, giving Noldor +1/+1/+1" . gameTest $ \pid -> do
      lindir <- putAllyIntoPlay pid Allies.lindir
      chooseAbilityOf pid lindir
      card <- putInDiscard pid Events.lordsOfTheEldar
      fund eowynCode 3
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      stat getAttack lindir `shouldReturn` 2
      deck <- (.deck) <$> getPlayer pid
      map (.id) (lastMay deck) `shouldBe` Just card.id

  describe "The Long Defeat" do
    it "lets each player draw 2 cards once its quest card is defeated" . gameTest $ \pid -> do
      quest <- putQuestStage Quests.fliesAndSpiders
      _ <- putAttachmentOn pid Attachments.theLongDefeat quest
      was <- handSize pid
      run $ CompleteQuest (fromJustNote "quest" $ questIdOf quest)
      chooseAbilityOf pid =<< longDefeat
      chooseLabel "$label.theLongDefeat.draw" pid
      handSize pid `shouldReturn` was + 2
      attachmentOf Attachments.theLongDefeat `shouldReturn` Nothing

    it "or heals up to 5 damage among his characters" . gameTest $ \pid -> do
      quest <- putQuestStage Quests.fliesAndSpiders
      _ <- putAttachmentOn pid Attachments.theLongDefeat quest
      eowyn <- hero eowynCode
      run $ DealDamage GameSource eowyn 2
      run $ CompleteQuest (fromJustNote "quest" $ questIdOf quest)
      chooseAbilityOf pid =<< longDefeat
      chooseLabel "$label.theLongDefeat.heal" pid
      chooseTarget pid eowyn
      chooseTarget pid eowyn
      getDamage eowyn `shouldReturn` 0

  describe "Quick Ears" do
    it "shuffles an enemy just revealed back and reveals another card" . amarthiulTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad]
      card <- putCardIntoHand pid Events.quickEars
      fund beravorCode 1
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid =<< hero amarthiulCode
      (isExhausted =<< hero amarthiulCode) `shouldReturn` True
      -- the spider is shuffled back in with the road, and one of them is revealed
      revealed <- getsGame (\g -> length g.entities.enemies + length g.entities.locations)
      revealed `shouldBe` 1
      length <$> getEncounterDeck `shouldReturn` 1

  describe "Favor of the Valar" do
    it "keeps its player from elimination, lowering his threat to 5 below the level" . gameTest $ \pid -> do
      _ <- putAttachmentOn pid Attachments.favorOfTheValar (PlayerTarget pid)
      setThreat pid 48
      run $ RaiseThreat pid 4
      (.eliminated) <$> getPlayer pid `shouldReturn` False
      threat pid `shouldReturn` 45
      attachmentOf Attachments.favorOfTheValar `shouldReturn` Nothing

  describe "Éothain" do
    it "readies after a Rohan ally is discarded from play by a card effect" . gameTest $ \pid -> do
      eothain <- putAllyIntoPlay pid Allies.eothain
      run $ Exhaust eothain
      finest <- putAllyIntoPlay pid Allies.theRiddermarksFinest
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      run $ ActionWindow pid
      chooseAbilityOf pid finest
      chooseAbilityOf pid eothain
      isExhausted eothain `shouldReturn` False

  describe "Sword of Númenor" do
    it "adds a resource after its hero helps destroy an enemy with 5 or more hit points" . amarthiulTest $ \pid -> do
      amarthiul <- hero amarthiulCode
      sword <- putAttachmentOn pid Attachments.swordOfNumenor amarthiul
      stat getAttack amarthiul `shouldReturn` 4
      eid <- engagedEnemy pid Enemies.hillTroll
      run $ PlaceTokens GameSource (toTarget eid) Damage 8
      was <- getHeroResources . toId =<< getHeroWithCode amarthiulCode
      run $ PlayerAttacks pid
      chooseTarget pid amarthiul
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      chooseAbilityOf pid sword
      now <- getHeroResources . toId =<< getHeroWithCode amarthiulCode
      now - was `shouldBe` 1

    it "is not offered for an enemy with fewer printed hit points" . amarthiulTest $ \pid -> do
      amarthiul <- hero amarthiulCode
      sword <- putAttachmentOn pid Attachments.swordOfNumenor amarthiul
      _ <- engagedEnemy pid Enemies.dolGuldurOrcs
      run $ PlayerAttacks pid
      chooseTarget pid amarthiul
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      offersAbilityOf pid sword `shouldReturn` False

  describe "Hour of Wrath" do
    it "keeps the chosen hero from exhausting to attack this phase" . tacticsTest $ \pid -> do
      card <- putCardIntoHand pid Events.hourOfWrath
      eowyn <- hero eowynCode
      fund legolasCode 4
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid eowyn
      getModifiers eowyn >>= (`shouldSatisfy` elem DoesNotExhaustToAttack)

  describe "Tale of Tinúviel" do
    it "exhausts a Noldor to ready a Dúnedain, adding its printed willpower" . amarthiulTest $ \pid -> do
      amarthiul <- hero amarthiulCode
      run $ Exhaust amarthiul
      lindir <- putAllyIntoPlay pid Allies.lindir
      chooseAbilityOf pid lindir
      card <- putCardIntoHand pid Events.taleOfTinuviel
      fund eowynCode 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid lindir
      chooseTarget pid amarthiul
      isExhausted lindir `shouldReturn` True
      isExhausted amarthiul `shouldReturn` False
      stat getAttack amarthiul `shouldReturn` 5

  describe "Galadhrim Healer" do
    it "heals 1 damage from each hero of the chosen player" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      beravor <- hero beravorCode
      run $ DealDamage GameSource eowyn 2
      run $ DealDamage GameSource beravor 1
      healer <- putAllyIntoPlay pid Allies.galadhrimHealer
      chooseAbilityOf pid healer
      getDamage eowyn `shouldReturn` 1
      getDamage beravor `shouldReturn` 0

  describe "Weather-stained Cloak" do
    it "cancels 1 damage just dealt to its committed Ranger" . amarthiulTest $ \pid -> do
      amarthiul <- hero amarthiulCode
      cloak <- putAttachmentOn pid Attachments.weatherStainedCloak amarthiul
      run $ CommitToQuest amarthiul
      run $ DealDamage GameSource amarthiul 2
      chooseAbilityOf pid cloak
      getDamage amarthiul `shouldReturn` 1

  describe "Sword-thain" do
    it "makes a unique ally a hero, and an ally again once it leaves" . gameTest $ \pid -> do
      lindir <- putAllyIntoPlay pid Allies.lindir
      chooseAbilityOf pid lindir
      run $ DealDamage GameSource lindir 1
      card <- genPlayerCard Attachments.swordThain (Just pid)
      run $ PlayAttachmentOn pid card lindir
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      asHero <- getHeroWithCode (toCardCode Allies.lindir)
      getDamage (toTarget asHero) `shouldReturn` 1
      attachmentOf Attachments.swordThain `shouldReturn` Just (AttachedTo $ toTarget asHero)
      run $ BeginPhase ResourcePhase
      passActions pid
      getHeroResources asHero.id `shouldReturn` 1
      sword <- getsGame (toTarget . fromJustNote "sword" . find ((== toCardCode Attachments.swordThain) . toCardCode) . toList . (.entities.attachments))
      run $ Discard sword
      allies <- getsGame (toList . (.entities.allies))
      map toCardCode allies `shouldBe` [toCardCode Allies.lindir]

    it "keeps the ally's action ability, exhausting the hero to pay for it" . gameTest $ \pid -> do
      faramir <- swordThained pid Allies.faramir
      run $ ActionWindow pid
      chooseAbilityOf pid faramir
      exhausted faramir `shouldReturn` True
      eowyn <- hero eowynCode
      getModifiers eowyn >>= (`shouldSatisfy` elem (WillpowerModifier 1))

    it "keeps the ally's constant modifiers, given to the hero" . gameTest $ \pid -> do
      ingold <- swordThained pid Allies.ingold
      was <- stat getWillpower ingold
      run $ PlaceTokens GameSource ingold Resource 1
      stat getWillpower ingold `shouldReturn` (was + 1)

    it "keeps the ally's responses" . gameTest $ \pid -> do
      sam <- swordThained pid Allies.samGamgee
      run $ Exhaust sam
      fund eowynCode 1
      run $ RaiseThreat pid 1
      chooseAbilityOf pid sam
      exhausted sam `shouldReturn` False
      getModifiers sam >>= (`shouldSatisfy` elem (AttackModifier 1))

    it "is a hero its controller controls, and not an ally" . gameTest $ \pid -> do
      faramir <- swordThained pid Allies.faramir
      map toTarget <$> select (HeroControlledBy $ PlayerWithId pid) >>= (`shouldSatisfy` elem faramir)
      select (AllyControlledBy $ PlayerWithId pid) `shouldReturn` []

    it "keeps another copy of it from being played, as unique" . gameTest $ \pid -> do
      _ <- swordThained pid Allies.faramir
      fund theodredCode 4
      card <- putCardIntoHand pid Allies.faramir
      run $ BeginPhase PlanningPhase
      Just question <- getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldSatisfy` notElem (CardIdTarget card.id)

    it "goes to its owner's discard pile as it leaves play" . gameTest $ \pid -> do
      faramir <- swordThained pid Allies.faramir
      run $ Discard faramir
      getsGame (length . (.entities.heroes)) `shouldReturn` 3
      map toCardCode . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.faramir))

    it "keeps Beechbone's response, reading the damage on the hero" . gameTest $ \pid -> do
      beechbone <- swordThained pid Allies.beechbone
      eid <- engagedEnemy pid Enemies.hillTroll
      run $ DealDamage GameSource beechbone 1
      run $ CheckWindows [mkAfter $ DeclaredAsAttacker beechbone eid]
      chooseAbilityOf pid beechbone
      getDamage beechbone `shouldReturn` 2
      getDamage (toTarget eid) `shouldReturn` 2

    it "keeps Éothain's response while the hero is exhausted, as its text asks of it by its id" . gameTest $ \pid -> do
      eothain <- swordThained pid Allies.eothain
      finest <- putAllyIntoPlay pid Allies.theRiddermarksFinest
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      run $ Exhaust eothain
      run $ ActionWindow pid
      chooseAbilityOf pid finest
      chooseAbilityOf pid eothain
      exhausted eothain `shouldReturn` False

    it "keeps Riddermark Knight's response, discarding the hero after its attack" . gameTest $ \pid -> do
      spider <- engagedEnemy pid Enemies.forestSpider
      AllyTarget knight <- putAllyIntoPlay pid Allies.riddermarkKnight
      -- the knight is not unique, so made a hero as Sword-thain makes one
      run $ AllyBecomesHero knight
      asHero <- toTarget <$> getHeroWithCode (toCardCode Allies.riddermarkKnight)
      run $ PlayerAttacks pid
      chooseTarget pid asHero
      chooseAbilityOf pid asHero
      chooseLabel "$label.doneAttacking" pid
      passResponses pid
      passActions pid
      getDamage (toTarget spider) `shouldReturn` 3
      getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` notElem (toCardCode Allies.riddermarkKnight))
 where
  -- the unique ally put into play, then made a hero by Sword-thain
  swordThained pid def = do
    ally' <- putAllyIntoPlay pid def
    card <- genPlayerCard Attachments.swordThain (Just pid)
    run $ PlayAttachmentOn pid card ally'
    toTarget <$> getHeroWithCode (toCardCode def)
  passResponses pid =
    getQuestion pid >>= \case
      Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
      _ -> pure ()
  exhausted t = getsGame (maybe False (heroExhausted . toAttrs) . find ((== t) . toTarget) . toList . (.entities.heroes))
  longDefeat = getsGame (toTarget . fromJustNote "The Long Defeat" . find ((== toCardCode Attachments.theLongDefeat) . toCardCode) . toList . (.entities.attachments))
  questIdOf = \case
    QuestTarget qid -> Just qid
    _ -> Nothing
