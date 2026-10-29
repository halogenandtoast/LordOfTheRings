module LordOfTheRings.RingMaker.PlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (UndefendedDamageMayGoTo))
import LordOfTheRings.I18n (ikeyWith, intVar)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.Window (WindowType (DealtDamage, ThreatRaisedByEncounter), mkAfter)
import TestImport

beravorCode, eowynCode, theodredCode, idraenCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
idraenCode = "08025"
theodredCode = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

-- | Idraen (spirit, Ranger, Scout), Beravor (lore) and Théodred (leadership).
idraenTest :: (PlayerId -> GameT ()) -> IO ()
idraenTest = gameTestWith (DeckList [idraenCode, beravorCode, theodredCode] testDeck.cards)

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

inHand :: PlayerId -> CardId -> GameT Bool
inHand pid cid = elem cid . map (.id) . (.hand) <$> getPlayer pid

allyWithCard :: CardId -> GameT Target
allyWithCard cid = getsGame (toTarget . fromJustNote "ally did not enter play" . find ((== cid) . allyCardId . toAttrs) . toList . (.entities.allies))

locationProgress :: GameT [Int]
locationProgress = getsGame (map (countTokens Progress . locationTokens . toAttrs) . toList . (.entities.locations))

allyController :: Target -> GameT (Maybe Placement)
allyController = \case
  AllyTarget aid -> fmap (allyPlacement . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure Nothing

putInDiscard :: PlayerId -> CardDef -> GameT Card
putInDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . discardL %~ (card :)
  pure card

erkenbrandCode :: CardCode
erkenbrandCode = "08137"

-- | Erkenbrand (leadership, sentinel), Beravor (lore) and Éowyn (spirit).
erkenbrandTest :: (PlayerId -> GameT ()) -> IO ()
erkenbrandTest = gameTestWith (DeckList [erkenbrandCode, beravorCode, eowynCode] testDeck.cards)

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

spec :: Spec
spec = describe "The Ring-maker cycle player cards" do
  describe "Swift and Silent" do
    it "returns to hand the first time it is played in a round, while threat is 20 or less" . gameTest $ \pid -> do
      setThreat pid 20
      beravor <- hero beravorCode
      run $ Exhaust beravor
      card <- playWith pid theodredCode 2 Events.swiftAndSilent
      chooseTarget pid beravor
      isExhausted beravor `shouldReturn` False
      inHand pid card.id `shouldReturn` True
      run $ Exhaust beravor
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid beravor
      inHand pid card.id `shouldReturn` False

    it "is discarded while threat is above 20" . gameTest $ \pid -> do
      setThreat pid 21
      card <- playWith pid theodredCode 1 Events.swiftAndSilent
      chooseTarget pid =<< hero beravorCode
      inHand pid card.id `shouldReturn` False

  describe "Booming Ent" do
    it "enters play exhausted and gets +1 attack for each damaged Ent you control" . gameTest $ \pid -> do
      booming <- putAllyIntoPlay pid Allies.boomingEnt
      isExhausted booming `shouldReturn` True
      wandering <- putAllyIntoPlay pid Allies.wanderingEnt
      getAttack booming `shouldReturn` 2
      run $ DealDamage GameSource wandering 1
      run $ DealDamage GameSource booming 1
      getAttack booming `shouldReturn` 4

  describe "Cloak of Lórien" do
    it "gives +2 defense while the active location is a Forest" . gameTest $ \pid -> do
      archer <- putAllyIntoPlay pid Allies.galadhonArcher
      _ <- putAttachmentOn pid Attachments.cloakOfLorien archer
      getDefense archer `shouldReturn` 1
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.oldForestRoad
      getDefense archer `shouldReturn` 2

  describe "Island Amid Perils" do
    it "returns a Silvan ally to hand to reduce threat by its printed cost" . gameTest $ \pid -> do
      setThreat pid 30
      _ <- putAllyIntoPlay pid Allies.galadhonArcher
      _ <- playWith pid eowynCode 0 Events.islandAmidPerils
      threat pid `shouldReturn` 28
      getsGame (length . (.entities.allies)) `shouldReturn` 0

  describe "Free to Choose" do
    it "reduces threat by what an encounter card raised it" . gameTest $ \pid -> do
      setThreat pid 30
      card <- putCardIntoHand pid Events.freeToChoose
      run $ RaiseThreat pid 3
      run $ CheckWindows [mkAfter $ ThreatRaisedByEncounter pid 3]
      chooseTarget pid (CardIdTarget card.id)
      threat pid `shouldReturn` 30

  describe "Idraen" do
    it "readies after a location is explored" . idraenTest $ \pid -> do
      idraen <- hero idraenCode
      run $ Exhaust idraen
      road <- genEncounterCard Locations.oldForestRoad
      run $ AddToStaging road
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      chooseAbilityOf pid idraen
      isExhausted idraen `shouldReturn` False

  describe "Ithilien Lookout" do
    it "may discard the top card of the encounter deck when it is an enemy" . gameTest $ \pid -> do
      [bats] <- setEncounterDeck [Enemies.blackForestBats]
      lookout <- putAllyIntoPlay pid Allies.ithilienLookout
      chooseAbilityOf pid lookout
      chooseLabel "$label.discardIt" pid
      map (.id) <$> getEncounterDiscard `shouldReturn` [bats.id]

  describe "Follow Me!" do
    it "takes the first player token and draws a card" . twoPlayerTest testDeck $ \one two -> do
      card <- putCardIntoHand two Events.followMe
      t <- heroOf two theodredCode
      run $ PlaceTokens GameSource t Resource 1
      run $ BeginPhase PlanningPhase
      passActions one
      hand <- length . (.hand) <$> getPlayer two
      chooseTarget two (CardIdTarget card.id)
      getLead `shouldReturn` two
      length . (.hand) <$> getPlayer two `shouldReturn` hand

  describe "Greyflood Wanderer" do
    it "given doomed 2, places 1 progress on each location in play after it is played" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      was <- threat pid
      card <- playWith pid eowynCode 3 Allies.greyfloodWanderer
      chooseLabel "$label.giveItDoomed" pid
      threat pid `shouldReturn` was + 2
      chooseAbilityOf pid =<< allyWithCard card.id
      locationProgress `shouldReturn` [1]

    it "not given doomed, gains nothing" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      was <- threat pid
      card <- playWith pid eowynCode 3 Allies.greyfloodWanderer
      chooseLabel "$label.doNotGiveDoomed" pid
      threat pid `shouldReturn` was
      wanderer <- allyWithCard card.id
      (`shouldBe` False) =<< offersAbilityOf pid wanderer

    it "loses the doomed it was given as it is played again" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      card <- playWith pid eowynCode 3 Allies.greyfloodWanderer
      chooseLabel "$label.giveItDoomed" pid
      chooseAbilityOf pid =<< allyWithCard card.id
      -- back to hand and played again from the planning window
      payer <- getHeroWithCode eowynCode
      wanderer <- allyWithCard card.id
      void $ runDebug $ Run [ReturnToHand wanderer, PlaceTokens GameSource (toTarget payer) Resource 3]
      chooseTarget pid (CardIdTarget card.id)
      chooseLabel "$label.doNotGiveDoomed" pid
      (`shouldBe` False) =<< offersAbilityOf pid =<< allyWithCard card.id

  describe "Herald of Anórien" do
    it "given doomed 2, lets a player put a cheap ally from hand into play" . gameTest $ \pid -> do
      cheap <- putCardIntoHand pid Allies.galadhonArcher
      card <- playWith pid theodredCode 2 Allies.heraldOfAnorien
      chooseLabel "$label.giveItDoomed" pid
      chooseAbilityOf pid =<< allyWithCard card.id
      chooseTarget pid (CardIdTarget cheap.id)
      getsGame (length . (.entities.allies)) `shouldReturn` 2

  describe "Leaf Brooch" do
    it "lowers the first event of its hero's sphere each round by 1 while threat is 20 or less" . gameTest $ \pid -> do
      setThreat pid 20
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.leafBrooch eowyn
      card <- playWith pid eowynCode 0 Events.courageAwakened
      chooseTarget pid eowyn
      -- returned to hand, it now costs its full 1
      inHand pid card.id `shouldReturn` True
      Just q <- getQuestion pid
      [t | TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (CardIdTarget card.id)

  describe "Close Call" do
    -- Gimli gives the 0 cost Tactics event its resource match
    it "cancels X damage just dealt to a hero, raising each threat by X" . gameTestWith testDeck {heroes = [beravorCode, "01004", theodredCode]} $ \pid -> do
      beravor <- hero beravorCode
      card <- putCardIntoHand pid Events.closeCall
      run $ PlaceTokens GameSource beravor Damage 2
      was <- threat pid
      run $ CheckWindows [mkAfter $ DealtDamage beravor 2]
      chooseTarget pid (CardIdTarget card.id)
      chooseLabel (ikeyWith "label.xEquals" [intVar "x" 2]) pid
      getCharacterDamage beravor `shouldReturn` 0
      threat pid `shouldReturn` was + 2

  describe "Blue Mountain Trader" do
    it "goes to another player, who moves a resource to one of your heroes" . twoPlayerTest testDeck $ \one two -> do
      trader <- putAllyIntoPlay one Allies.blueMountainTrader
      theirs <- heroOf two theodredCode
      run $ PlaceTokens GameSource theirs Resource 1
      mine <- heroOf one beravorCode
      run $ BeginPhase PlanningPhase
      -- the players' planning windows come in no set order
      passActions two
      chooseAbilityOf one trader
      chooseTarget two theirs
      chooseTarget two mine
      allyController trader `shouldReturn` Just (InPlayArea two)
      case mine of
        HeroTarget h -> getHeroResources h `shouldReturn` 1
        _ -> error "not a hero"

  describe "Defender of the West" do
    it "gives the first player control of the ally, and its player may give it undefended damage" . twoPlayerTest testDeck $ \one two -> do
      henamarth <- putAllyIntoPlay two Allies.henamarthRiversong
      defender <- putAttachmentOn two Attachments.defenderOfTheWest henamarth
      case defender of
        AttachmentTarget aid -> run $ AttachCard aid henamarth
        _ -> error "not an attachment"
      allyController henamarth `shouldReturn` Just (InPlayArea one)
      getModifiers (PlayerTarget two) >>= (`shouldSatisfy` elem (UndefendedDamageMayGoTo henamarth))
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem (UndefendedDamageMayGoTo henamarth))

  describe "The Fall of Gil-Galad" do
    it "goes to the victory display as its hero is destroyed, reducing threat by its threat cost" . gameTest $ \pid -> do
      setThreat pid 30
      beravor <- hero beravorCode
      fall <- putAttachmentOn pid Attachments.theFallOfGilGalad beravor
      run $ DealDamage GameSource beravor 4
      chooseAbilityOf pid fall
      threat pid `shouldReturn` 20
      getsGame (length . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` 1

  describe "The White Council" do
    it "costs 1 a player, and the player chooses an option" . gameTest $ \pid -> do
      handSize <- length . (.hand) <$> getPlayer pid
      _ <- playWith pid theodredCode 1 Events.theWhiteCouncil
      chooseLabel "$label.theWhiteCouncil.draw" pid
      length . (.hand) <$> getPlayer pid `shouldReturn` handSize + 1
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 0

  describe "Tighten Our Belts" do
    it "gives a resource to each hero that spent none this round" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      run $ PlaceTokens GameSource beravor Resource 1
      run $ RemoveTokens (PlayerSource pid) beravor Resource 1
      card <- putCardIntoHand pid Events.tightenOurBelts
      putGame . (phaseL .~ RefreshPhase) =<< getGame
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      (getHeroResources . toId =<< getHeroWithCode beravorCode) `shouldReturn` 0
      (getHeroResources . toId =<< getHeroWithCode eowynCode) `shouldReturn` 1
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 1

  describe "Warden of Arnor" do
    it "places 1 progress on the first location revealed each round while its hero is committed" . idraenTest $ \pid -> do
      idraen <- hero idraenCode
      _ <- putAttachmentOn pid Attachments.wardenOfArnor idraen
      run $ CommitToQuest idraen
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      locationProgress `shouldReturn` [1]
      run RevealEncounterCard
      sum <$> locationProgress `shouldReturn` 1

    it "explores Tharbad Hideout as it is revealed, so its surge does not resolve (FAQ 3.188)" . idraenTest $ \pid -> do
      idraen <- hero idraenCode
      _ <- putAttachmentOn pid Attachments.wardenOfArnor idraen
      run $ CommitToQuest idraen
      _ <- setEncounterDeck [Locations.tharbadHideout, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      locationProgress `shouldReturn` []
      length <$> getEncounterDeck `shouldReturn` 1

    it "places its progress on Streets of Tharbad before it reaches the staging area (FAQ 3.188)" . idraenTest $ \pid -> do
      idraen <- hero idraenCode
      _ <- putAttachmentOn pid Attachments.wardenOfArnor idraen
      run $ CommitToQuest idraen
      _ <- setEncounterDeck [Locations.streetsOfTharbad]
      run RevealEncounterCard
      resolveRevealed
      locationProgress `shouldReturn` []

  describe "Gwaihir" do
    it "puts an Eagle from the discard pile into play, back to hand at the end of the round" . gameTest $ \pid -> do
      eagle <- putInDiscard pid Allies.wingedGuardian
      gwaihir <- putAllyIntoPlay pid Allies.gwaihir
      chooseAbilityOf pid gwaihir
      getsGame (length . (.entities.allies)) `shouldReturn` 2
      run EndRound
      inHand pid eagle.id `shouldReturn` True
      getsGame (length . (.entities.allies)) `shouldReturn` 1

  describe "Message from Elrond" do
    it "passes a card to another player's hand, shuffled into its owner's deck at the end of the round" . twoPlayerTest testDeck $ \one two -> do
      gift <- putCardIntoHand one Events.closeCall
      card <- putCardIntoHand one Events.messageFromElrond
      run $ PlayCard one card.id
      chooseTarget one one
      chooseTarget one (CardIdTarget gift.id)
      inHand two gift.id `shouldReturn` True
      inHand one gift.id `shouldReturn` False
      run EndRound
      inHand two gift.id `shouldReturn` False
      elem gift.id . map (.id) . (.deck) <$> getPlayer one `shouldReturn` True

  describe "Erkenbrand" do
    it "takes 1 damage to cancel a shadow effect while he defends" . erkenbrandTest $ \pid -> do
      erk <- hero erkenbrandCode
      eid <- engagedEnemy pid Enemies.blackForestBats
      _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
      run $ DealShadowCard eid
      run $ DefendWith pid eid erk
      passActions pid
      chooseAbilityOf pid erk
      passActions pid
      -- the bats' 1 attack, not raised by the cancelled shadow, against his 3 defense
      getCharacterDamage erk `shouldReturn` 1

  describe "The Day's Rising" do
    it "adds a resource after its hero defends an attack without taking damage" . erkenbrandTest $ \pid -> do
      erk <- hero erkenbrandCode
      rising <- putAttachmentOn pid Attachments.theDaysRising erk
      eid <- engagedEnemy pid Enemies.blackForestBats
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ DefendWith pid eid erk
      passActions pid
      passActions pid
      chooseAbilityOf pid rising
      (getHeroResources . toId =<< getHeroWithCode erkenbrandCode) `shouldReturn` 1

  describe "Ride Them Down" do
    it "deals the progress questing would place on the quest to the chosen enemy" . gameTest $ \pid -> do
      card <- genEncounterCard Enemies.forestSpider
      run $ AddToStaging card
      eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
      ride <- putCardIntoHand pid Events.rideThemDown
      hero eowynCode >>= \e -> run (PlaceTokens GameSource e Resource 2)
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget ride.id)
      passActions pid
      run $ PlaceProgressOnQuest 2
      getCharacterDamage (toTarget eid) `shouldReturn` 2

  describe "Don't Be Hasty!" do
    it "readies a committed character and removes it from the quest as a card is revealed" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      run $ CommitToQuest beravor
      card <- putCardIntoHand pid Events.dontBeHasty
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget card.id)
      isExhausted beravor `shouldReturn` False
      heroCommitted . toAttrs <$> getHeroWithCode beravorCode `shouldReturn` False
