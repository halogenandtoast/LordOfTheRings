module LordOfTheRings.RingMaker.PlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.Window (WindowType (ThreatRaisedByEncounter), mkAfter)
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
