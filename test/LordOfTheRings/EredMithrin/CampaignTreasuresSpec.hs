module LordOfTheRings.EredMithrin.CampaignTreasuresSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (TreasureType), cdCardType, isTreasure)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (PlanningPhase))
import TestImport

beravorCode, eowynCode, theodredCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

attachmentOf :: Card -> GameT Target
attachmentOf card = getsGame (toTarget . fromJustNote "no attachment" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.attachments))

hostOf :: Card -> GameT (Maybe Placement)
hostOf card = getsGame (fmap (.placement) . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.attachments))

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  existing <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  eid <- getsGame (fromJustNote "no enemy" . find (`notElem` existing) . map toId . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "Ered Mithrin treasures" do
  it "are their own card type, played from hand onto a character as an attachment is" . gameTest $ \pid -> do
    cdCardType Attachments.jewelsOfWilderland `shouldBe` TreasureType
    beravor <- hero beravorCode
    card <- playWith pid theodredCode 0 Attachments.jewelsOfWilderland
    chooseTarget pid beravor
    hostOf card `shouldReturn` Just (AttachedTo beravor)
    -- +1 willpower to Beravor's 2
    getWillpower beravor `shouldReturn` 3

  it "are treasures, the card type riddles and Expert Treasure-hunter name"
    $ (isTreasure Attachments.masterworkBow `shouldBe` True :: IO ())

  describe "Durin's Dagger" do
    it "may be played from its owner's discard pile" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      card <- PlayerCard <$> genPlayerCard Attachments.durinsDagger (Just pid)
      putGame . (playersL . ix pid . discardL .~ [card]) =<< getGame
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource theodred Resource 1
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid beravor
      hostOf card `shouldReturn` Just (AttachedTo beravor)
      getAttack beravor `shouldReturn` 3

  describe "Axe of the Edain" do
    it "deals 2 damage to a non-unique enemy engaged with its player as it is played" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      eid <- engagedEnemy pid Enemies.kingSpider
      card <- playWith pid theodredCode 0 Attachments.axeOfTheEdain
      chooseTarget pid beravor
      chooseAbilityOf pid =<< attachmentOf card
      getDamage (toTarget eid) `shouldReturn` 2
      getAttack beravor `shouldReturn` 4

  describe "Glittering Lute" do
    it "lowers by 2 the threat of a player who discards a card as it is played" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      card <- playWith pid theodredCode 0 Attachments.glitteringLute
      chooseTarget pid eowyn
      other <- putCardIntoHand pid Allies.gondorianSpearman
      putGame . (playersL . ix pid . threatL .~ 30) =<< getGame
      was <- threat pid
      chooseAbilityOf pid =<< attachmentOf card
      chooseTarget pid (CardIdTarget other.id)
      threat pid `shouldReturn` was - 2
      getWillpower eowyn `shouldReturn` 6

  describe "Helm of Heroes" do
    it "adds a resource to a hero's pool after its character takes damage from an attack" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      theodred <- hero theodredCode
      helm <- putAttachmentOn pid Attachments.helmOfHeroes beravor
      getDefense beravor `shouldReturn` 4
      eid <- engagedEnemy pid Enemies.hillTroll
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid beravor
      passActions pid
      chooseAbilityOf pid helm
      chooseTarget pid theodred
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 1

  describe "Jewels of Wilderland" do
    it "exhausts and discards a card from hand to draw a card" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      jewels <- putAttachmentOn pid Attachments.jewelsOfWilderland beravor
      spare <- putCardIntoHand pid Allies.gondorianSpearman
      handSize <- length . (.hand) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid jewels
      chooseTarget pid (CardIdTarget spare.id)
      length . (.hand) <$> getPlayer pid `shouldReturn` handSize
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [spare.id]

  describe "Masterwork Bow" do
    it "gives ranged, and places a progress on a staged location as its character destroys an enemy" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      bow <- putAttachmentOn pid Attachments.masterworkBow beravor
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      eid <- engagedEnemy pid Enemies.forestSpider
      run $ PlaceTokens GameSource (toTarget eid) Damage 3
      run $ PlayerAttacks pid
      chooseTarget pid beravor
      chooseLabel "$label.doneAttacking" pid
      chooseAbilityOf pid bow
      (.progress) <$> getLocation lid `shouldReturn` 1

  describe "Dwarf-Lord's Crown" do
    it "boosts each ally sharing a trait with its hero until the end of the phase" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      crown <- putAttachmentOn pid Attachments.dwarfLordsCrown theodred
      -- Théodred is Noble, Rohan, Warrior: the Spearman is a Gondor Warrior
      spearman <- putAllyIntoPlay pid Allies.gondorianSpearman
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid crown
      getAttack spearman `shouldReturn` 2
      getDefense spearman `shouldReturn` 2

  describe "Anvil of Gundabad" do
    it "searches the top 5 cards for any number of Item attachments as it is claimed" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      plate <- PlayerCard <$> genPlayerCard Attachments.citadelPlate (Just pid)
      blade <- PlayerCard <$> genPlayerCard Attachments.bladeOfGondolin (Just pid)
      putGame . (playersL . ix pid . deckL .~ [plate, blade]) =<< getGame
      anvil <- genPlayerCard Attachments.anvilOfGundabad (Just pid)
      run $ AttachFreedCard pid anvil
      chooseTarget pid beravor
      chooseAbilityOf pid =<< attachmentOf (PlayerCard anvil)
      chooseTarget pid (CardIdTarget plate.id)
      chooseTarget pid (CardIdTarget blade.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` \hand -> plate.id `elem` hand && blade.id `elem` hand)

    it "plays only an Item attachment from hand that has a legal host, for 1 less" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      anvil <- putAttachmentOn pid Attachments.anvilOfGundabad beravor
      -- Boots from Erebor would go on a Dwarf or Hobbit, and none is in play; Healing
      -- Herbs goes on a Lore hero, as Beravor is
      _ <- putCardIntoHand pid Attachments.bootsFromErebor
      herbs <- putCardIntoHand pid Attachments.healingHerbs
      run $ ActionWindow pid
      chooseAbilityOf pid anvil
      question <- fromJustNote "no question" <$> getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldBe` [CardIdTarget herbs.id]

    it "offers no action with only Item attachments that have no legal host" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      anvil <- putAttachmentOn pid Attachments.anvilOfGundabad beravor
      _ <- putCardIntoHand pid Attachments.bootsFromErebor
      run $ ActionWindow pid
      offersAbilityOf pid anvil `shouldReturn` False

  describe "Gondolin Shield" do
    it "exhausts and spends a resource to cancel a shadow effect against its defender, dealing another" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      shield <- putAttachmentOn pid Attachments.gondolinShield beravor
      run $ PlaceTokens GameSource beravor Resource 1
      eid <- engagedEnemy pid Enemies.blackForestBats
      _ <- setEncounterDeck [Enemies.dolGuldurOrcs, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ DefendWith pid eid beravor
      passActions pid
      chooseAbilityOf pid shield
      -- the other card was dealt from the encounter deck
      length <$> getEncounterDeck `shouldReturn` 0
      (getHeroResources . toId =<< getHeroWithCode beravorCode) `shouldReturn` 0
