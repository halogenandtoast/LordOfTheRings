module LordOfTheRings.Core.AlliesAndAttachmentsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import TestImport

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

-- | A location from the encounter deck, revealed into the staging area.
stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  _ <- setEncounterDeck [def]
  run RevealEncounterCard
  getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

spec :: Spec
spec = do
  describe "Daughter of the Nimrodel" do
    it "exhausts to heal up to 2 damage on a hero" . gameTest $ \pid -> do
      daughter <- putAllyIntoPlay pid Allies.daughterOfTheNimrodel
      beravor <- toTarget <$> getHeroWithCode "01012"
      run $ PlaceTokens GameSource beravor Damage 3
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid daughter
      chooseTarget pid beravor
      getCharacterDamage beravor `shouldReturn` 1

  describe "Gléowine" do
    it "exhausts to have a player draw a card" . gameTest $ \pid -> do
      gleowine <- putAllyIntoPlay pid Allies.gleowine
      before <- handSize pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid gleowine
      handSize pid `shouldReturn` before + 1

  describe "Henamarth Riversong" do
    it "exhausts to look at the top card of the encounter deck" . gameTest $ \pid -> do
      henamarth <- putAllyIntoPlay pid Allies.henamarthRiversong
      deck <- setEncounterDeck [Locations.forestGate, Locations.oldForestRoad]
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid henamarth
      Just question <- getQuestion pid
      [() | Label "Move it to the bottom" _ <- questionChoices question] `shouldBe` []
      chooseLabel "Continue" pid
      getEncounterDeck `shouldReturn` deck

  describe "Erebor Hammersmith" do
    it "returns the topmost attachment in a discard pile to its owner's hand" . gameTest $ \pid -> do
      plate <- PlayerCard <$> genPlayerCard Attachments.citadelPlate (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . discardL .~ [plate]
      hammersmith <- putAllyIntoPlay pid Allies.ereborHammersmith
      chooseAbilityOf pid hammersmith
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem plate.id)

  describe "Miner of the Iron Hills" do
    it "discards a Condition attachment from play" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      favor <- putAttachmentOn pid Attachments.theFavorOfTheLady eowyn
      miner <- putAllyIntoPlay pid Allies.minerOfTheIronHills
      chooseAbilityOf pid miner
      chooseTarget pid favor
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

  describe "Lórien Guide" do
    it "places 1 progress on the active location after committing" . gameTest $ \pid -> do
      guide <- putAllyIntoPlay pid Allies.lorienGuide
      lid <- stageLocation Locations.forestGate
      run $ TravelTo lid
      passActions pid
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ BeginPhase QuestPhase
      chooseTarget pid guide
      chooseAbilityOf pid guide
      (.progress) <$> getLocation lid `shouldReturn` 1

  describe "Northern Tracker" do
    it "places 1 progress on each location in the staging area after committing" . gameTest $ \pid -> do
      tracker <- putAllyIntoPlay pid Allies.northernTracker
      lid <- stageLocation Locations.forestGate
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ BeginPhase QuestPhase
      chooseTarget pid tracker
      chooseAbilityOf pid tracker
      (.progress) <$> getLocation lid `shouldReturn` 1

  describe "Beorn" do
    it "gains +5 attack and is shuffled into the deck at the end of the phase" . gameTest $ \pid -> do
      beorn <- putAllyIntoPlay pid Allies.beorn
      deck <- length . (.deck) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid beorn
      getAttack beorn `shouldReturn` 8
      passActions pid
      run $ EndPhase PlanningPhase
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      length . (.deck) <$> getPlayer pid `shouldReturn` deck + 1

  describe "Wandering Took" do
    it "has no one to go to in a one player game" . gameTest $ \pid -> do
      took <- putAllyIntoPlay pid Allies.wanderingTook
      run $ BeginPhase PlanningPhase
      offersAbilityOf pid took `shouldReturn` False

  describe "The Favor of the Lady" do
    it "gives the attached hero +1 willpower" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      _ <- putAttachmentOn pid Attachments.theFavorOfTheLady eowyn
      getWillpower eowyn `shouldReturn` 5

  describe "Power in the Earth" do
    it "gives the attached location -1 threat" . gameTest $ \pid -> do
      lid <- stageLocation Locations.forestGate
      _ <- putAttachmentOn pid Attachments.powerInTheEarth (toTarget lid)
      getLocationThreat lid `shouldReturn` 1

  describe "Citadel Plate" do
    it "gives the attached hero +4 hit points" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      _ <- putAttachmentOn pid Attachments.citadelPlate eowyn
      getHitPoints eowyn `shouldReturn` 7

  describe "Dwarven Axe" do
    it "gives +2 attack to a Dwarf and +1 to anyone else" . gameTestWith (coreDeck "tactics") $ \pid -> do
      gimli <- toTarget <$> getHeroWithCode "01004"
      legolas <- toTarget <$> getHeroWithCode "01005"
      _ <- putAttachmentOn pid Attachments.dwarvenAxe gimli
      _ <- putAttachmentOn pid Attachments.dwarvenAxe legolas
      getAttack gimli `shouldReturn` 4
      getAttack legolas `shouldReturn` 4

  describe "Unexpected Courage" do
    it "exhausts to ready the attached hero" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      courage <- putAttachmentOn pid Attachments.unexpectedCourage eowyn
      run $ Exhaust eowyn
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid courage
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False

  describe "Self Preservation" do
    it "exhausts to heal 2 damage on the attached character" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      preservation <- putAttachmentOn pid Attachments.selfPreservation eowyn
      run $ PlaceTokens GameSource eowyn Damage 2
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid preservation
      getCharacterDamage eowyn `shouldReturn` 0

  describe "Protector of Lórien" do
    it "discards a card for +1 defense until the end of the phase" . gameTest $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      protector <- putAttachmentOn pid Attachments.protectorOfLorien eowyn
      card <- putCardIntoHand pid Events.grimResolve
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid protector
      chooseTarget pid (CardIdTarget card.id)
      chooseLabel "+1 defense" pid
      getDefense eowyn `shouldReturn` 2
