module LordOfTheRings.AgainstTheShadow.PlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Modifier (ModifierType (CannotAttack, CostModifier, DiscardNextRevealed, NoEngagementChecks))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import TestImport

-- | Heroes, by code, with the test deck's cards.
withHeroes :: [CardCode] -> (PlayerId -> GameT ()) -> IO ()
withHeroes heroes = gameTestWith (DeckList heroes testDeck.cards)

hirluin, theoden, gimli, legolas, pippin, frodo, bilbo, eowyn, caldara, beravor :: CardCode
hirluin = "06001"
theoden = "06134"
gimli = "01004"
legolas = "01005"
pippin = "06056"
frodo = "02025"
bilbo = "02001"
eowyn = "01007"
caldara = "06107"
beravor = "01012"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies))
    <&> toId

enemyPlacement' :: EnemyId -> GameT Placement
enemyPlacement' eid = getsGame (maybe Unplaced (enemyPlacement . toAttrs) . lookup eid . mapToList . (.entities.enemies))

allyCount :: GameT Int
allyCount = getsGame (length . (.entities.allies))

attachmentCount :: GameT Int
attachmentCount = getsGame (length . (.entities.attachments))

putInDiscard :: PlayerId -> CardDef -> GameT Card
putInDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . discardL %~ (card :)
  pure card

putOnDeck :: PlayerId -> [CardDef] -> GameT [Card]
putOnDeck pid defs = do
  cards <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (cards <>)
  pure cards

spec :: Spec
spec = describe "Against the Shadow player cards" do
  describe "Hirluin the Fair" do
    it "pays for an Outlands ally of any sphere, which gives him its bonus" . withHeroes [hirluin, beravor, eowyn] $ \pid -> do
      _ <- playWith pid hirluin 1 Allies.knightsOfTheSwan
      allyCount `shouldReturn` 1
      (getAttack =<< hero hirluin) `shouldReturn` 2

  describe "Records" do
    it "cost less only for heroes with the printed icon, not a gained one" . withHeroes [theoden, gimli, beravor] $ \pid -> do
      _ <- putAttachmentOn pid Attachments.songOfBattle =<< hero beravor
      card <- putCardIntoHand pid Attachments.bookOfEldacar
      run $ PlaceTokens GameSource GameTarget Resource 0
      -- Théoden and Gimli are printed tactics; Beravor's gained icon does not count
      getModifiers (CardIdTarget card.id) >>= (`shouldSatisfy` elem (CostModifier (-2)))

  describe "Book of Eldacar" do
    it "costs 1 less for each tactics hero, and plays a tactics event from the discard pile to the bottom of the deck" . withHeroes [theoden, gimli, legolas] $ \pid -> do
      blade <- putInDiscard pid Events.bladeMastery
      th <- hero theoden
      run $ PlaceTokens GameSource th Resource 1
      _ <- playWith pid theoden 1 Attachments.bookOfEldacar
      chooseTarget pid th
      attachmentCount `shouldReturn` 1
      chooseTarget pid (CardIdTarget blade.id)
      chooseTarget pid th
      attachmentCount `shouldReturn` 0
      (getDefense th) `shouldReturn` 3
      (lastMay . map (.id) . (.deck) <$> getPlayer pid) `shouldReturn` Just blade.id

  describe "Pippin" do
    it "returns an enemy that engages him to the staging area, where it cannot engage him this round" . withHeroes [pippin, frodo, bilbo] $ \pid -> do
      eid <- stage Enemies.blackForestBats
      threat0 <- (.threat) <$> getPlayer pid
      run $ EngageEnemy pid eid
      chooseAbilityOf pid =<< hero pippin
      (.threat) <$> getPlayer pid `shouldReturn` threat0 + 3
      enemyPlacement' eid `shouldReturn` StagingArea
      run $ EngagementCheck pid 0
      enemyPlacement' eid `shouldReturn` StagingArea

  describe "Against the Shadow" do
    it "gives spirit characters their willpower as defense" . withHeroes [pippin, frodo, eowyn] $ \pid -> do
      _ <- playWith pid eowyn 2 Events.againstTheShadow
      (getDefense =<< hero eowyn) `shouldReturn` 4

  describe "White Tower Watchman" do
    it "may take an undefended attack's damage while the heroes share a sphere" . withHeroes [pippin, frodo, eowyn] $ \pid -> do
      watchman <- putAllyIntoPlay pid Allies.whiteTowerWatchman
      eid <- stage Enemies.blackForestBats
      run $ EngageEnemy pid eid
      run $ UndefendedDamage pid eid
      chooseTarget pid watchman
      getCharacterDamage watchman `shouldReturn` 1

  describe "Minas Tirith Lampwright" do
    it "discards the next card revealed, of the type named, after a surge" . gameTest $ \pid -> do
      lampwright <- putAllyIntoPlay pid Allies.minasTirithLampwright
      [_, road] <- setEncounterDeck [Enemies.mirkwoodBats, Locations.oldForestRoad]
      run RevealEncounterCard
      chooseAbilityOf pid lampwright
      chooseLabel "$label.minasTirithLampwright.location" pid
      allyCount `shouldReturn` 0
      map (.id) <$> getEncounterDiscard `shouldReturn` [road.id]
      getsGame (length . (.entities.locations)) `shouldReturn` 0

    it "waits past the end of the round for the next card revealed" . gameTest $ \pid -> do
      lampwright <- putAllyIntoPlay pid Allies.minasTirithLampwright
      _ <- setEncounterDeck [Enemies.mirkwoodBats]
      run RevealEncounterCard
      chooseAbilityOf pid lampwright
      chooseLabel "$label.minasTirithLampwright.location" pid
      run EndRound
      getsGame (any (any (\case DiscardNextRevealed _ -> True; _ -> False) . effectModifiers) . gameEffects) `shouldReturn` True

  describe "Hobbit-sense" do
    it "stops the engaged enemies attacking, and the player attacking" . withHeroes [pippin, frodo, bilbo] $ \pid -> do
      eid <- stage Enemies.forestSpider
      run $ EngageEnemy pid eid
      chooseLabel "$label.pass" pid
      card <- putCardIntoHand pid Events.hobbitSense
      p <- hero pippin
      run $ PlaceTokens GameSource p Resource 2
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ BeginPhase CombatPhase
      chooseTarget pid (CardIdTarget card.id)
      passActions pid
      getQuestion pid `shouldReturn` Nothing
      getCharacterDamage p `shouldReturn` 0

    it "stops only the enemies engaged as it is played, not ones engaging later (FAQ 1.55)" . withHeroes [pippin, frodo, bilbo] $ \pid -> do
      eid <- stage Enemies.forestSpider
      run $ EngageEnemy pid eid
      chooseLabel "$label.pass" pid
      card <- putCardIntoHand pid Events.hobbitSense
      p <- hero pippin
      run $ PlaceTokens GameSource p Resource 2
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ BeginPhase CombatPhase
      chooseTarget pid (CardIdTarget card.id)
      -- the action window waits, so the later enemy comes in past it
      spider <- genEncounterCard Enemies.forestSpider
      _ <- runDebug $ AddToStaging spider
      later <- getsGame (fromJustNote "no spider" . find ((== spider.id) . enemyCardId . toAttrs) . toList . (.entities.enemies)) <&> toId
      _ <- runDebug $ EngageEnemy pid later
      getModifiers (toTarget eid) >>= (`shouldSatisfy` elem CannotAttack)
      getModifiers (toTarget later) >>= (`shouldSatisfy` notElem CannotAttack)

  describe "Advance Warning" do
    it "stops engagement checks only for enemies in play as it is played (FAQ 1.55)" . withHeroes ["06032", beravor, bilbo] $ \pid -> do
      early <- stage Enemies.forestSpider
      _ <- playWith pid beravor 2 Events.advanceWarning
      -- the planning window waits, so the later enemy comes in past it
      spider <- genEncounterCard Enemies.forestSpider
      _ <- runDebug $ AddToStaging spider
      later <- getsGame (fromJustNote "no spider" . find ((== spider.id) . enemyCardId . toAttrs) . toList . (.entities.enemies)) <&> toId
      getModifiers (toTarget early) >>= (`shouldSatisfy` elem NoEngagementChecks)
      getModifiers (toTarget later) >>= (`shouldSatisfy` notElem NoEngagementChecks)

  describe "Men of the West" do
    it "returns an Outlands ally from the discard pile to hand for each leadership resource" . withHeroes [hirluin, beravor, eowyn] $ \pid -> do
      warrior <- putInDiscard pid Allies.warriorOfLossarnach
      knights <- putInDiscard pid Allies.knightsOfTheSwan
      _ <- playWith pid hirluin 2 Events.menOfTheWest
      chooseTarget pid (CardIdTarget warrior.id)
      chooseTarget pid (CardIdTarget knights.id)
      sort . map (.id) . (.hand) <$> getPlayer pid `shouldReturn` sort [warrior.id, knights.id]
      (getHeroResources . toId =<< getHeroWithCode hirluin) `shouldReturn` 0

    it "returns no ally played for no cost, as X is 0" . withHeroes [hirluin, beravor, eowyn] $ \pid -> do
      warrior <- putInDiscard pid Allies.warriorOfLossarnach
      card <- genPlayerCard Events.menOfTheWest (Just pid)
      hirluin' <- getHeroWithCode hirluin
      run $ PlaceTokens GameSource (toTarget hirluin') Resource 2
      run $ PlayAtNoCost pid card
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` []
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [card.id, warrior.id]

  describe "Emery" do
    it "cannot be put into play with fewer than 3 cards in the deck to discard" . withHeroes [pippin, frodo, eowyn] $ \pid -> do
      g <- getGame
      putGame $ g & playersL . ix pid . deckL .~ []
      _ <- putOnDeck pid [Events.aTestOfWill, Events.aTestOfWill]
      card <- putCardIntoHand pid Allies.emery
      run $ ActionWindow pid
      offered <- maybe False (any \case TargetLabel (CardIdTarget cid) _ -> cid == card.id; _ -> False) . fmap questionChoices <$> getQuestion pid
      offered `shouldBe` False

    it "is put into play for the top 3 cards of the deck, and discarded if one is off-sphere" . withHeroes [pippin, frodo, eowyn] $ \pid -> do
      _ <- putOnDeck pid [Events.aTestOfWill, Events.aTestOfWill, Events.bladeMastery]
      card <- putCardIntoHand pid Allies.emery
      run $ ActionWindow pid
      chooseOptionMatching "use Emery" pid \case
        TargetLabel (CardIdTarget cid) _ -> cid == card.id
        _ -> False
      allyCount `shouldReturn` 0
      length . (.discard) <$> getPlayer pid `shouldReturn` 4

    it "stays in play when the discarded cards are all spirit or neutral" . withHeroes [pippin, frodo, eowyn] $ \pid -> do
      _ <- putOnDeck pid [Events.aTestOfWill, Events.aTestOfWill, Events.aTestOfWill]
      card <- putCardIntoHand pid Allies.emery
      run $ ActionWindow pid
      chooseOptionMatching "use Emery" pid \case
        TargetLabel (CardIdTarget cid) _ -> cid == card.id
        _ -> False
      allyCount `shouldReturn` 1
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` []

  describe "Caldara" do
    it "is discarded to put a spirit ally into play for each other spirit hero" . withHeroes [caldara, frodo, beravor] $ \pid -> do
      refugee <- putInDiscard pid Allies.silvanRefugee
      _ <- putInDiscard pid Allies.ethirSwordsman
      run $ ActionWindow pid
      chooseAbilityOf pid =<< hero caldara
      chooseTarget pid (CardIdTarget refugee.id)
      allyCount `shouldReturn` 1
      getsGame (length . (.entities.heroes)) `shouldReturn` 2

    it "counts only printed spirit icons, not gained ones" . withHeroes [caldara, frodo, beravor] $ \pid -> do
      _ <- putAttachmentOn pid Attachments.songOfTravel =<< hero beravor
      refugee <- putInDiscard pid Allies.silvanRefugee
      _ <- putInDiscard pid Allies.ethirSwordsman
      run $ ActionWindow pid
      chooseAbilityOf pid =<< hero caldara
      chooseTarget pid (CardIdTarget refugee.id)
      -- Frodo's printed icon brings one ally; Beravor's gained one brings none
      allyCount `shouldReturn` 1

  describe "Mithrandir's Advice" do
    it "draws 1 card for each hero with a printed lore icon only" . withHeroes [beravor, eowyn, gimli] $ \pid -> do
      _ <- putAttachmentOn pid Attachments.songOfWisdom =<< hero eowyn
      _ <- playWith pid beravor 1 Events.mithrandirsAdvice
      length . (.hand) <$> getPlayer pid `shouldReturn` 1

  describe "Small Target" do
    it "turns the attack on a Hobbit onto another engaged enemy when the shadow card has no effect" . withHeroes [pippin, frodo, bilbo] $ \pid -> do
      attacker <- stage Enemies.forestSpider
      other <- stage Enemies.blackForestBats
      run $ EngageEnemy pid attacker
      chooseLabel "$label.pass" pid
      run $ EngageEnemy pid other
      chooseLabel "$label.pass" pid
      card <- putCardIntoHand pid Events.smallTarget
      p <- hero pippin
      run $ PlaceTokens GameSource p Resource 1
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard attacker
      run $ DeclareDefenders pid attacker []
      chooseTarget pid p
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid other
      passActions pid
      getCharacterDamage p `shouldReturn` 0
      -- Forest Spider's 2 attack against Black Forest Bats' 0 defense destroys them
      enemyPlacement' other `shouldReturn` Unplaced

    it "reveals the shadow card chosen, the others resolving, and reckons the damage at the damage step" . withHeroes [pippin, frodo, bilbo] $ \pid -> do
      attacker <- stage Enemies.forestSpider
      other <- stage Enemies.forestSpider
      run $ EngageEnemy pid attacker
      chooseLabel "$label.pass" pid
      run $ EngageEnemy pid other
      chooseLabel "$label.pass" pid
      card <- putCardIntoHand pid Events.smallTarget
      p <- hero pippin
      run $ PlaceTokens GameSource p Resource 1
      [road, _] <- setEncounterDeck [Locations.oldForestRoad, Enemies.easternCrows]
      run $ DealShadowCard attacker
      run $ DealShadowCard attacker
      run $ DeclareDefenders pid attacker []
      chooseTarget pid p
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid other
      chooseTarget pid (CardIdTarget road.id)
      passActions pid
      getCharacterDamage p `shouldReturn` 0
      -- Forest Spider's 3 attack (+1 for engaging), +1 from Eastern Crows' shadow, against
      -- the other's 1 defense
      getCharacterDamage (EnemyTarget other) `shouldReturn` 3

    it "resolves the attack as normal against an enemy dealt no shadow card" . withHeroes [pippin, frodo, bilbo] $ \pid -> do
      attacker <- stage Enemies.forestSpider
      other <- stage Enemies.blackForestBats
      run $ EngageEnemy pid attacker
      chooseLabel "$label.pass" pid
      run $ EngageEnemy pid other
      chooseLabel "$label.pass" pid
      card <- putCardIntoHand pid Events.smallTarget
      p <- hero pippin
      run $ PlaceTokens GameSource p Resource 1
      _ <- setEncounterDeck []
      run $ DeclareDefenders pid attacker []
      chooseTarget pid p
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid other
      passActions pid
      enemyPlacement' other `shouldReturn` EngagedWith pid

    it "is offered against an attack from the staging area with one enemy engaged" . withHeroes [pippin, frodo, bilbo] $ \pid -> do
      attacker <- stage Enemies.forestSpider
      other <- stage Enemies.blackForestBats
      run $ EngageEnemy pid other
      chooseLabel "$label.pass" pid
      card <- putCardIntoHand pid Events.smallTarget
      p <- hero pippin
      run $ PlaceTokens GameSource p Resource 1
      _ <- setEncounterDeck []
      run $ DeclareDefenders pid attacker []
      chooseTarget pid p
      offered <- maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices <$> getQuestion pid
      offered `shouldBe` True

  describe "Well-Equipped" do
    it "attaches a discarded attachment to a Dwarf" . withHeroes [gimli, legolas, theoden] $ \pid -> do
      [_, axe] <- putOnDeck pid [Events.bladeMastery, Attachments.dwarvenAxe]
      g <- hero gimli
      _ <- playWith pid gimli 0 Events.wellEquipped
      chooseTarget pid (CardIdTarget axe.id)
      getAttack g `shouldReturn` 4

  describe "Théoden" do
    it "gives +1 willpower only to heroes with the printed tactics icon, not a gained one" . withHeroes [theoden, gimli, beravor] $ \pid -> do
      _ <- putAttachmentOn pid Attachments.songOfBattle =<< hero beravor
      run $ PlaceTokens GameSource GameTarget Resource 0
      (getWillpower =<< hero gimli) `shouldReturn` 3
      (getWillpower =<< hero beravor) `shouldReturn` 2

  describe "Mirlonde" do
    it "lowers the starting threat of each lore hero by 1" . withHeroes ["06032", beravor, bilbo] $ \pid -> do
      run Setup
      (.threat) <$> getPlayer pid `shouldReturn` 24

  describe "Gondorian Discipline" do
    it "cancels 2 of the damage just dealt to a Gondor character" . withHeroes ["05002", theoden, gimli] $ \pid -> do
      card <- putCardIntoHand pid Events.gondorianDiscipline
      boromir <- hero "05002"
      run $ DealDamage GameSource boromir 3
      chooseTarget pid (CardIdTarget card.id)
      getCharacterDamage boromir `shouldReturn` 1

  describe "The Hammer-stroke" do
    it "engages each enemy in play in an encounter phase action window" . withHeroes [theoden, gimli, legolas] $ \pid -> do
      eid <- stage Enemies.blackForestBats
      card <- putCardIntoHand pid Events.theHammerStroke
      th <- hero theoden
      run $ PlaceTokens GameSource th Resource 2
      run $ BeginPhase EncounterPhase
      chooseLabel "$label.doNotEngage" pid
      chooseTarget pid (CardIdTarget card.id)
      enemyPlacement' eid `shouldReturn` EngagedWith pid

  describe "Palantir" do
    it "draws a card for each of the top 3 encounter cards of the named type, and raises threat 2 for each other" . withHeroes [theoden, gimli, legolas] $ \pid -> do
      palantir <- putAttachmentOn pid Attachments.palantir =<< hero theoden
      _ <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad, Enemies.blackForestBats]
      threat0 <- (.threat) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid palantir
      chooseLabel "$label.palantir.enemy" pid
      length . (.hand) <$> getPlayer pid `shouldReturn` 2
      (.threat) <$> getPlayer pid `shouldReturn` threat0 + 2

  describe "Knight of Minas Tirith" do
    it "engages and attacks an enemy in the staging area as it enters play" . withHeroes [theoden, gimli, legolas] $ \pid -> do
      _ <- stage Enemies.blackForestBats
      knight <- putAllyIntoPlay pid Allies.knightOfMinasTirith
      chooseAbilityOf pid knight
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
