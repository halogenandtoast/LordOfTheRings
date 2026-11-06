module LordOfTheRings.EredMithrin.SharedEncounterSetsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (AllyMatcher (..), HeroMatcher (..), PlayerMatcher (..), getEnemy, playerCardsGuardedBy, select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), cavesDeckL)
import LordOfTheRings.Trait (Trait (Dragon))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT (Maybe Placement)
placementOfEnemy eid = getsGame (fmap (.placement) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

activeCodes :: GameT [CardCode]
activeCodes = getsGame (map toCardCode . filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

handIds :: PlayerId -> GameT [CardId]
handIds pid = map (.id) . (.hand) <$> getPlayer pid

handCodes :: PlayerId -> GameT [CardCode]
handCodes pid = map toCardCode . (.hand) <$> getPlayer pid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

lasting :: Target -> ModifierType -> GameT ()
lasting t m = run $ CreateEffect $ Effect GameSource t [m] UntilEndOfRound

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

setCavesDeck :: [CardDef] -> GameT [EncounterCard]
setCavesDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (cavesDeckL .~ cards)
  pure cards

alliesInPlay :: GameT Int
alliesInPlay = getsGame (length . (.entities.allies))

{- | The enemy attacks the player, the shadow card dealt to it first, and the defender
declares against it.
-}
shadowAttack :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
shadowAttack pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

-- | As 'shadowAttack', the attack left undefended.
undefendedShadowAttack :: PlayerId -> EnemyId -> CardDef -> GameT ()
undefendedShadowAttack pid eid shadow = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

spec :: Spec
spec = describe "The Wilds of Rhovanion's shared encounter sets" do
  describe "Grey Mountain Goblins" do
    it "gives each other Goblin engaged with its player +1 attack and +1 defense, and refuses player attachments" . gameTest $ \pid -> do
      swordsman <- engaged pid Enemies.goblinSwordsman
      troop <- engaged pid Enemies.goblinTroop
      getEnemyAttack swordsman `shouldReturn` 4
      getEnemyAttack troop `shouldReturn` 5
      getModifiers troop >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)

    it "gives the attacking enemy +2 attack with Goblin Troop's shadow" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      shadowAttack pid spider Enemies.goblinTroop beravor
      -- Forest Spider's 3 attack, raised as it engaged, against Beravor's 2 defense
      getDamage beravor `shouldReturn` 3

    it "has a staged Stray Goblin engage the player who engages an enemy, never optionally" . gameTest $ \pid -> do
      stray <- stage Enemies.strayGoblinGreyMountains
      getModifiers stray >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))
      _ <- engaged pid Enemies.forestSpider
      chooseAbilityOf pid (EnemyTarget stray)
      placementOfEnemy stray `shouldReturn` Just (EngagedWith pid)

    it "adds Stray Goblin to the staging area as its shadow resolves" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      shadowAttack pid spider Enemies.strayGoblinGreyMountains =<< hero "01012"
      stray <- enemyOf Enemies.strayGoblinGreyMountains
      placementOfEnemy stray `shouldReturn` Just StagingArea

  describe "Hills of Wilderland" do
    it "has Pack of Wargs attack again, with no shadow card, when dealt a shadow card with no effect" . gameTest $ \pid -> do
      wargs <- engaged pid Enemies.packOfWargs
      beravor <- hero "01012"
      shadowAttack pid wargs Locations.oldForestRoad beravor
      getDamage beravor `shouldReturn` 2
      -- the additional attack asks for a defender again, dealt no other shadow card
      Just (QuestionLabel _ source _ _) <- getQuestion pid
      source `shouldBe` Just (toTarget wargs)
      -- FAQ 1.42 discards the previous attack's shadows, even with no new deal.
      fmap (length . (.shadowCards)) (getEnemy wargs) `shouldReturn` 0

    it "returns the attacking enemy to the staging area after the attack with Pack of Wargs' shadow" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      shadowAttack pid spider Enemies.packOfWargs =<< hero "01012"
      placementOfEnemy spider `shouldReturn` Just StagingArea

    it "has Hills of Wilderland's threat be the most characters a player controls, lowered by the first player's quest action" . gameTest $ \pid -> do
      hills <- stageLocation Locations.hillsOfWilderland
      getLocationThreat hills `shouldReturn` 3
      inPhase QuestPhase
      run $ ActionWindow pid
      chooseAbilityOf pid hills
      chooseTarget pid =<< hero "01012"
      exhausted "01012" `shouldReturn` True
      getLocationThreat hills `shouldReturn` 2

    it "places an event played in the quest phase under Lonely Lands, +2 threat for each" . gameTest $ \pid -> do
      lands <- stageLocation Locations.lonelyLands
      card <- genPlayerCard Events.sneakAttack (Just pid)
      run $ PutInDiscard card
      inPhase QuestPhase
      run $ CheckWindows [mkAfter $ PlayedCard pid card]
      (.underneath) . toAttrs <$> getLocation lands `shouldReturn` [PlayerCard card]
      getLocationThreat lands `shouldReturn` 4
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` []
      run $ Discard GameSource (LocationTarget lands)
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [card.id]

  describe "Lost in Wilderland" do
    it "raises a player's threat by 1 for each ally for Ruined Supplies, or damages each ally" . gameTest $ \pid -> do
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      reveal [Treacheries.ruinedSupplies]
      chooseLabel "$label.ruinedSupplies.damage" pid
      getDamage guardian `shouldReturn` 1
      reveal [Treacheries.ruinedSupplies]
      chooseLabelStarting "$label.ruinedSupplies.raiseThreat" pid
      -- Doomed 1 for each, and 1 for the ally
      threatOf pid `shouldReturn` 3

    it "raises the defending player's threat by 1 for each enemy engaged with him with Ruined Supplies' shadow" . gameTest $ \pid -> do
      _ <- engaged pid Enemies.eastBightPatrol
      spider <- engaged pid Enemies.forestSpider
      _ <- setEncounterDeck [Treacheries.ruinedSupplies]
      run $ DealShadowCard spider
      inPhase CombatPhase
      run $ EnemyAttack pid spider
      passActions pid
      chooseTarget pid =<< hero "01012"
      passActions pid
      threatOf pid `shouldReturn` 2

    it "goes to the hand of the player with the most cards for Lost in the Wild, which discards his hand once he plays a card" . gameTest $ \pid -> do
      replicateM_ 2 $ putCardIntoHand pid Allies.guardOfTheCitadel
      reveal [Treacheries.lostInTheWild]
      handCodes pid >>= (`shouldSatisfy` elem (toCardCode Treacheries.lostInTheWild))
      played <- genPlayerCard Events.sneakAttack (Just pid)
      run $ CheckWindows [mkAfter $ PlayedCard pid played]
      handSize pid `shouldReturn` 0
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Treacheries.lostInTheWild))

    it "attaches Weighed Down to the hero with the most attachments and exhausts it, discarding an attachment as it readies" . gameTest $ \pid -> do
      beravor <- hero "01012"
      blade <- putAttachmentOn pid Attachments.bladeOfGondolin beravor
      reveal [Treacheries.weighedDown]
      exhausted "01012" `shouldReturn` True
      [weighed] <- getsGame (map toTarget . toList . (.entities.treacheries))
      run $ Ready beravor
      -- only the Blade can be chosen while it is there, then Weighed Down itself
      getsGame (map toTarget . toList . (.entities.attachments)) `shouldReturn` []
      run $ Exhaust beravor
      run $ Ready beravor
      getsGame (map toTarget . toList . (.entities.treacheries)) `shouldReturn` []
      _ <- pure (blade, weighed)
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  describe "Dark Woods" do
    it "keeps When Revealed effects from being canceled while Accursed Forest is staged, and reveals a card as the players travel there" . gameTest $ \_ -> do
      forest <- stageLocation Locations.accursedForest
      getModifiers GameTarget >>= (`shouldSatisfy` elem WhenRevealedCannotBeCanceled)
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ TravelTo forest
      getsGame (length . (.entities.locations)) `shouldReturn` 2

    it "lets no more than 1 progress a round onto staged Forest locations while Dark Black Woods is staged" . gameTest $ \_ -> do
      woods <- stageLocation Locations.darkBlackWoods
      run $ PlaceTokens GameSource (toTarget woods) Progress 2
      (.progress) . toAttrs <$> getLocation woods `shouldReturn` 1
      run $ PlaceTokens GameSource (toTarget woods) Progress 1
      (.progress) . toAttrs <$> getLocation woods `shouldReturn` 1

    it "adds each enemy the players discard to the staging area as they travel to Dark Black Woods" . gameTest $ \_ -> do
      woods <- stageLocation Locations.darkBlackWoods
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ TravelTo woods
      spider <- enemyOf Enemies.forestSpider
      placementOfEnemy spider `shouldReturn` Just StagingArea

  describe "Gathering Gloom" do
    it "reveals an encounter card for Gathering Gloom when a player chooses so" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Treacheries.gatheringGloom, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.gatheringGloom.reveal" pid
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "discards an ally of the defending player if the attack with Gathering Gloom's shadow destroys a character" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      spider <- engaged pid Enemies.forestSpider
      eowyn <- hero "01007"
      run $ DealDamage GameSource eowyn 2
      shadowAttack pid spider Treacheries.gatheringGloom eowyn
      alliesInPlay `shouldReturn` 0

    it "has each player discard the resources from his heroes' pools for Swarm of Bats, raising his threat as much" . gameTest $ \pid -> do
      beravor <- hero "01012"
      run $ PlaceTokens GameSource beravor Resource 2
      reveal [Treacheries.swarmOfBats]
      threatOf pid `shouldReturn` 2
      (getHeroResources . toId =<< getHeroWithCode "01012") `shouldReturn` 0

    it "has Swarm of Bats surge when no resource is discarded" . gameTest $ \_ -> do
      reveal [Treacheries.swarmOfBats, Locations.oldForestRoad]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "gives the attacking enemy +2 attack with Swarm of Bats' shadow if the defender has an attachment" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      _ <- putAttachmentOn pid Attachments.citadelPlate beravor
      shadowAttack pid spider Treacheries.swarmOfBats beravor
      getDamage beravor `shouldReturn` 3

  describe "Afraid of the Dark" do
    it "attaches Afraid of the Dark to a questing hero, removed from the quest, blank with no willpower at a Dark active location" . gameTest $ \_ -> do
      eowyn <- hero "01007"
      run $ CommitToQuest eowyn
      reveal [Treacheries.afraidOfTheDark]
      select CommittedHero `shouldReturn` []
      getModifiers eowyn >>= (`shouldSatisfy` notElem Blank)
      _ <- activeLocation Locations.darkTunnel
      getModifiers eowyn >>= (`shouldSatisfy` elem Blank)
      getWillpower eowyn `shouldReturn` 0

    it "discards a questing character for Eyes in the Dark when a player chooses so" . gameTest $ \pid -> do
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ CommitToQuest guardian
      reveal [Treacheries.eyesInTheDark]
      chooseLabel "$label.eyesInTheDark.discard" pid
      alliesInPlay `shouldReturn` 0

    it "discards an ally of the defending player if the attack with Eyes in the Dark's shadow is undefended" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      spider <- engaged pid Enemies.forestSpider
      undefendedShadowAttack pid spider Treacheries.eyesInTheDark
      alliesInPlay `shouldReturn` 0

  describe "Fell Beasts" do
    it "has the player discard a card from his hand for each excess point of damage Stone-troll's attack deals" . gameTest $ \pid -> do
      replicateM_ 3 $ putCardIntoHand pid Allies.guardOfTheCitadel
      troll <- engaged pid Enemies.stoneTroll
      getModifiers troll >>= (`shouldSatisfy` elem CannotHaveAttachments)
      eowyn <- hero "01007"
      shadowAttack pid troll Locations.oldForestRoad eowyn
      -- 6 attack against 1 defense: 5 damage to a hero of 3 hit points
      getQuestion pid >>= (`shouldSatisfy` isJust)
      replicateM_ 2 $ getQuestion pid >>= \case
        Just q | Just ui <- headMay (questionChoices q) -> chooseOptionMatching "discard" pid (== ui)
        _ -> pure ()
      handSize pid `shouldReturn` 1

    it "discards an attachment of the defending player with Stone-troll's shadow" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      _ <- putAttachmentOn pid Attachments.citadelPlate beravor
      plate <- getsGame (map toTarget . toList . (.entities.attachments))
      shadowAttack pid spider Enemies.stoneTroll beravor
      traverse_ (chooseTarget pid) plate
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

    it "guards the top card of the first player's deck as Hobgoblin enters play, with its cost added, and returns it to hand as it leaves" . gameTest $ \pid -> do
      plate <- genPlayerCard Attachments.citadelPlate (Just pid)
      run $ PutOnTopOfDeck pid (PlayerCard plate)
      hob <- stage Enemies.hobgoblin
      (playerCardsGuardedBy . (.cardId) . toAttrs =<< getEnemy hob) `shouldReturn` [plate]
      getEnemyThreat hob `shouldReturn` 5
      getEnemyAttack hob `shouldReturn` 5
      run $ Discard GameSource (EnemyTarget hob)
      handIds pid >>= (`shouldSatisfy` elem plate.id)

  describe "Wild Creatures" do
    it "deals 1 damage to Werewolf and its defender when dealt a shadow card with no effect, its attack raised by its damage" . gameTest $ \pid -> do
      wolf <- engaged pid Enemies.werewolf
      beravor <- hero "01012"
      shadowAttack pid wolf Locations.oldForestRoad beravor
      getDamage (toTarget wolf) `shouldReturn` 1
      getEnemyAttack wolf `shouldReturn` 3
      -- 1 from the Forced effect, 1 from an attack of 3 against 2 defense
      getDamage beravor `shouldReturn` 2

    it "deals 1 damage to the defender with Werewolf's shadow" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      shadowAttack pid spider Enemies.werewolf beravor
      -- 1 from the shadow, 1 from Forest Spider's 3 attack against 2 defense
      getDamage beravor `shouldReturn` 2

    it "bars a character Giant Spider's attack damages from readying this round" . gameTest $ \_ -> do
      spider <- stage Enemies.giantSpider
      eowyn <- hero "01007"
      run $ DealDamage (EnemyAttackSource spider) eowyn 1
      getModifiers eowyn >>= (`shouldSatisfy` elem CannotReady)

    it "bars the defender from readying this round with Giant Spider's shadow" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      shadowAttack pid spider Enemies.giantSpider beravor
      getModifiers beravor >>= (`shouldSatisfy` elem CannotReady)

    it "keeps damage off Black Bats while the active location is Dark" . gameTest $ \_ -> do
      bats <- stage Enemies.blackBats
      run $ DealDamage GameSource (toTarget bats) 1
      getDamage (toTarget bats) `shouldReturn` 1
      _ <- activeLocation Locations.darkTunnel
      run $ DealDamage GameSource (toTarget bats) 1
      getDamage (toTarget bats) `shouldReturn` 1

  describe "Deep Underground" do
    it "keeps progress off staged Dark locations while Dark Tunnel is staged" . gameTest $ \_ -> do
      _ <- stageLocation Locations.darkTunnel
      woods <- stageLocation Locations.darkBlackWoods
      hid <- toId <$> getHeroWithCode "01012"
      run $ PlaceTokens (HeroSource hid) (toTarget woods) Progress 1
      (.progress) . toAttrs <$> getLocation woods `shouldReturn` 0

    it "arranges the Caves deck as the players travel to Forked Passage, which goes to the victory display for the top card" . gameTest $ \pid -> do
      [armory, den] <- setCavesDeck [Locations.lostArmory, Locations.frightfulDen]
      passage <- stageLocation Locations.forkedPassage
      run $ TravelTo passage
      chooseAbilityOf pid passage
      chooseTarget pid (CardIdTarget den.id)
      threatOf pid `shouldReturn` 2
      victoryDisplay `shouldReturn` [toCardCode Locations.forkedPassage]
      activeCodes `shouldReturn` [toCardCode Locations.frightfulDen]
      map (.id) <$> getsGame ((.cavesDeck) . toAttrs . (.scenario)) `shouldReturn` [armory.id]

    it "adds the active location's threat to the staging area's for Dark Places, or finds a location without one" . gameTest $ \_ -> do
      _ <- activeLocation Locations.oldForestRoad
      was <- getStagingThreat
      reveal [Treacheries.darkPlaces]
      getStagingThreat `shouldReturn` (was + 1)

    it "adds the first location discarded to the staging area for Dark Places with no active location" . gameTest $ \_ -> do
      reveal [Treacheries.darkPlaces, Enemies.forestSpider, Locations.oldForestRoad]
      _ <- locationOf Locations.oldForestRoad
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Enemies.forestSpider))

  describe "Dragon Might" do
    it "attaches Dragon Scales to a Dragon enemy, cancelling the next damage to it" . gameTest $ \_ -> do
      spider <- stage Enemies.forestSpider
      lasting (EnemyTarget spider) (GainTrait Dragon)
      reveal [Treacheries.dragonScales, Locations.oldForestRoad]
      getsGame (length . (.entities.treacheries)) `shouldReturn` 1
      run $ DealDamage GameSource (toTarget spider) 2
      getDamage (toTarget spider) `shouldReturn` 0
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0
      run $ DealDamage GameSource (toTarget spider) 2
      getDamage (toTarget spider) `shouldReturn` 2

    it "deals 1 damage to each character of the player a Dragon with Dragon Breath attacks" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      lasting (EnemyTarget spider) (GainTrait Dragon)
      reveal [Treacheries.dragonBreath, Locations.oldForestRoad]
      beravor <- hero "01012"
      shadowAttack pid spider Locations.oldForestRoad beravor
      -- 1 from Dragon Breath, 1 from Forest Spider's 3 attack against 2 defense
      getDamage beravor `shouldReturn` 2
      (getDamage =<< hero "01007") `shouldReturn` 1
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  describe "Lost Caves" do
    it "lets each player search his deck for a Weapon or Armor once Lost Armory is active" . gameTest $ \pid -> do
      blade <- genPlayerCard Attachments.bladeOfGondolin (Just pid)
      run $ PutOnTopOfDeck pid (PlayerCard blade)
      armory <- activeLocation Locations.lostArmory
      chooseAbilityOf pid armory
      chooseTarget pid (CardIdTarget blade.id)
      handIds pid >>= (`shouldSatisfy` elem blade.id)

    it "lets each player reduce his threat, draw or gain a resource once Ancient Treasury is active" . gameTest $ \pid -> do
      treasury <- activeLocation Locations.ancientTreasury
      was <- handSize pid
      chooseAbilityOf pid treasury
      chooseLabel "$label.ancientTreasury.draw" pid
      handSize pid `shouldReturn` (was + 2)

    it "adds an enemy for each player to the staging area as Frightful Den becomes active" . gameTest $ \_ -> do
      _ <- setEncounterDeck [Locations.oldForestRoad, Enemies.forestSpider]
      _ <- activeLocation Locations.frightfulDen
      spider <- enemyOf Enemies.forestSpider
      placementOfEnemy spider `shouldReturn` Just StagingArea

    it "resolves the When Revealed of the first treachery discarded once the players commit, while Lightless Grotto is in play" . gameTest $ \pid -> do
      _ <- activeLocation Locations.lightlessGrotto
      _ <- setEncounterDeck [Locations.oldForestRoad, Treacheries.ruinedSupplies]
      run $ CheckWindows [mkWhen StagingStepBegins]
      resolveRevealed
      chooseLabelStarting "$label.ruinedSupplies.raiseThreat" pid
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Treacheries.ruinedSupplies))

    it "has each player assign X damage among his X characters as Crumbling Cavern becomes active" . gameTest $ \pid -> do
      _ <- activeLocation Locations.crumblingCavern
      beravor <- hero "01012"
      replicateM_ 3 $ chooseTarget pid beravor
      getDamage beravor `shouldReturn` 3

    it "has Underground Lake's threat be the most characters a player controls, exhausting each ally entering play" . gameTest $ \pid -> do
      lake <- stageLocation Locations.undergroundLake
      getLocationThreat lake `shouldReturn` 3
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      map toTarget <$> select ExhaustedAlly `shouldReturn` [guardian]

  describe "Haldan" do
    it "comes under the first player's control, ready, as the enemy guarding him leaves play, and loses the game as he leaves play" . gameTest $ \pid -> do
      spider <- stage Enemies.forestSpider
      captor <- getEnemy spider
      card <- genEncounterCard Objectives.haldan
      run $ GuardCard (EncounterCard card) (toAttrs captor).cardId
      select (AllyControlledBy $ PlayerWithId pid) `shouldReturn` []
      run $ Discard GameSource (EnemyTarget spider)
      [haldan] <- select (AllyControlledBy $ PlayerWithId pid)
      select (AllyWithId haldan <> ExhaustedAlly) `shouldReturn` []
      _ <- activeLocation Locations.oldForestRoad
      getModifiers (AllyTarget haldan) >>= (`shouldSatisfy` elem DoesNotExhaustToCommit)
      run $ Discard GameSource (AllyTarget haldan)
      getsGame (.outcome) `shouldReturn` Just Lost
