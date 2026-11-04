module LordOfTheRings.VengeanceOfMordor.TheLandOfSorrowSpec (spec) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdAttack, cdCardTraits, cdEncounterSet, cdEngagementCost), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEngagementCost, getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getArcheryTotal)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theLandOfSorrow)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, victoryDisplayL)
import LordOfTheRings.Trait (Trait (Camp, Captive, Warden, Weapon))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

sorrowTest :: (PlayerId -> GameT ()) -> IO ()
sorrowTest = gameTestIn theLandOfSorrow testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup, its stage 1A clicked.
setUp :: PlayerId -> GameT ()
setUp = flipFirstQuest

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

hasTrait :: Trait -> CardDef -> Bool
hasTrait t = (t `member`) . cdCardTraits

-- | The Camp location of the card.
camp :: CardDef -> GameT LocationAttrs
camp def = getsGame (toAttrs . fromJustNote "no such camp" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

camps :: GameT [LocationAttrs]
camps = getsGame (filter (hasTrait Camp . toCardDef) . map toAttrs . toList . (.entities.locations))

-- | The Captive allies the card guards.
guardedBy :: LocationAttrs -> GameT [CardCode]
guardedBy l = getsGame (map toCardCode . filter ((== AttachedTo (CardIdTarget l.cardId)) . (.placement)) . toList . (.entities.allies))

captive :: CardDef -> GameT AllyAttrs
captive def = getsGame (toAttrs . fromJustNote "no such captive" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

-- | A Captive, put into play under the player's control.
controlCaptive :: PlayerId -> CardDef -> GameT Target
controlCaptive pid def = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
  toTarget <$> captive def

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

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

attachedTo :: EnemyId -> GameT [CardCode]
attachedTo eid = getsGame (map toCardCode . filter ((== AttachedTo (EnemyTarget eid)) . (.placement)) . toList . (.entities.treacheries))

treacheryOf :: CardDef -> GameT Target
treacheryOf def = getsGame (toTarget . fromJustNote "no such treachery" . find ((== toCardCode def) . toCardCode) . toList . (.entities.treacheries))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

exhausted :: Target -> GameT Bool
exhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "no hero" . lookup hid . (.entities.heroes))
  AllyTarget aid -> allyExhausted . toAttrs <$> getsGame (fromJustNote "no ally" . lookup aid . (.entities.allies))
  _ -> pure False

-- | Whether the open question offers the target.
offers :: PlayerId -> Target -> GameT Bool
offers pid t = maybe False (any \case TargetLabel t' _ -> t == t'; _ -> False) . fmap questionChoices <$> getQuestion pid

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

-- | The enemy attacks the player, the shadow card dealt to it first.
shadowAttack :: PlayerId -> EnemyId -> CardDef -> GameT ()
shadowAttack pid eid shadow = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

-- | Explore the Camp: it becomes the active location, its Forced effect untriggered, and
-- takes its quest points in progress.
explore :: LocationAttrs -> GameT ()
explore l = do
  run $ PlaceLocation l.id ActiveLocation
  run $ PlaceTokens GameSource (LocationTarget l.id) Progress 10

spec :: Spec
spec = describe "The Land of Sorrow" do
  it "is a scenario the game can be dealt" . sorrowTest $ \_ ->
    isJust (lookupScenario "the-land-of-sorrow") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "stages each Camp guarding a random Captive with a random Warden under it, stage 2 in the quest deck" . sorrowTest $ \pid -> do
      setUp pid
      cs <- camps
      sort (map toCardCode cs) `shouldBe` sort (map toCardCode [Locations.theBarracks, Locations.slaveQuarters, Locations.theStorehouse])
      all ((== StagingArea) . (.placement)) cs `shouldBe` True
      guarded <- traverse guardedBy cs
      map length guarded `shouldBe` [1, 1, 1]
      sort (concat guarded) `shouldBe` sort (map toCardCode [Objectives.edrahil, Objectives.arador, Objectives.farin])
      let under = map (map toCardCode . (.underneath)) cs
      map length under `shouldBe` [1, 1, 1]
      sort (concat under) `shouldBe` sort (map toCardCode [Enemies.ulwarth, Enemies.lorgan, Enemies.borlach])
      -- the Wardens are not in play, nor is any other location for a single player
      enemyCodes `shouldReturn` []
      length <$> locationCodes `shouldReturn` 3
      attrs <- scenarioAttrs
      attrs.scenarioSetAside `shouldBe` []
      any (\c -> any (`hasTrait` toCardDef c) [Camp, Captive, Warden]) attrs.encounterDeck `shouldBe` False
      all ((`elem` map Just [TheLandOfSorrow, EasterlingRaiders, RollingPlains]) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      map toCardCode attrs.questDeck `shouldBe` [toCardCode Quests.leadTheRevolt]
      activeQuestCode `shouldReturn` toCardCode Quests.slavesOfTheDarkLord
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

    it "shows the players the Warden faceup under each Camp" . sorrowTest $ \pid -> do
      setUp pid
      seen <- getsGame publicGame
      let under = parseMaybe (withObject "game" \o -> do
            ls <- (.: "locations") =<< o .: "entities"
            traverse (withObject "location" (.: "underneath")) (toList (ls :: Map Text Value))) seen
      fmap (map length) (under :: Maybe [[Value]]) `shouldBe` Just [1, 1, 1]

    it "adds the first location discarded to the staging area for a second player" . twoPlayerTestIn theLandOfSorrow testDeck $ \p1 p2 -> do
      run Setup
      replicateM_ 2 do
        asked <- filterM (fmap isJust . getQuestion) [p1, p2]
        for_ (take 1 asked) $ chooseLabel "$label.keepHand"
      lead <- getLead
      chooseOptionMatching "flip the quest" lead (const True)
      locations <- getsGame (map toCardDef . toList . (.entities.locations))
      length locations `shouldBe` 4
      length (filter (not . hasTrait Camp) locations) `shouldBe` 1

    it "builds the encounter deck in normal mode" . sorrowTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.easterlingGuard, Enemies.nurnSlaver, Locations.workingFields, Locations.watchTowerTheLandOfSorrow, Treacheries.overseersWhip, Treacheries.crueltyOfMordor, Treacheries.brokenCaptives]
        `shouldBe` [3, 4, 3, 3, 2, 3, 2]

    it "builds the easy mode encounter deck" . gameTestIn (overAttrs (modeL .~ EasyMode) theLandOfSorrow) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.easterlingGuard, Enemies.nurnSlaver, Locations.workingFields, Locations.watchTowerTheLandOfSorrow, Treacheries.overseersWhip, Treacheries.crueltyOfMordor, Treacheries.brokenCaptives]
        `shouldBe` [2, 3, 3, 1, 1, 2, 1]
      length . scenarioSetAside <$> scenarioAttrs `shouldReturn` 9

  -- \* Slaves of the Dark Lord

  describe "Slaves of the Dark Lord" do
    it "keeps the Camps immune, untravelable while a Warden is in play, and raises threat for each staged Camp at the end of the round" . sorrowTest $ \pid -> do
      setUp pid
      barracks <- camp Locations.theBarracks
      getModifiers (LocationTarget barracks.id) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getModifiers (LocationTarget barracks.id) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      _ <- stage Enemies.ulwarth
      getModifiers (LocationTarget barracks.id) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      threat <- threatOf pid
      run $ CheckWindows [mkAfter RoundEnds]
      threatOf pid `shouldReturn` (threat + 3)

    it "is not defeated until the players control all 3 Captives, then advances to Lead the Revolt" . sorrowTest $ \pid -> do
      setUp pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 15
      activeQuestCode `shouldReturn` toCardCode Quests.slavesOfTheDarkLord
      [c1, c2, c3] <- camps
      explore c1
      explore c2
      activeQuestCode `shouldReturn` toCardCode Quests.slavesOfTheDarkLord
      explore c3
      activeQuestCode `shouldReturn` toCardCode Quests.leadTheRevolt

  -- \* The Camps

  describe "the Camps" do
    it "adds the Warden under The Barracks to the staging area, undamageable this round, as the players travel there, and frees its Captive as it is explored" . sorrowTest $ \pid -> do
      setUp pid
      barracks <- camp Locations.theBarracks
      [warden] <- pure $ map toCardCode barracks.underneath
      [held] <- guardedBy barracks
      run $ TravelTo barracks.id
      enemyCodes `shouldReturn` [warden]
      eid <- enemyOf (toCardDef warden)
      placementOfEnemy eid `shouldReturn` StagingArea
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem CannotTakeDamage)
      (.underneath) <$> camp Locations.theBarracks `shouldReturn` []
      storehouse <- camp Locations.theStorehouse
      getModifiers (LocationTarget storehouse.id) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ PlaceTokens GameSource (LocationTarget barracks.id) Progress 5
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode Locations.theBarracks))
      ally <- captive (toCardDef held)
      ally.placement `shouldBe` InPlayArea pid
      allyExhausted ally `shouldBe` False

    it "has the Warden under Slave Quarters engage the first player and attack as the players travel there" . sorrowTest $ \pid -> do
      setUp pid
      quarters <- camp Locations.slaveQuarters
      [warden] <- pure $ map toCardCode quarters.underneath
      run $ TravelTo quarters.id
      eid <- enemyOf (toCardDef warden)
      placementOfEnemy eid `shouldReturn` EngagedWith pid
      passActions pid
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.declareDefender" `isPrefixOf` l; _ -> False)

    it "adds the Warden under The Storehouse to the staging area with +3 attack and -30 engagement cost as the players travel there" . sorrowTest $ \pid -> do
      setUp pid
      storehouse <- camp Locations.theStorehouse
      [warden] <- pure $ map toCardCode storehouse.underneath
      run $ TravelTo storehouse.id
      eid <- enemyOf (toCardDef warden)
      placementOfEnemy eid `shouldReturn` StagingArea
      let def = toCardDef warden
      getEnemyAttack eid `shouldReturn` (fromJustNote "attack" (cdAttack def) + 3)
      -- an engagement cost goes no lower than 0
      getEngagementCost eid `shouldReturn` max 0 (fromJustNote "engagement" (cdEngagementCost def) - 30)

  -- \* The Wardens

  describe "the Wardens" do
    it "keeps allies from defending against Ulwarth" . sorrowTest $ \pid -> do
      ulwarth <- engaged pid Enemies.ulwarth
      getModifiers (EnemyTarget ulwarth) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, OnlyEngagedPlayerFights, NotFoughtByAllies])
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      beravor <- hero "01012"
      _ <- setEncounterDeck [Locations.workingFields]
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      offers pid beravor `shouldReturn` True
      offers pid guard' `shouldReturn` False

    it "raises each player's threat by 1 after Lorgan attacks, by 2 if the attack destroys a character" . sorrowTest $ \pid -> do
      lorgan <- engaged pid Enemies.lorgan
      getModifiers (EnemyTarget lorgan) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, OnlyEngagedPlayerFights])
      beravor <- hero "01012"
      threat <- threatOf pid
      shadowAttack pid lorgan Locations.workingFields
      chooseTarget pid beravor
      passActions pid
      threatOf pid `shouldReturn` (threat + 1)
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ Ready beravor
      shadowAttack pid lorgan Locations.workingFields
      chooseTarget pid guard'
      passActions pid
      threatOf pid `shouldReturn` (threat + 3)

    it "has Borlach's excess damage dealt to the defending player's other characters" . sorrowTest $ \pid -> do
      borlach <- engaged pid Enemies.borlach
      getModifiers (EnemyTarget borlach) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, OnlyEngagedPlayerFights, ExcessDamageToOtherCharacters])

  -- \* The Captives

  describe "the Captives" do
    it "has Edrahil let a player draw after he quests successfully, and the players lose if he leaves play" . sorrowTest $ \pid -> do
      edrahil <- controlCaptive pid Objectives.edrahil
      getModifiers edrahil >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ CommitToQuest edrahil
      hand <- handSize pid
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      chooseAbilityOf pid edrahil
      handSize pid `shouldReturn` (hand + 1)
      run $ Discard edrahil
      getsGame (.outcome) `shouldReturn` Just Lost

    it "has Arador ready after he is declared as a defender, once a round" . sorrowTest $ \pid -> do
      arador <- controlCaptive pid Objectives.arador
      spider <- engaged pid Enemies.easterlingGuard
      shadowAttack pid spider Locations.workingFields
      chooseTarget pid arador
      chooseAbilityOf pid arador
      passActions pid
      exhausted arador `shouldReturn` False
      shadowAttack pid spider Locations.workingFields
      chooseTarget pid arador
      offersAbilityOf pid arador `shouldReturn` False

    it "has Farin deal 1 damage to an engaged enemy after he is declared as an attacker" . sorrowTest $ \pid -> do
      farin <- controlCaptive pid Objectives.farin
      ulwarth <- engaged pid Enemies.ulwarth
      guardE <- engaged pid Enemies.easterlingGuard
      inPhase CombatPhase
      run $ DeclareAttackers pid guardE
      chooseTarget pid farin
      offersAbilityOf pid farin `shouldReturn` True
      chooseAbilityOf pid farin
      chooseTarget pid (EnemyTarget ulwarth)
      getDamage (EnemyTarget ulwarth) `shouldReturn` 1

  -- \* The encounter cards

  describe "encounter cards" do
    it "has Easterling Guard assign the archery total as damage among characters, and its shadow keep the defender from readying" . sorrowTest $ \pid -> do
      _ <- stage Enemies.easterlingRaider
      beravor <- hero "01012"
      reveal [Enemies.easterlingGuard]
      -- archery 2, and the Raider's 1
      getArcheryTotal `shouldReturn` 3
      replicateM_ 3 (chooseTarget pid beravor)
      getDamage beravor `shouldReturn` 3
      raider <- enemyOf Enemies.easterlingRaider
      run $ EngageEnemy pid raider
      eowyn <- hero "01007"
      shadowAttack pid raider Enemies.easterlingGuard
      chooseTarget pid eowyn
      passActions pid
      getModifiers eowyn >>= (`shouldSatisfy` elem CannotReady)

    it "has Nurn Slaver surge while fewer Camps are staged than players, and its shadow add 1 attack for each Camp in the victory display" . sorrowTest $ \pid -> do
      reveal [Enemies.nurnSlaver, Locations.workingFields]
      locationCodes `shouldReturn` [toCardCode Locations.workingFields]
      _ <- stageLocation Locations.theBarracks
      reveal [Enemies.nurnSlaver, Locations.workingFields]
      length <$> locationCodes `shouldReturn` 2
      storehouse <- stageLocation Locations.theStorehouse
      run $ AddToVictoryDisplay (LocationTarget storehouse)
      raider <- engaged pid Enemies.easterlingRaider
      beravor <- hero "01012"
      shadowAttack pid raider Enemies.nurnSlaver
      chooseTarget pid beravor
      passActions pid
      -- its 2 attack, +1, against Beravor's 2 defense
      getDamage beravor `shouldReturn` 1

    it "keeps characters from healing while Working Fields is staged, and has each player damage a character to travel there" . sorrowTest $ \pid -> do
      fields <- stageLocation Locations.workingFields
      beravor <- hero "01012"
      getModifiers beravor >>= (`shouldSatisfy` elem CannotBeHealed)
      run $ TravelTo fields
      chooseTarget pid beravor
      getDamage beravor `shouldReturn` 1
      getModifiers beravor >>= (`shouldSatisfy` notElem CannotBeHealed)

    it "gives Watch Tower X threat and archery X while staged, X 1 more than the Captives controlled, and reveals a card to travel there" . sorrowTest $ \pid -> do
      tower <- stageLocation Locations.watchTowerTheLandOfSorrow
      getLocationThreat tower `shouldReturn` 1
      getModifiers (LocationTarget tower) >>= (`shouldSatisfy` elem (GainKeyword (Archery 1)))
      _ <- controlCaptive pid Objectives.farin
      getLocationThreat tower `shouldReturn` 2
      getArcheryTotal `shouldReturn` 2
      _ <- setEncounterDeck [Locations.workingFields]
      run $ TravelTo tower
      resolveRevealed
      locationCodes >>= (`shouldSatisfy` elem (toCardCode Locations.workingFields))
      getArcheryTotal `shouldReturn` 0

    it "attaches Overseer's Whip to the lowest engagement cost Easterling enemy, whip or not, damaging an exhausted character as it attacks" . sorrowTest $ \pid -> do
      slaver <- engaged pid Enemies.nurnSlaver
      _ <- stage Enemies.easterlingGuard
      reveal [Treacheries.overseersWhip, Locations.workingFields]
      attachedTo slaver `shouldReturn` [toCardCode Treacheries.overseersWhip]
      (getModifiers =<< treacheryOf Treacheries.overseersWhip) >>= (`shouldSatisfy` elem (GainTrait Weapon))
      locationCodes `shouldReturn` [toCardCode Locations.workingFields]
      reveal [Treacheries.overseersWhip, Locations.workingFields]
      attachedTo slaver `shouldReturn` replicate 2 (toCardCode Treacheries.overseersWhip)
      eowyn <- hero "01007"
      run $ Exhaust eowyn
      _ <- setEncounterDeck [Locations.workingFields]
      run $ DealShadowCard slaver
      inPhase CombatPhase
      run $ EnemyAttacks pid
      -- each Whip's Forced effect, Éowyn the only exhausted character
      useForcedAbilities pid
      getDamage eowyn `shouldReturn` 2

    it "has Cruelty of Mordor damage a character, then each player damages each damaged one or raises his threat for each; its shadow gives +2 defense" . sorrowTest $ \pid -> do
      beravor <- hero "01012"
      eowyn <- hero "01007"
      run $ DealDamage GameSource eowyn 1
      reveal [Treacheries.crueltyOfMordor]
      chooseTarget pid beravor
      threat <- threatOf pid
      chooseLabelStarting "$label.crueltyOfMordor.threat" pid
      threatOf pid `shouldReturn` (threat + 2)
      reveal [Treacheries.crueltyOfMordor]
      chooseTarget pid beravor
      chooseLabelStarting "$label.crueltyOfMordor.damage" pid
      getDamage beravor `shouldReturn` 3
      getDamage eowyn `shouldReturn` 2
      raider <- engaged pid Enemies.easterlingRaider
      defense <- getEnemyDefense raider
      theodred <- hero "01002"
      shadowAttack pid raider Treacheries.crueltyOfMordor
      chooseTarget pid theodred
      passActions pid
      getEnemyDefense raider `shouldReturn` (defense + 2)

    it "has Broken Captives exhaust each damaged character, or surge and raise threat by 1 with none, and its shadow give a Warden an additional attack" . sorrowTest $ \pid -> do
      beravor <- hero "01012"
      run $ DealDamage GameSource beravor 1
      threat <- threatOf pid
      reveal [Treacheries.brokenCaptives]
      exhausted beravor `shouldReturn` True
      threatOf pid `shouldReturn` threat
      reveal [Treacheries.brokenCaptives, Locations.workingFields]
      threatOf pid `shouldReturn` (threat + 1)
      locationCodes `shouldReturn` [toCardCode Locations.workingFields]
      ulwarth <- engaged pid Enemies.ulwarth
      theodred <- hero "01002"
      shadowAttack pid ulwarth Treacheries.brokenCaptives
      chooseTarget pid theodred
      passActions pid
      -- the additional attack
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.declareDefender" `isPrefixOf` l; _ -> False)

  -- \* Lead the Revolt

  describe "Lead the Revolt" do
    it "returns each Warden in the victory display to the staging area, and has each other player add the first enemy he discards" . twoPlayerTestIn theLandOfSorrow testDeck $ \p1 _ -> do
      lorgan <- genEncounterCard Enemies.lorgan
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard lorgan])
      _ <- setEncounterDeck [Locations.workingFields, Enemies.nurnSlaver, Enemies.easterlingGuard]
      q <- putQuestStage Quests.leadTheRevolt
      run $ Flip q
      resolveWhenRevealed p1
      sort <$> enemyCodes `shouldReturn` sort (map toCardCode [Enemies.lorgan, Enemies.nurnSlaver])
      victoryCodes `shouldReturn` []
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.easterlingGuard]

    it "has 10 quest points more for each player, a player not engaged with a Warden reveals a card as he commits, and it is won once no Warden is in play" . sorrowTest $ \pid -> do
      q <- putQuestStage Quests.leadTheRevolt
      run $ Flip q
      resolveWhenRevealed pid
      beravor <- hero "01012"
      _ <- setEncounterDeck [Locations.workingFields]
      run $ CommitToQuest beravor
      run $ CheckWindows [mkAfter $ CommittedToQuest beravor]
      resolveRevealed
      locationCodes `shouldReturn` [toCardCode Locations.workingFields]
      lorgan <- engaged pid Enemies.lorgan
      eowyn <- hero "01007"
      _ <- setEncounterDeck [Locations.workingFields]
      run $ CommitToQuest eowyn
      run $ CheckWindows [mkAfter $ CommittedToQuest eowyn]
      length <$> locationCodes `shouldReturn` 1
      run $ PlaceTokens GameSource q Progress 20
      getsGame (.outcome) `shouldReturn` Nothing
      run $ Destroy GameSource (EnemyTarget lorgan)
      getsGame (.outcome) `shouldReturn` Just Won
