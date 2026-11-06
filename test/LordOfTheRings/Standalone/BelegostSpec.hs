module LordOfTheRings.Standalone.BelegostSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Character (getAttack)
import LordOfTheRings.Helpers.Window (getActionAbilities)
import LordOfTheRings.Helpers.Attachment (attachmentHosts)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getEnemyAttack, getEnemyDefense, getEngagementCost, getLocationThreat)
import LordOfTheRings.Matcher (getEnemy, select, getThreat, playerCardsGuardedBy, AttachMatcher (AttachToEnemy), EnemyMatcher (AnyEnemy, EnemyEngagedWith, EnemyWithCardCode), PlayerMatcher (PlayerWithId))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Keyword qualified as Keyword
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Weapon))
import LordOfTheRings.Window (WindowType (PhaseBegins, EnemyAttacking, AdvancedToStage), mkAfter, mkWhen)
import TestImport

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "missing enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies)

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  lid <- getsGame $ toId . fromJustNote "missing location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations)
  run $ PlaceLocation lid ActiveLocation
  pure lid

claimLoot :: PlayerId -> CardDef -> GameT ()
claimLoot pid def = do
  hero <- heroOf pid "01012"
  run . (`PutObjectiveIntoPlayAttached` hero) =<< genEncounterCard def

stats :: EnemyId -> GameT [Int]
stats eid = sequence [getEnemyThreat eid, getEnemyAttack eid, getEnemyDefense eid]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame $ toId . fromJustNote "missing enemy" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

clock :: GameT ObjectiveAttrs
clock = getsGame $ toAttrs . fromJustNote "missing Stalking the Ruins" . find ((== toCardCode Objectives.stalkingTheRuins) . toCardCode) . toList . (.entities.objectives)

stageClock :: GameT ObjectiveId
stageClock = do
  run . AddToStaging =<< genEncounterCard Objectives.stalkingTheRuins
  (.id) <$> clock

questBegins :: GameT ()
questBegins = run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]

strongDefender :: PlayerId -> GameT Target
strongDefender pid = do
  h <- heroOf pid "01012"
  run $ CreateEffect $ Effect GameSource h [DefenseModifier 10] UntilEndOfRound
  pure h

defendAttack :: PlayerId -> Target -> GameT ()
defendAttack pid h = passAttackActions >> chooseTarget pid h >> passAttackActions

passAttackActions :: GameT ()
passAttackActions = do
  questions <- getsGame (Map.toList . (.question))
  for_ (headMay [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.takeAction"]) \pid -> do
    passActions pid
    passAttackActions

lootOf :: CardDef -> GameT ObjectiveAttrs
lootOf def = getsGame $ toAttrs . fromJustNote "missing Loot objective" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives)

lootAvailable :: PlayerId -> CardDef -> GameT Bool
lootAvailable pid def = do
  oid <- (.id) <$> lootOf def
  any ((== ObjectiveSource oid) . abilitySource) <$> getActionAbilities pid

useLoot :: PlayerId -> CardDef -> GameT ()
useLoot pid def = do
  oid <- (.id) <$> lootOf def
  abilities <- getActionAbilities pid
  let ab = fromJustNote "Loot action unavailable" $ find ((== ObjectiveSource oid) . abilitySource) abilities
  run $ UseAbility pid ab []

belegostLoot :: [CardDef]
belegostLoot = [Objectives.swordOfBelegost, Objectives.mapOfEredLuin, Objectives.keysOfBelegost, Objectives.bookOfAulesChildren, Objectives.dwarvenTorch, Objectives.blueMountainGem]

spec :: Spec
spec = describe "Belegost enemies" do
  describe "Loot" do
    for_ belegostLoot \def ->
      it ("claims freed " <> show (toCardCode def) <> " for a first-player hero") . twoPlayerTest testDeck $ \one two -> do
        lid <- activeLocation Locations.oldForestRoad
        card <- genEncounterCard def
        guardian <- (.cardId) . toAttrs <$> getLocation lid
        run $ PutObjectiveIntoPlayGuardedBy card guardian
        (.placement) <$> lootOf def `shouldReturn` StagingArea
        lootAvailable one def `shouldReturn` False
        run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
        h1 <- heroOf one "01012"
        h2 <- heroOf two "01012"
        question <- fromJustNote "no claim question" <$> getQuestion one
        let hosts = [t | TargetLabel t _ <- questionChoices question]
        hosts `shouldSatisfy` elem h1
        hosts `shouldSatisfy` notElem h2
        chooseTarget one h1
        (.placement) <$> lootOf def `shouldReturn` AttachedTo h1

    it "waits without looping when no first-player hero can receive an attachment" . gameTest $ \pid -> do
      for_ ["01012", "01007", "01002"] \code -> do
        h <- heroOf pid code
        run $ CreateEffect $ Effect GameSource h [CannotHaveAttachments] UntilEndOfPhase
      run . AddToStaging =<< genEncounterCard Objectives.keysOfBelegost
      getQuestion pid `shouldReturn` Nothing
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` StagingArea
      run $ EndEffects UntilEndOfPhase
      h <- heroOf pid "01012"
      chooseTarget pid h
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` AttachedTo h

    it "respects required attachment traits and restrictions on Restricted cards" . gameTest $ \pid -> do
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [CannotHaveAttachmentsWithoutTrait Weapon] UntilEndOfPhase
      for_ ["01007", "01002"] \code -> do
        other <- heroOf pid code
        run $ CreateEffect $ Effect GameSource other [CannotHaveAttachments] UntilEndOfPhase
      run . AddToStaging =<< genEncounterCard Objectives.mapOfEredLuin
      getQuestion pid `shouldReturn` Nothing
      run $ CreateEffect $ Effect GameSource h [CannotHaveRestrictedAttachments] UntilEndOfPhase
      run . AddToStaging =<< genEncounterCard Objectives.swordOfBelegost
      getQuestion pid `shouldReturn` Nothing
      (.placement) <$> lootOf Objectives.swordOfBelegost `shouldReturn` StagingArea

    it "discards Loot to the encounter pile when its hero leaves play" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      h <- heroOf pid "01012"
      run $ Destroy GameSource h
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.keysOfBelegost]
      getsGame (any ((== toCardCode Objectives.keysOfBelegost) . toCardCode) . toList . (.entities.objectives)) `shouldReturn` False

    it "only enters play through Discover when revealed from the encounter deck" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Objectives.keysOfBelegost, Enemies.forestSpider, Locations.oldForestRoad]
      run RevealEncounterCard
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.keysOfBelegost]
      select (EnemyWithCardCode $ toCardCode Enemies.forestSpider) >>= (`shouldSatisfy` notNull)
      getQuestion pid `shouldReturn` Nothing

    it "keeps discovered Loot guarded until its location is explored" . gameTest $ \pid -> do
      lid <- activeLocation Locations.moonlitPassage
      _ <- setEncounterDeck [Objectives.keysOfBelegost, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ ResolveDiscover pid lid
      chooseLabel "$label.continue" pid
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` False
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
      h <- heroOf pid "01012"
      chooseTarget pid h
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` True

  describe "Sword of Belegost" do
    it "gives the attached hero four attack even while the Sword is exhausted" . gameTest $ \pid -> do
      claimLoot pid Objectives.swordOfBelegost
      h <- heroOf pid "01012"
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      getAttack h `shouldReturn` 6
      run $ Exhaust (ObjectiveTarget sword)
      getAttack h `shouldReturn` 6
      run $ Discard GameSource (ObjectiveTarget sword)
      getAttack h `shouldReturn` 2
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.swordOfBelegost]

    it "cancels a shadow on an attack against another player without exhausting its hero" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.swordOfBelegost
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      h <- heroOf one "01012"
      eid <- stageEnemy Enemies.forestSpider
      _ <- setEncounterDeck [Enemies.namelessCaveDweller, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadow eid two []
      chooseAbilityOf one (ObjectiveTarget sword)
      getEnemyAttack eid `shouldReturn` 2
      (.exhausted) <$> lootOf Objectives.swordOfBelegost `shouldReturn` True
      case h of
        HeroTarget hid -> getsGame (maybe True (heroExhausted . toAttrs) . Map.lookup hid . (.entities.heroes)) `shouldReturn` False
        _ -> error "expected hero"

    it "cannot cancel explicitly uncancelable shadow effects" . gameTest $ \pid -> do
      claimLoot pid Objectives.swordOfBelegost
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      run $ CreateEffect $ Effect GameSource GameTarget [ShadowCannotBeCanceled] UntilEndOfPhase
      _ <- setEncounterDeck [Enemies.namelessCaveDweller, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadow eid pid []
      offersAbilityOf pid (ObjectiveTarget sword) `shouldReturn` False
      getEnemyAttack eid `shouldReturn` 4
      (.exhausted) <$> lootOf Objectives.swordOfBelegost `shouldReturn` False

    it "counts towards the restricted limit and can itself be discarded for it" . gameTest $ \pid -> do
      h <- heroOf pid "01012"
      _ <- putAttachmentOn pid Attachments.daggerOfWesternesse h
      _ <- putAttachmentOn pid Attachments.daggerOfWesternesse h
      run . AddToStaging =<< genEncounterCard Objectives.swordOfBelegost
      chooseTarget pid h
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      chooseTarget pid (ObjectiveTarget sword)
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.swordOfBelegost]

  describe "Map of Ered Luin" do
    it "reduces a non-unique location's threat until phase end" . gameTest $ \pid -> do
      claimLoot pid Objectives.mapOfEredLuin
      lid <- activeLocation Locations.oldForestRoad
      useLoot pid Objectives.mapOfEredLuin
      chooseTarget pid lid
      getLocationThreat lid `shouldReturn` 0
      (.exhausted) <$> lootOf Objectives.mapOfEredLuin `shouldReturn` True
      lootAvailable pid Objectives.mapOfEredLuin `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      getLocationThreat lid `shouldReturn` 1

    it "has no action when only unique locations are in play" . gameTest $ \pid -> do
      claimLoot pid Objectives.mapOfEredLuin
      _ <- activeLocation Locations.withywindle
      lootAvailable pid Objectives.mapOfEredLuin `shouldReturn` False

  describe "Keys of Belegost" do
    it "adds a resource to any player's hero and only exhausts the Keys" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.keysOfBelegost
      target <- heroOf two "01012"
      useLoot one Objectives.keysOfBelegost
      chooseTarget one target
      case target of
        HeroTarget hid -> getHeroResources hid `shouldReturn` 1
        _ -> error "expected hero"
      (.exhausted) <$> lootOf Objectives.keysOfBelegost `shouldReturn` True
      lootAvailable two Objectives.keysOfBelegost `shouldReturn` False

    it "offers no action when every hero is barred from encounter resource gains" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      for_ ["01012", "01007", "01002"] \code -> do
        h <- heroOf pid code
        run $ CreateEffect $ Effect GameSource h [CannotGainResourcesFromCardEffects] UntilEndOfPhase
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` True

    it "does not treat encounter resources as a player-card effect" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [CannotGainResourcesFromPlayerCardEffects] UntilEndOfPhase
      useLoot pid Objectives.keysOfBelegost
      chooseTarget pid h
      case h of
        HeroTarget hid -> getHeroResources hid `shouldReturn` 1
        _ -> error "expected hero"

    it "excludes a hero who can gain resources only from Treasure cards" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [CannotGainResourcesFromNonTreasureCards] UntilEndOfPhase
      useLoot pid Objectives.keysOfBelegost
      question <- fromJustNote "no resource target question" <$> getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldSatisfy` notElem h

    it "follows its hero's controller when that hero changes control" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      claimLoot one Objectives.keysOfBelegost
      h <- heroOf one "01012"
      case h of
        HeroTarget hid -> run $ ChangeHeroController hid two
        _ -> error "expected hero"
      lootAvailable one Objectives.keysOfBelegost `shouldReturn` False
      lootAvailable two Objectives.keysOfBelegost `shouldReturn` True

  describe "Book of Aulë's Children" do
    it "draws a card for the chosen player" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.bookOfAulesChildren
      handSize <- length . (.hand) <$> getPlayer two
      useLoot one Objectives.bookOfAulesChildren
      chooseTarget one two
      length . (.hand) <$> getPlayer two `shouldReturn` handSize + 1
      (.exhausted) <$> lootOf Objectives.bookOfAulesChildren `shouldReturn` True

    it "has no action while its only player cannot draw from card effects" . gameTest $ \pid -> do
      claimLoot pid Objectives.bookOfAulesChildren
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCardsFromCardEffects] UntilEndOfPhase
      lootAvailable pid Objectives.bookOfAulesChildren `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      lootAvailable pid Objectives.bookOfAulesChildren `shouldReturn` True

    it "can draw through a prohibition on player-card draws" . gameTest $ \pid -> do
      claimLoot pid Objectives.bookOfAulesChildren
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCardsFromPlayerCardEffects] UntilEndOfPhase
      useLoot pid Objectives.bookOfAulesChildren
      length . (.hand) <$> getPlayer pid `shouldReturn` 1

    it "has no action when all player decks are empty" . gameTest $ \pid -> do
      claimLoot pid Objectives.bookOfAulesChildren
      run $ DiscardTopOfDeck pid 100
      lootAvailable pid Objectives.bookOfAulesChildren `shouldReturn` False

  describe "Dwarven Torch" do
    it "weakens attack and defense for the phase, including enemies immune to player cards" . gameTest $ \pid -> do
      claimLoot pid Objectives.dwarvenTorch
      eid <- stageEnemy Enemies.forestSpider
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [ImmuneToPlayerCardEffects] UntilEndOfPhase
      useLoot pid Objectives.dwarvenTorch
      chooseTarget pid eid
      getEnemyAttack eid `shouldReturn` 1
      getEnemyDefense eid `shouldReturn` 0
      run $ EndEffects UntilEndOfPhase
      getEnemyAttack eid `shouldReturn` 2
      getEnemyDefense eid `shouldReturn` 1

    it "cannot target unique enemies" . gameTest $ \pid -> do
      claimLoot pid Objectives.dwarvenTorch
      _ <- stageEnemy Enemies.naurlhug
      lootAvailable pid Objectives.dwarvenTorch `shouldReturn` False

  describe "Blue Mountain Gem" do
    it "reduces any eligible player's threat by one as an encounter effect" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.blueMountainGem
      run $ RaiseThreat two 5
      run $ CreateEffect $ Effect GameSource (PlayerTarget two) [PlayerCardThreatReductionLimit 0] UntilEndOfPhase
      useLoot one Objectives.blueMountainGem
      getThreat two `shouldReturn` 4
      (.exhausted) <$> lootOf Objectives.blueMountainGem `shouldReturn` True

    it "cannot be spent at zero threat or against a threat floor" . gameTest $ \pid -> do
      claimLoot pid Objectives.blueMountainGem
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` False
      run $ RaiseThreat pid 10
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotReduceThreatBelow 10] UntilEndOfPhase
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` True

    it "respects a spent round-wide threat reduction limit" . gameTest $ \pid -> do
      claimLoot pid Objectives.blueMountainGem
      run $ RaiseThreat pid 10
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [ThreatReductionPerRoundLimit 1, ThreatReducedThisRound 1] UntilEndOfRound
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` False

  describe "Stalking the Ruins" do
    it "counts resources, not progress, and summons the set-aside dragon on the third quest phase" . gameTest $ \_ -> do
      oid <- stageClock
      dragon <- genEncounterCard Enemies.naurlhug
      run $ SetAside dragon
      questBegins
      (.resources) <$> clock `shouldReturn` 1
      (.progress) <$> clock `shouldReturn` 0
      select (EnemyWithCardCode $ toCardCode Enemies.naurlhug) `shouldReturn` []
      questBegins
      (.resources) <$> clock `shouldReturn` 2
      questBegins
      eid <- enemyOf Enemies.naurlhug
      (.cardId) . toAttrs <$> getEnemy eid `shouldReturn` dragon.id
      getEnemyThreat eid `shouldReturn` 3
      run $ RemoveTokens GameSource (ObjectiveTarget oid) Resource 2
      getEnemyThreat eid `shouldReturn` 1

    it "returns an engaged dragon to staging at three or more resources without recreating it" . gameTest $ \pid -> do
      oid <- stageClock
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 4
      eid <- stageEnemy Enemies.naurlhug
      run $ EngageEnemy pid eid
      questBegins
      (.resources) <$> clock `shouldReturn` 5
      select (EnemyWithCardCode $ toCardCode Enemies.naurlhug) `shouldReturn` [eid]
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "clears every token and sets the dragon aside before the new stage is revealed" . gameTest $ \pid -> do
      oid <- stageClock
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 5
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Progress 2
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Damage 1
      eid <- stageEnemy Enemies.naurlhug
      dragon <- (.cardId) . toAttrs <$> getEnemy eid
      run $ EngageEnemy pid eid
      run . AdvanceToQuest =<< genEncounterCard Quests.theWickedWillow
      (.resources) <$> clock `shouldReturn` 0
      (.progress) <$> clock `shouldReturn` 0
      getDamage (ObjectiveTarget oid) `shouldReturn` 0
      select (EnemyWithCardCode $ toCardCode Enemies.naurlhug) `shouldReturn` []
      getsGame (any ((== dragon) . (.id)) . (.scenarioSetAside) . toAttrs . (.scenario)) `shouldReturn` True

    it "can summon the same physical dragon again after retreat" . gameTest $ \_ -> do
      oid <- stageClock
      eid <- stageEnemy Enemies.naurlhug
      dragon <- (.cardId) . toAttrs <$> getEnemy eid
      run $ CheckWindows [mkWhen AdvancedToStage]
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 2
      questBegins
      returning <- enemyOf Enemies.naurlhug
      (.cardId) . toAttrs <$> getEnemy returning `shouldReturn` dragon
      getsGame (filter ((== dragon) . (.id)) . (.scenarioSetAside) . toAttrs . (.scenario)) `shouldReturn` []

  describe "Naurlhûg" do
    it "has zero variable threat without Stalking the Ruins and cannot take damage" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.naurlhug
      getEnemyThreat eid `shouldReturn` 0
      getModifiers eid >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ DealDamage GameSource (EnemyTarget eid) 4
      getDamage (EnemyTarget eid) `shouldReturn` 0

    it "assigns X damage only among the defending player's characters when it attacks" . twoPlayerTest testDeck $ \one two -> do
      oid <- stageClock
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 2
      eid <- stageEnemy Enemies.naurlhug
      h <- heroOf two "01012"
      other <- heroOf one "01012"
      run $ CheckWindows [mkWhen $ EnemyAttacking eid two]
      chooseTarget two h
      chooseTarget two h
      getDamage h `shouldReturn` 2
      getDamage other `shouldReturn` 0

    it "assigns no extra damage when the clock has no resources" . gameTest $ \pid -> do
      _ <- stageClock
      eid <- stageEnemy Enemies.naurlhug
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getQuestion pid `shouldReturn` Nothing

  describe "Orc of Ered Luin" do
    it "makes no revelation attack when nobody controls Loot" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.orcOfEredLuin
      run $ WhenRevealed (EnemyTarget eid)
      getQuestion pid `shouldReturn` Nothing

    it "attacks each Loot holder once in player order, remaining in staging" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.cryptKey
      claimLoot one Objectives.oldLantern
      claimLoot two Objectives.scathasTeeth
      h1 <- strongDefender one
      h2 <- strongDefender two
      eid <- stageEnemy Enemies.orcOfEredLuin
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ WhenRevealed (EnemyTarget eid)
      passAttackActions
      getQuestion two `shouldReturn` Nothing
      defendAttack one h1
      getQuestion one `shouldReturn` Nothing
      defendAttack two h2
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion one `shouldReturn` Nothing
      getQuestion two `shouldReturn` Nothing

    it "skips players without Loot when revealing the Orc" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.cryptKey
      h <- strongDefender two
      eid <- stageEnemy Enemies.orcOfEredLuin
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ WhenRevealed (EnemyTarget eid)
      passAttackActions
      getQuestion one `shouldReturn` Nothing
      defendAttack two h
      length <$> getEncounterDeck `shouldReturn` 1

    it "moves the shadow's attacker to the next player and attacks after the original attack" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.forestSpider
      h <- strongDefender two
      [card, _, _] <- setEncounterDeck [Enemies.orcOfEredLuin, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      pushAll [ResolveShadowCard card.id eid one [], CheckWindows [mkAfter $ EnemyAttacking eid one]]
      void mainLoop
      defendAttack two h
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith two
      length <$> getEncounterDeck `shouldReturn` 1

    it "makes no next-player attack in a solo game (FAQ 1.46)" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      [card, _] <- setEncounterDeck [Enemies.orcOfEredLuin, Locations.oldForestRoad]
      run $ DealShadowCard eid
      pushAll [ResolveShadowCard card.id eid pid [], CheckWindows [mkAfter $ EnemyAttacking eid pid]]
      void mainLoop
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 1

  describe "Blue Mountain Goblin" do
    it "gains surge from any hero's Loot but not from guarded Loot" . twoPlayerTest testDeck $ \_ two -> do
      eid <- stageEnemy Enemies.blueMountainGoblin
      getModifiers eid >>= (`shouldSatisfy` notElem (GainKeyword Keyword.Surge))
      run . (`PutObjectiveIntoPlayAttached` (EnemyTarget eid)) =<< genEncounterCard Objectives.cryptKey
      getModifiers eid >>= (`shouldSatisfy` notElem (GainKeyword Keyword.Surge))
      claimLoot two Objectives.oldLantern
      getModifiers eid >>= (`shouldSatisfy` elem (GainKeyword Keyword.Surge))

    it "actually surges on revelation while Loot is held" . gameTest $ \pid -> do
      claimLoot pid Objectives.cryptKey
      _ <- setEncounterDeck [Enemies.blueMountainGoblin, Enemies.forestSpider, Locations.oldForestRoad]
      run RevealEncounterCard
      length <$> getEncounterDeck `shouldReturn` 1
      select (EnemyWithCardCode $ toCardCode Enemies.forestSpider) >>= (`shouldSatisfy` notNull)

    it "enters from shadow engaged with its Loot holder and is dealt a facedown shadow, without surging" . gameTest $ \pid -> do
      claimLoot pid Objectives.cryptKey
      eid <- stageEnemy Enemies.forestSpider
      [card, shadow, _] <- setEncounterDeck [Enemies.blueMountainGoblin, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadowCard card.id eid pid []
      goblin <- enemyOf Enemies.blueMountainGoblin
      a <- toAttrs <$> getEnemy goblin
      a.placement `shouldBe` EngagedWith pid
      map (.id) a.shadowCards `shouldBe` [shadow.id]
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion pid `shouldReturn` Nothing

    it "does not enter from shadow when only another player controls Loot" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.cryptKey
      eid <- stageEnemy Enemies.forestSpider
      [card, _] <- setEncounterDeck [Enemies.blueMountainGoblin, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadowCard card.id eid one []
      select (EnemyWithCardCode $ toCardCode Enemies.blueMountainGoblin) `shouldReturn` []
      length <$> getEncounterDeck `shouldReturn` 1

    it "opens enters-play reactions when the shared shadow-to-enemy operation puts an enemy into play" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      deckSize <- length . (.deck) <$> getPlayer pid
      [card, _] <- setEncounterDeck [Enemies.hobgoblin, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ PutShadowEnemyIntoPlay eid card.id pid
      length . (.deck) <$> getPlayer pid `shouldReturn` deckSize - 1
      length <$> playerCardsGuardedBy card.id `shouldReturn` 1

  describe "Lurker of the Depths" do
    it "is considered engaged with every player while remaining in staging" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      for_ [one, two] \pid -> do
        select (EnemyEngagedWith $ PlayerWithId pid) >>= (`shouldSatisfy` elem eid)
        ownAttackers pid eid >>= (`shouldSatisfy` notNull)
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "rejects both forced engagement and direct engaged placement" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      run $ EngageEnemy pid eid
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      run $ PlaceEnemy eid (EngagedWith pid)
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "takes at most four damage across separate hits each round and can be defeated" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      getDamage (EnemyTarget eid) `shouldReturn` 4
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 8
      getDamage (EnemyTarget eid) `shouldReturn` 8
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 8
      run $ CheckDefeated GameSource (EnemyTarget eid)
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))
        `shouldReturn` [toCardCode Enemies.lurkerOfTheDepths]

    it "prohibits attachments without preventing removal from play" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)
      attachmentHosts (AttachToEnemy AnyEnemy) >>= (`shouldSatisfy` notElem (EnemyTarget eid))
      run $ Discard GameSource (EnemyTarget eid)
      getsGame (any ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` False

  describe "Nameless Cave Dweller" do
    it "has one threat, attack and defense without an active Discover location" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      stats eid `shouldReturn` [1, 1, 1]
      _ <- activeLocation Locations.oldForestRoad
      stats eid `shouldReturn` [1, 1, 1]
      getModifiers eid >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)
      attachmentHosts (AttachToEnemy AnyEnemy) >>= (`shouldSatisfy` notElem (EnemyTarget eid))

    it "reads the active location's modified Discover value dynamically" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      lid <- activeLocation Locations.hauntedHall
      stats eid `shouldReturn` [5, 5, 5]
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [DiscoverValueModifier 2] UntilEndOfPhase
      stats eid `shouldReturn` [7, 7, 7]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfPhase
      stats eid `shouldReturn` [1, 1, 1]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]
      run $ PlaceLocation lid StagingArea
      stats eid `shouldReturn` [1, 1, 1]

    it "loses its variable stats while its text is blank" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      _ <- activeLocation Locations.hauntedHall
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfPhase
      stats eid `shouldReturn` [0, 0, 0]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]

    for_ [False, True] \hasLoot ->
      it ("gives its shadow +" <> if hasLoot then "2 when the defender controls Loot" else "1 without Loot") . gameTest $ \pid -> do
        when hasLoot $ claimLoot pid Objectives.cryptKey
        eid <- stageEnemy Enemies.namelessCaveDweller
        cid <- getsGame $ (.cardId) . toAttrs . fromJustNote "missing dweller" . find ((== eid) . toId) . toList . (.entities.enemies)
        run $ ResolveShadowEffect cid eid pid []
        getEnemyAttack eid `shouldReturn` if hasLoot then 3 else 2
        run $ EndEffects (UntilEnemyAttackEnds eid)
        getEnemyAttack eid `shouldReturn` 1

    it "does not count another player's Loot for its shadow bonus" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.cryptKey
      eid <- stageEnemy Enemies.namelessCaveDweller
      cid <- (.cardId) . toAttrs <$> getEnemy eid
      run $ ResolveShadowEffect cid eid one []
      getEnemyAttack eid `shouldReturn` 2

  describe "Old Stone Troll" do
    it "reduces engagement cost for each claimed Loot, excluding guarded Loot" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      getEngagementCost eid `shouldReturn` 50
      claimLoot pid Objectives.cryptKey
      claimLoot pid Objectives.oldLantern
      getEngagementCost eid `shouldReturn` 30
      run . (`PutObjectiveIntoPlayAttached` (EnemyTarget eid)) =<< genEncounterCard Objectives.scathasTeeth
      getEngagementCost eid `shouldReturn` 30
      getModifiers eid >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)

    it "allows only its engaged player's attackers, while permitting sentinel defense" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      other <- heroOf two "01012"
      run $ CreateEffect $ Effect GameSource other [GainKeyword Ranged, GainKeyword Sentinel] UntilEndOfRound
      run $ EngageEnemy one eid
      ownAttackers one eid >>= (`shouldSatisfy` notNull)
      ownAttackers two eid `shouldReturn` []
      mayAttack one eid >>= (`shouldSatisfy` notElem other)
      canAttack eid other `shouldReturn` False
      canDefend eid other `shouldReturn` True
      getModifiers eid >>= (`shouldSatisfy` notElem OnlyEngagedPlayerFights)
      run $ DeclareRangedAttackers one eid [two]
      getQuestion two `shouldReturn` Nothing

    it "does not allow staging attacks before it engages a player" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      hero <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource hero [CanAttackStagingArea] UntilEndOfRound
      ownAttackers pid eid `shouldReturn` []
