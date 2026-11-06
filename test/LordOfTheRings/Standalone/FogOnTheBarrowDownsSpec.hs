module LordOfTheRings.Standalone.FogOnTheBarrowDownsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Sphere (Sphere (Baggins))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getStagingThreat, getEnemyAttack)
import LordOfTheRings.Helpers.Window (getActionAbilities)
import LordOfTheRings.Location.Types (LocationAttrs (locationTokens))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Treachery.Types (TreacheryAttrs (..))
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Trait (Trait (Condition, Wight))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Matcher (getHero, getThreat, getConditionAttachments, getAlly, select, LocationMatcher (LocationWithCardCode))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Scenario.Types (encounterDiscardL)
import LordOfTheRings.Window (WindowType (EnemyAttacking, RoundEnds), mkAfter)
import TestImport

wights :: [CardDef]
wights = [Enemies.barrowWight, Enemies.coldWight, Enemies.darkWight]

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "missing Wight" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies)

restriction :: PlayerId -> CardDef -> GameT (Target, ModifierType)
restriction pid def
  | toCardCode def == toCardCode Enemies.barrowWight = pure (PlayerTarget pid, CannotReduceThreat)
  | toCardCode def == toCardCode Enemies.coldWight = pure (PlayerTarget pid, CannotDrawCardsFromCardEffects)
  | otherwise = (,CannotGainResourcesFromCardEffects) <$> heroOf pid "01012"

passAttackActions :: GameT ()
passAttackActions = do
  questions <- getsGame (Map.toList . (.question))
  for_ (headMay [pid | (pid, QuestionLabel "$label.takeAction" _ _ _) <- questions]) \pid -> do
    passActions pid
    passAttackActions

defendAttack :: PlayerId -> Target -> GameT ()
defendAttack pid target = passAttackActions >> chooseTarget pid target >> passAttackActions

prepareVictimController :: PlayerId -> GameT ()
prepareVictimController pid = do
  run $ RaiseThreat pid 10
  _ <- putCardIntoHand pid Allies.guardOfTheCitadel
  for_ ["01012", "01007", "01002"] \code -> do
    h <- heroOf pid code
    run $ PlaceTokens GameSource h Resource 3

resourcesOf :: PlayerId -> GameT [Int]
resourcesOf pid = for ["01012", "01007", "01002"] \code -> do
  target <- heroOf pid code
  case target of
    HeroTarget hid -> (.resources) . toAttrs <$> getHero hid
    _ -> error "not a hero"

checkVictimEffect :: CardDef -> PlayerId -> GameT ()
checkVictimEffect def pid = do
  getThreat pid `shouldReturn` if toCardCode def == toCardCode Enemies.barrowWight then 12 else 10
  length . (.hand) <$> getPlayer pid `shouldReturn` if toCardCode def == toCardCode Enemies.coldWight then 0 else 1
  resourcesOf pid `shouldReturn` if toCardCode def == toCardCode Enemies.darkWight then [0, 0, 0] else [3, 3, 3]

checkUnaffected :: PlayerId -> GameT ()
checkUnaffected pid = do
  getThreat pid `shouldReturn` 10
  length . (.hand) <$> getPlayer pid `shouldReturn` 1
  resourcesOf pid `shouldReturn` [3, 3, 3]

revealSpell :: PlayerId -> CardDef -> GameT ()
revealSpell pid def = do
  _ <- setEncounterDeck [def, Locations.oldForestRoad, Locations.oldForestRoad]
  pushAll [SetRevealer (Just pid), RevealEncounterCard, SetRevealer Nothing]
  void mainLoop
  resolveRevealed

songs :: GameT [TreacheryAttrs]
songs = getsGame $ map toAttrs . filter ((== toCardCode Treacheries.dreadfulSong) . toCardCode) . toList . (.entities.treacheries)

spec :: Spec
spec = describe "Fog on the Barrow-downs Wights" do
  describe "Chill Fog" do
    it "empties hero and ally pools and raises each player's threat only for their own resources" . twoPlayerTest testDeck $ \one two -> do
      firstHero <- heroOf one "01012"
      secondHero <- heroOf two "01012"
      run $ PlaceTokens GameSource firstHero Resource 2
      run $ PlaceTokens GameSource secondHero Resource 5
      tree <- putAllyIntoPlay one Allies.treebeard
      run $ PlaceTokens GameSource tree Resource 3
      revealSpell two Treacheries.chillFog
      resourcesOf one `shouldReturn` [0, 0, 0]
      resourcesOf two `shouldReturn` [0, 0, 0]
      for_ [one, two] \pid -> getThreat pid `shouldReturn` 5
      case tree of
        AllyTarget aid -> (.resources) <$> getAlly aid `shouldReturn` 0
        _ -> error "Treebeard is not an ally"
      length <$> getEncounterDeck `shouldReturn` 2

    it "also discards resources on controlled attachments" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      attachment <- putAttachmentOn pid Attachments.stewardOfGondor hero
      run $ PlaceTokens GameSource attachment Resource 2
      revealSpell pid Treacheries.chillFog
      getThreat pid `shouldReturn` 2
      -- With only the attachment's pool left empty, a second copy surges.
      revealSpell pid Treacheries.chillFog
      length <$> getEncounterDeck `shouldReturn` 1

    it "discards uncontrolled encounter resources without charging them to a player" . twoPlayerTest testDeck $ \one two -> do
      card <- genEncounterCard Locations.oldForestRoad
      run $ AddToStaging card
      location <- getsGame $ toTarget . fromJustNote "missing road" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations)
      run $ PlaceTokens GameSource location Resource 4
      revealSpell one Treacheries.chillFog
      for_ [one, two] \pid -> getThreat pid `shouldReturn` 0
      length <$> getEncounterDeck `shouldReturn` 2
      -- The encounter pool no longer prevents the next Chill Fog from surging.
      revealSpell one Treacheries.chillFog
      length <$> getEncounterDeck `shouldReturn` 1

    it "surges with no resources, leaving time counters and villagers untouched" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      run $ PlaceTokens GameSource hero TimeCounter 2
      run $ PlaceTokens GameSource hero Villager 3
      revealSpell pid Treacheries.chillFog
      resourcesOf pid `shouldReturn` [0, 0, 0]
      case hero of
        HeroTarget hid -> do
          attrs <- toAttrs <$> getHero hid
          countTokens TimeCounter attrs.heroTokens `shouldBe` 2
          countTokens Villager attrs.heroTokens `shouldBe` 3
        _ -> error "not a hero"
      getThreat pid `shouldReturn` 0
      length <$> getEncounterDeck `shouldReturn` 1

    it "does not discard immune resources or count them against surge" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      run $ PlaceTokens GameSource hero Resource 4
      run $ CreateEffect $ Effect GameSource hero [ImmuneToCardEffects] UntilEndOfRound
      revealSpell pid Treacheries.chillFog
      resourcesOf pid `shouldReturn` [4, 0, 0]
      getThreat pid `shouldReturn` 0
      length <$> getEncounterDeck `shouldReturn` 1

    it "leaves resources on cards considered out of play" . gameTest $ \pid -> do
      tree <- putAllyIntoPlay pid Allies.treebeard
      run $ PlaceTokens GameSource tree Resource 2
      run $ CreateEffect $ Effect GameSource tree [ConsideredOutOfPlay] UntilEndOfRound
      revealSpell pid Treacheries.chillFog
      case tree of
        AllyTarget aid -> (.resources) <$> getAlly aid `shouldReturn` 2
        _ -> error "Treebeard is not an ally"
      getThreat pid `shouldReturn` 0
      length <$> getEncounterDeck `shouldReturn` 1

    it "charges a hero's current controller after control changes" . twoPlayerTest testDeck $ \one two -> do
      hero <- heroOf one "01012"
      run $ PlaceTokens GameSource hero Resource 2
      case hero of
        HeroTarget hid -> run $ ChangeHeroController hid two
        _ -> error "not a hero"
      revealSpell one Treacheries.chillFog
      getThreat one `shouldReturn` 0
      getThreat two `shouldReturn` 2

    it "finishes all removals before resolving resource-discard passives" . gameTest $ \pid -> do
      card <- genEncounterCard Locations.stonewainValley
      run $ AddToStaging card
      location <- getsGame $ toId . fromJustNote "missing valley" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations)
      run $ PlaceTokens GameSource (LocationTarget location) Resource 4
      hero <- heroOf pid "01012"
      run $ PlaceTokens GameSource hero Resource 2
      revealSpell pid Treacheries.chillFog
      -- Stonewain Valley collects the two newly discarded hero resources after
      -- losing its original four, rather than losing those new resources too.
      countTokens Resource . (.locationTokens) . toAttrs <$> getLocation location `shouldReturn` 2
      getThreat pid `shouldReturn` 2

    it "keeps Baggins-resource discard windows and their Forced effects" . twoPlayerTest testDeck $ \one two -> do
      run . AddToStaging =<< genEncounterCard Locations.pineSlopes
      firstHero <- heroOf one "01012"
      secondHero <- heroOf two "01012"
      run $ CreateEffect $ Effect GameSource firstHero [GainResourceIcon Baggins] UntilEndOfRound
      run $ PlaceTokens GameSource firstHero Resource 2
      run $ PlaceTokens GameSource secondHero Resource 3
      revealSpell one Treacheries.chillFog
      -- Pine Slopes raises both players twice, in addition to their own pools.
      getThreat one `shouldReturn` 4
      getThreat two `shouldReturn` 5
      resourcesOf one `shouldReturn` [0, 0, 0]
      resourcesOf two `shouldReturn` [0, 0, 0]

    for_ [False, True] \prevented ->
      it ("raises shadow threat for damage actually dealt, with prevention = " <> show prevented) . gameTest $ \pid -> do
        eid <- stageEnemy Enemies.darkWight
        run $ EngageEnemy pid eid
        hero <- heroOf pid "01012"
        when prevented $ run $ CreateEffect $ Effect GameSource hero [CancelNextDamage 1] UntilEndOfRound
        _ <- setEncounterDeck [Treacheries.chillFog]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid hero
        getDamage hero `shouldReturn` if prevented then 0 else 1
        getThreat pid `shouldReturn` if prevented then 0 else 1

    it "charges the attacked player for damage to another player's Sentinel, including excess damage" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.darkWight
      run $ EngageEnemy one eid
      defender <- putAllyIntoPlay two Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource defender [GainKeyword Sentinel] UntilEndOfRound
      _ <- setEncounterDeck [Treacheries.chillFog]
      run $ DealShadowCard eid
      run $ EnemyAttack one eid
      passAttackActions
      chooseLabel "$label.takeAttackUndefended" one
      chooseTarget two defender
      passAttackActions
      getThreat one `shouldReturn` 3
      getThreat two `shouldReturn` 0

    it "counts partially prevented damage rather than the attack's full strength" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.darkWight
      run $ EngageEnemy pid eid
      hero <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [AttackModifier 2] UntilEndOfRound
      run $ CreateEffect $ Effect GameSource hero [CancelNextDamage 1] UntilEndOfRound
      _ <- setEncounterDeck [Treacheries.chillFog]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid hero
      getDamage hero `shouldReturn` 2
      getThreat pid `shouldReturn` 2

  describe "I'm Waiting for You!" do
    it "surges when no engaged Wight can attack, ignoring staged Wights and engaged non-Wights" . gameTest $ \pid -> do
      _ <- stageEnemy Enemies.barrowWight
      nonWight <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid nonWight
      revealSpell pid Treacheries.imWaitingForYou
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion pid `shouldReturn` Nothing

    it "surges when every engaged Wight is prevented from attacking" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.coldWight
      run $ EngageEnemy pid eid
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
      revealSpell pid Treacheries.imWaitingForYou
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion pid `shouldReturn` Nothing

    it "surges if an engaged Wight cancels its attack" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.cavernWarg
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [GainTrait Wight] UntilEndOfRound
      run $ EngageEnemy pid eid
      hero <- heroOf pid "01012"
      torch <- genEncounterCard Objectives.caveTorchDarkOfMirkwood
      run $ PutObjectiveIntoPlayAttached torch hero
      -- One shadow, one Cave Torch discard, then the surge revelation.
      _ <- setEncounterDeck [Treacheries.imWaitingForYou, Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseAbilityOf pid eid
      passAttackActions
      getQuestion pid `shouldReturn` Nothing
      length <$> select (LocationWithCardCode $ toCardCode Locations.oldForestRoad) `shouldReturn` 1

    it "does not surge if one of several Wights completes an attack" . gameTest $ \pid -> do
      blocked <- stageEnemy Enemies.coldWight
      attacker <- stageEnemy Enemies.darkWight
      for_ [blocked, attacker] $ run . EngageEnemy pid
      run $ CreateEffect $ Effect GameSource (EnemyTarget blocked) [CannotAttackPlayer pid] UntilEndOfRound
      hero <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource hero [DefenseModifier 20] UntilEndOfRound
      revealSpell pid Treacheries.imWaitingForYou
      defendAttack pid hero
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion pid `shouldReturn` Nothing

    it "attacks each engaged player in turn and does not surge after a completed attack" . twoPlayerTest testDeck $ \one two -> do
      for_ [(one, Enemies.coldWight), (two, Enemies.darkWight)] \(pid, def) -> do
        eid <- stageEnemy def
        run $ EngageEnemy pid eid
        hero <- heroOf pid "01012"
        run $ CreateEffect $ Effect GameSource hero [DefenseModifier 20] UntilEndOfRound
      revealSpell one Treacheries.imWaitingForYou
      firstHero <- heroOf one "01012"
      defendAttack one firstHero
      secondHero <- heroOf two "01012"
      defendAttack two secondHero
      -- One shadow for each attack; no third revelation from surge.
      getEncounterDeck `shouldReturn` []
      for_ [one, two] \pid -> getQuestion pid `shouldReturn` Nothing
      length <$> select (LocationWithCardCode $ toCardCode Locations.oldForestRoad) `shouldReturn` 0

    it "discards shadows from an earlier attack before dealing its own shadow" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.darkWight
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Treacheries.frozenByFear]
      run $ DealShadowCard eid
      hero <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource hero [DefenseModifier 20] UntilEndOfRound
      revealSpell pid Treacheries.imWaitingForYou
      defendAttack pid hero
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 1
      getDamage (EnemyTarget eid) `shouldReturn` 1

    for_ [False, True] \allyDefends ->
      it ("adds the printed shadow attack bonus with ally defender = " <> show allyDefends) . gameTest $ \pid -> do
        eid <- stageEnemy Enemies.darkWight
        defender <- if allyDefends then putAllyIntoPlay pid Allies.guardOfTheCitadel else heroOf pid "01012"
        [card] <- setEncounterDeck [Treacheries.imWaitingForYou]
        run $ DealShadowCard eid
        run $ ResolveShadowCard card.id eid pid [defender]
        getEnemyAttack eid `shouldReturn` (if allyDefends then 6 else 4)

  describe "Frozen by Fear" do
    it "resolves doomed and surge, suppresses every player's actions, and expires at round end" . twoPlayerTest testDeck $ \one two -> do
      getActionAbilities one >>= (`shouldSatisfy` notNull)
      revealSpell one Treacheries.frozenByFear
      for_ [one, two] \pid -> do
        getThreat pid `shouldReturn` 1
        getActionAbilities pid `shouldReturn` []
      length <$> getEncounterDeck `shouldReturn` 1
      run $ EndEffects UntilEndOfRound
      getActionAbilities one >>= (`shouldSatisfy` notNull)

    it "suppresses responses without suppressing Forced encounter effects" . gameTest $ \pid -> do
      revealSpell pid Treacheries.frozenByFear
      _ <- putCardIntoHand pid Events.aTestOfWill
      h <- heroOf pid "01007"
      run $ PlaceTokens GameSource h Resource 1
      run $ Exhaust h
      _ <- setEncounterDeck [Treacheries.theNecromancersReach, Locations.oldForestRoad]
      run RevealEncounterCard
      q <- fromJustNote "missing when-revealed confirmation" <$> getQuestion pid
      case q of
        QuestionLabel label _ _ _ -> label `shouldSatisfy` ("$label.resolveWhenRevealed" `isPrefixOf`)
        _ -> error "unexpected response question"
      resolveRevealed
      getDamage h `shouldReturn` 1
      eid <- stageEnemy Enemies.coldWight
      run $ EngageEnemy pid eid
      ally <- putAllyIntoPlay pid Allies.snowbournScout
      run $ EnemyAttack pid eid
      defendAttack pid ally
      length . (.hand) <$> getPlayer pid `shouldReturn` 0

    it "protects its shadow's attacker for the entire round, even after the shadow leaves" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.barrowWight
      [card, _] <- setEncounterDeck [Treacheries.frozenByFear, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadowCard card.id eid pid []
      getThreat pid `shouldReturn` 0
      run $ EndEffects (UntilEnemyAttackEnds eid)
      run $ DiscardShadowCards eid
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 2
      getDamage (EnemyTarget eid) `shouldReturn` 0
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 2
      getDamage (EnemyTarget eid) `shouldReturn` 2

  describe "Long Groping Arm" do
    it "counts only the revealing player's engaged Wights and follows later engagement changes" . twoPlayerTest testDeck $ \one two -> do
      a <- stageEnemy Enemies.barrowWight
      b <- stageEnemy Enemies.coldWight
      run $ EngageEnemy one a
      run $ EngageEnemy two b
      revealSpell two Treacheries.longGropingArm
      getStagingThreat `shouldReturn` 2
      run $ EngageEnemy two a
      getStagingThreat `shouldReturn` 5
      run $ EngageEnemy one b
      getStagingThreat `shouldReturn` 3
      run $ CreateEffect $ Effect GameSource (EnemyTarget a) [ThreatModifier 2] UntilEndOfPhase
      getStagingThreat `shouldReturn` 5
      run $ EndEffects UntilEndOfPhase
      getStagingThreat `shouldReturn` 0

    it "does not search when the revealing player is engaged with a non-Wight" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      revealSpell pid Treacheries.longGropingArm
      getStagingThreat `shouldReturn` 0
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 2

    it "finds a Wight in the deck and engages the revealing player, including its threat for the phase" . twoPlayerTest testDeck $ \one two -> do
      [_, card, _] <- setEncounterDeck [Treacheries.longGropingArm, Enemies.darkWight, Locations.oldForestRoad]
      pushAll [SetRevealer (Just two), RevealEncounterCard, SetRevealer Nothing]
      void mainLoop
      resolveRevealed
      chooseTarget two (CardIdTarget card.id)
      enemies <- getsGame (toList . (.entities.enemies))
      map ((.placement) . toAttrs) enemies `shouldBe` [EngagedWith two]
      getStagingThreat `shouldReturn` 2
      getQuestion one `shouldReturn` Nothing

    it "has no further effect when there is no Wight in the encounter deck or discard" . gameTest $ \pid -> do
      revealSpell pid Treacheries.longGropingArm
      chooseLabel "$label.continue" pid
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 2

    it "can take the selected Wight from the discard pile without revealing it" . gameTest $ \pid -> do
      card <- genEncounterCard Enemies.coldWight
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [card])
      revealSpell pid Treacheries.longGropingArm
      chooseTarget pid (CardIdTarget card.id)
      enemies <- getsGame (toList . (.entities.enemies))
      map ((.cardId) . toAttrs) enemies `shouldBe` [card.id]
      map ((.placement) . toAttrs) enemies `shouldBe` [EngagedWith pid]
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Treacheries.longGropingArm]
      getStagingThreat `shouldReturn` 2

  describe "Dreadful Song" do
    it "attaches only to the revealing player's committed hero, counts as a Condition, and fixes willpower at zero" . twoPlayerTest testDeck $ \one two -> do
      h1 <- heroOf one "01007"
      h2 <- heroOf two "01007"
      for_ [h1, h2] \h -> run $ CommitToQuest h
      run $ CreateEffect $ Effect GameSource h2 [WillpowerModifier 8] UntilEndOfRound
      revealSpell two Treacheries.dreadfulSong
      attached <- songs
      map (.placement) attached `shouldBe` [AttachedTo h2]
      getWillpower h1 `shouldReturn` 4
      getWillpower h2 `shouldReturn` 0
      for_ attached \song -> do
        getModifiers song.id >>= (`shouldSatisfy` elem (GainTrait Condition))
        getConditionAttachments >>= (`shouldSatisfy` elem (TreacheryTarget song.id))
        run $ Discard GameSource (TreacheryTarget song.id)
      getWillpower h2 `shouldReturn` 12

    it "discards without attaching when only another player has a committed hero" . twoPlayerTest testDeck $ \one two -> do
      run . CommitToQuest =<< heroOf one "01007"
      revealSpell two Treacheries.dreadfulSong
      songs `shouldReturn` []
      getQuestion two `shouldReturn` Nothing

    it "respects limit one per hero while allowing another committed hero" . gameTest $ \pid -> do
      h1 <- heroOf pid "01007"
      h2 <- heroOf pid "01012"
      run $ CommitToQuest h1
      revealSpell pid Treacheries.dreadfulSong
      revealSpell pid Treacheries.dreadfulSong
      length <$> songs `shouldReturn` 1
      run $ CommitToQuest h2
      revealSpell pid Treacheries.dreadfulSong
      attached <- songs
      length attached `shouldBe` 2
      map (.placement) attached `shouldSatisfy` elem (AttachedTo h2)

    it "respects a hero's prohibition on treachery attachments" . gameTest $ \pid -> do
      h <- heroOf pid "01007"
      run $ CommitToQuest h
      run $ CreateEffect $ Effect GameSource h [CannotHaveTreacheryAttachments] UntilEndOfRound
      revealSpell pid Treacheries.dreadfulSong
      songs `shouldReturn` []
      getWillpower h `shouldReturn` 4

    it "raises its host's controller's threat for that controller's Wights, even after control changes" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      h <- heroOf one "01007"
      run $ CommitToQuest h
      revealSpell one Treacheries.dreadfulSong
      a <- stageEnemy Enemies.barrowWight
      b <- stageEnemy Enemies.coldWight
      c <- stageEnemy Enemies.darkWight
      run $ EngageEnemy one a
      for_ [b, c] \eid -> run $ EngageEnemy two eid
      run $ CheckWindows [mkAfter RoundEnds]
      getThreat one `shouldReturn` 1
      getThreat two `shouldReturn` 0
      case h of
        HeroTarget hid -> run $ ChangeHeroController hid two
        _ -> error "not a hero"
      run $ CheckWindows [mkAfter RoundEnds]
      getThreat one `shouldReturn` 1
      getThreat two `shouldReturn` 2

  for_ wights \def -> describe (show $ toCardCode def) do
    it "applies its restriction only to its engaged player, immediately follows movement and respects blanking" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy def
      (t1, prohibited) <- restriction one def
      (t2, _) <- restriction two def
      getModifiers t1 >>= (`shouldSatisfy` notElem prohibited)
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)
      run $ EngageEnemy one eid
      getModifiers t1 >>= (`shouldSatisfy` elem prohibited)
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)
      run $ EngageEnemy two eid
      getModifiers t1 >>= (`shouldSatisfy` notElem prohibited)
      getModifiers t2 >>= (`shouldSatisfy` elem prohibited)
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfPhase
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)
      run $ EndEffects UntilEndOfPhase
      getModifiers t2 >>= (`shouldSatisfy` elem prohibited)
      run $ PlaceEnemy eid StagingArea
      getModifiers t2 >>= (`shouldSatisfy` notElem prohibited)

    it "does not punish a player when its attack destroys no character" . gameTest $ \pid -> do
      prepareVictimController pid
      eid <- stageEnemy def
      run $ EngageEnemy pid eid
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [DefenseModifier 30] UntilEndOfRound
      run $ EnemyAttack pid eid
      defendAttack pid h
      checkUnaffected pid

    it "punishes the controller of an ally its attack destroys" . gameTest $ \pid -> do
      prepareVictimController pid
      eid <- stageEnemy def
      run $ EngageEnemy pid eid
      ally <- putAllyIntoPlay pid Allies.snowbournScout
      run $ EnemyAttack pid eid
      defendAttack pid ally
      checkVictimEffect def pid

    it "retains the controller of a destroyed hero after that hero leaves play" . gameTest $ \pid -> do
      prepareVictimController pid
      eid <- stageEnemy def
      run $ EngageEnemy pid eid
      h <- heroOf pid "01002"
      run $ PlaceTokens GameSource h Damage 3
      run $ EnemyAttack pid eid
      defendAttack pid h
      getsGame (length . (.entities.heroes)) `shouldReturn` 2
      getThreat pid `shouldReturn` if toCardCode def == toCardCode Enemies.barrowWight then 12 else 10
      length . (.hand) <$> getPlayer pid `shouldReturn` if toCardCode def == toCardCode Enemies.coldWight then 0 else 1
      remaining <- getsGame (map ((.resources) . toAttrs) . toList . (.entities.heroes))
      remaining `shouldBe` if toCardCode def == toCardCode Enemies.darkWight then [0, 0] else [3, 3]

    it "punishes a killed Sentinel's controller rather than the defending player" . twoPlayerTest testDeck $ \one two -> do
      for_ [one, two] prepareVictimController
      eid <- stageEnemy def
      run $ EngageEnemy one eid
      ally <- putAllyIntoPlay two Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource ally [GainKeyword Sentinel] UntilEndOfRound
      run $ EnemyAttack one eid
      passAttackActions
      chooseLabel "$label.takeAttackUndefended" one
      chooseTarget two ally
      passAttackActions
      checkUnaffected one
      checkVictimEffect def two

  it "Barrow-wight blocks Gandalf's threat reduction until it leaves play" . gameTest $ \pid -> do
    run $ RaiseThreat pid 10
    eid <- stageEnemy Enemies.barrowWight
    run $ EngageEnemy pid eid
    gandalf <- putAllyIntoPlay pid Allies.gandalf
    chooseAbilityOf pid gandalf
    chooseLabel "$label.gandalf.threat" pid
    getThreat pid `shouldReturn` 10
    run $ Discard GameSource (EnemyTarget eid)
    run $ UseCardAbility pid (case gandalf of AllyTarget aid -> AllySource aid; _ -> error "not an ally") 1 []
    chooseLabel "$label.gandalf.threat" pid
    getThreat pid `shouldReturn` 5

  it "Cold-wight blocks Gandalf's card-effect draw while ordinary resource-phase drawing still works" . gameTest $ \pid -> do
    eid <- stageEnemy Enemies.coldWight
    run $ EngageEnemy pid eid
    gandalf <- putAllyIntoPlay pid Allies.gandalf
    chooseAbilityOf pid gandalf
    chooseLabel "$label.gandalf.draw" pid
    length . (.hand) <$> getPlayer pid `shouldReturn` 0
    run $ DoPhase ResourcePhase
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "Dark-wight blocks resources from player and encounter card effects, while framework resources remain legal" . gameTest $ \pid -> do
    eid <- stageEnemy Enemies.darkWight
    run $ EngageEnemy pid eid
    h <- heroOf pid "01012"
    for_ [EnemySource eid, case h of HeroTarget hid -> HeroSource hid; _ -> error "not a hero"] \source ->
      run $ PlaceTokens source h Resource 2
    resourcesOf pid `shouldReturn` [0, 0, 0]
    run $ PlaceTokens GameSource h Resource 1
    resourcesOf pid `shouldReturn` [1, 0, 0]
    run $ Discard GameSource (EnemyTarget eid)
    run $ PlaceTokens (EnemySource eid) h Resource 2
    resourcesOf pid `shouldReturn` [3, 0, 0]

  it "Dark-wight neither bars nor discards resources on an ally's own resource pool" . gameTest $ \pid -> do
    prepareVictimController pid
    eid <- stageEnemy Enemies.darkWight
    run $ EngageEnemy pid eid
    treebeard <- putAllyIntoPlay pid Allies.treebeard
    run $ PlaceTokens (EnemySource eid) treebeard Resource 2
    ally <- putAllyIntoPlay pid Allies.snowbournScout
    run $ EnemyAttack pid eid
    defendAttack pid ally
    resourcesOf pid `shouldReturn` [0, 0, 0]
    getsGame ((.resources) . toAttrs . fromJustNote "Treebeard missing" . find ((== treebeard) . toTarget) . toList . (.entities.allies)) `shouldReturn` 2

  it "does not count a character killed by a separate shadow effect as an attack kill" . gameTest $ \pid -> do
    prepareVictimController pid
    eid <- stageEnemy Enemies.barrowWight
    ally <- putAllyIntoPlay pid Allies.snowbournScout
    run $ DealDamage (EnemySource eid) ally 1
    run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    checkUnaffected pid

  for_ [Enemies.barrowWight, Enemies.coldWight] \def ->
    it ("punishes each character's controller exactly once for a multi-character attack kill: " <> show (toCardCode def)) . gameTest $ \pid -> do
      prepareVictimController pid
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      eid <- stageEnemy def
      a <- putAllyIntoPlay pid Allies.snowbournScout
      b <- putAllyIntoPlay pid Allies.snowbournScout
      for_ [a, b] \target -> run $ DealDamage (EnemyAttackSource eid) target 1
      run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
      getThreat pid `shouldReturn` if toCardCode def == toCardCode Enemies.barrowWight then 14 else 10
      length . (.hand) <$> getPlayer pid `shouldReturn` if toCardCode def == toCardCode Enemies.coldWight then 1 else 3
