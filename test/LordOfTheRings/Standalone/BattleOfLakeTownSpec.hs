module LordOfTheRings.Standalone.BattleOfLakeTownSpec (spec) where

import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario (theBattleOfLakeTown)
import LordOfTheRings.Scenario.Types (scenarioMeta)
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Meta (Meta (..), metaOf)
import LordOfTheRings.Window (WindowType (PhaseBegins, Revealed), mkWhen, mkAfter)
import LordOfTheRings.Phase (Phase (QuestPhase, TravelPhase))
import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectivePlacement))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Window (travelableLocations, windowMatchesM)
import LordOfTheRings.Ability (WindowMatcher (CardRevealedWindow, CancelableTreacheryRevealedWindow, EncounterCardRevealedWindow))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Burn))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getBurn, getEnemy, getHero, getAlly, getObjective)
import LordOfTheRings.Modifier (ModifierType (CannotBeDiscardedByEncounterCards, Blank, GainKeyword, ImmuneToPlayerCardEffects, WhenRevealedCannotBeCanceled, AlliesCannotBeDeclaredDefenders, DefenseModifier, AttackModifier, DoesNotExhaustToAttack, GainResourceIcon, ProgressFromQuestingThisPhase))
import TestImport

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "location missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations)

spec :: Spec
spec = describe "Battle of Lake-town" do
  describe "Fire and Water" do
    it "caps damage on Smaug at twelve" . gameTest $ \_ -> do
      q <- putQuestStage Quests.fireAndWater
      run $ Flip q
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      run $ PlaceTokens GameSource (toTarget eid) Damage 11
      run $ PlaceTokens GameSource (toTarget eid) Damage 5
      getDamage (toTarget eid) `shouldReturn` 12

    it "splits successful questing progress between Lake-town damage and the stage" . gameTest $ \pid -> do
      town <- stageTown
      run $ PlaceTokens GameSource town Damage 8
      q <- putQuestStage Quests.fireAndWater
      run $ Flip q
      run $ PlaceProgressOnQuest 7
      chooseLabel "$label.amount n=i:3" pid
      getDamage town `shouldReturn` 5
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 4
      getModifiers q >>= (`shouldSatisfy` elem ProgressFromQuestingThisPhase)

    it "allows keeping all questing progress" . gameTest $ \pid -> do
      town <- stageTown
      run $ PlaceTokens GameSource town Damage 8
      q <- putQuestStage Quests.fireAndWater
      run $ Flip q
      run $ PlaceProgressOnQuest 7
      chooseLabel "$label.amount n=i:0" pid
      getDamage town `shouldReturn` 8
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 7

    it "allows spending all questing progress" . gameTest $ \pid -> do
      town <- stageTown
      run $ PlaceTokens GameSource town Damage 8
      q <- putQuestStage Quests.fireAndWater
      run $ Flip q
      run $ PlaceProgressOnQuest 7
      chooseLabel "$label.amount n=i:7" pid
      getDamage town `shouldReturn` 1
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0

    it "fills an active location before offering to remove damage" . gameTest $ \pid -> do
      town <- stageTown
      run $ PlaceTokens GameSource town Damage 8
      lid <- stageLocation Locations.southPier
      run $ PlaceLocation lid ActiveLocation
      q <- putQuestStage Quests.fireAndWater
      run $ Flip q
      run $ PlaceProgressOnQuest 7
      -- South Pier takes four progress, leaving three to spend.
      chooseLabel "$label.amount n=i:3" pid
      getDamage town `shouldReturn` 5
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0

    it "does not replace direct card-effect progress" . gameTest $ \pid -> do
      town <- stageTown
      run $ PlaceTokens GameSource town Damage 8
      q <- putQuestStage Quests.fireAndWater
      run $ Flip q
      run $ PlaceTokens GameSource q Progress 3
      getQuestion pid `shouldReturn` Nothing
      getDamage town `shouldReturn` 8
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 3

  describe "Terror from the Mountain" do
    it "caps Smaug at six damage on side B, including later damage effects" . gameTest $ \pid -> do
      _ <- stageTown
      q <- putQuestStage Quests.terrorFromTheMountain
      run $ Flip q
      chooseTarget pid q
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      run $ PlaceTokens GameSource (toTarget eid) Damage 5
      run $ PlaceTokens GameSource (toTarget eid) Damage 4
      run $ PlaceTokens GameSource (toTarget eid) Damage 1
      getDamage (toTarget eid) `shouldReturn` 6

    it "does not cap Smaug on side A" . gameTest $ \_ -> do
      _ <- putQuestStage Quests.terrorFromTheMountain
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      run $ PlaceTokens GameSource (toTarget eid) Damage 9
      getDamage (toTarget eid) `shouldReturn` 9

    it "deals one damage per player to Lake-town when revealed" . twoPlayerTest testDeck $ \one _ -> do
      town <- stageTown
      q <- putQuestStage Quests.terrorFromTheMountain
      run $ Flip q
      chooseTarget one q
      getDamage town `shouldReturn` 2

    it "separates all nine Smaug cards, stages Lake-town, and reveals one setup card per player" . gameTestIn theBattleOfLakeTown testDeck $ \_ -> do
      let dragons = concatMap (replicate 3) [Enemies.smaugTheMightyLakeTown, Enemies.smaugTheTerrible, Enemies.smaugTheMagnificentLakeTown]
      _ <- setEncounterDeck $ dragons <> [Objectives.lakeTown, Locations.westPier, Locations.southPier, Locations.northPier]
      q <- putQuestStage Quests.terrorFromTheMountain
      run $ PerformSetup q
      -- Resolve the setup encounter card, then side B's When Revealed.
      resolveRevealed
      g <- getGame
      let a = toAttrs g.scenario
          m = metaOf a
      length (smaugDeck m) `shouldBe` 9
      smaugDiscard m `shouldBe` []
      length a.encounterDeck `shouldBe` 2
      length g.entities.locations `shouldBe` 1
      length g.entities.objectives `shouldBe` 1
      g.entities.enemies `shouldSatisfy` null
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True
      town <- getsGame $ toTarget . fromJustNote "Lake-town missing" . headMay . toList . (.entities.objectives)
      getDamage town `shouldReturn` 1

  it "adds West Pier's Burn to each location, including itself and the active location" . gameTest $ \_ -> do
    pier <- stageLocation Locations.westPier
    wharf <- stageLocation Locations.esgarothWharf
    run $ PlaceLocation wharf ActiveLocation
    getBurn (toTarget pier) Locations.westPier `shouldReturn` 1
    getBurn (toTarget wharf) Locations.esgarothWharf `shouldReturn` 2
    run $ Discard GameSource (toTarget pier)
    getBurn (toTarget wharf) Locations.esgarothWharf `shouldReturn` 1

  it "sums gained Burn values and removes printed Burn when blank" . gameTest $ \_ -> do
    pier <- stageLocation Locations.southPier
    run $ CreateEffect $ Effect GameSource (toTarget pier) [GainKeyword (Burn 2), GainKeyword (Burn 1)] UntilEndOfRound
    getBurn (toTarget pier) Locations.southPier `shouldReturn` 5
    run $ CreateEffect $ Effect GameSource (toTarget pier) [Blank] UntilEndOfRound
    getBurn (toTarget pier) Locations.southPier `shouldReturn` 3

  it "bars travel to other locations while South Pier is in play" . gameTest $ \_ -> do
    pier <- stageLocation Locations.southPier
    other <- stageLocation Locations.westPier
    run $ TravelTo other
    (.placement) <$> getLocation other `shouldReturn` StagingArea
    run $ TravelTo pier
    (.placement) <$> getLocation pier `shouldReturn` ActiveLocation

  it "prevents canceling When Revealed effects while East Pier is in play" . gameTest $ \_ -> do
    pier <- stageLocation Locations.eastPier
    mods <- getModifiers GameTarget
    mods `shouldSatisfy` elem WhenRevealedCannotBeCanceled
    run $ PlaceLocation pier ActiveLocation
    getModifiers GameTarget `shouldReturn` mods
    run $ Discard GameSource (toTarget pier)
    mods' <- getModifiers GameTarget
    mods' `shouldSatisfy` notElem WhenRevealedCannotBeCanceled

  it "makes all locations immune to player effects while Esgaroth Wharf is in play" . gameTest $ \pid -> do
    wharf <- stageLocation Locations.esgarothWharf
    pier <- stageLocation Locations.westPier
    for_ [wharf, pier] $ \lid -> do
      mods <- getModifiers lid
      mods `shouldSatisfy` elem ImmuneToPlayerCardEffects
    beravor <- heroOf pid "01012"
    let source = case beravor of HeroTarget hid -> HeroSource hid; _ -> GameSource
    run $ PlaceTokens source (toTarget pier) Progress 1
    (.progress) <$> getLocation pier `shouldReturn` 0
    run $ Discard GameSource (toTarget wharf)
    run $ PlaceTokens source (toTarget pier) Progress 1
    (.progress) <$> getLocation pier `shouldReturn` 1

  it "puts an explored Lake-town location in the victory display" . gameTest $ \_ -> do
    pier <- stageLocation Locations.eastPier
    run $ PlaceTokens GameSource (toTarget pier) Progress 2
    cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
    map (cdVictoryPoints . toCardDef) cards `shouldBe` [Just 2]

  it "raises only the current attack by two, then makes one additional attack" . gameTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    eid <- getsGame $ toId . fromJustNote "enemy missing" . headMay . toList . (.entities.enemies)
    run $ EngageEnemy pid eid
    defender <- heroOf pid "01012"
    other <- heroOf pid "01007"
    _ <- setEncounterDeck [Locations.westPier]
    run . AttachShadowCard eid =<< genEncounterCard Locations.esgarothWharf
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    passActions pid
    getDamage defender `shouldReturn` 3
    chooseTarget pid other
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    getDamage other `shouldReturn` 2

  it "prevents all damage to Smaug while North Pier is in play, including from an active pier" . gameTest $ \_ -> do
    pier <- stageLocation Locations.northPier
    run . AddToStaging =<< genEncounterCard Enemies.smaugTheMagnificent
    eid <- getsGame $ toId . fromJustNote "Smaug missing" . headMay . toList . (.entities.enemies)
    run $ DealDamage GameSource (toTarget eid) 3
    (.damage) <$> getEnemy eid `shouldReturn` 0
    run $ PlaceLocation pier ActiveLocation
    run $ DealDamage GameSource (toTarget eid) 3
    (.damage) <$> getEnemy eid `shouldReturn` 0
    run $ Discard GameSource (toTarget pier)
    run $ DealDamage GameSource (toTarget eid) 3
    (.damage) <$> getEnemy eid `shouldReturn` 3

  describe "Burn damage" do
    it "puts all unabsorbed Burn damage on Lake-town" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      _ <- stageLocation Locations.westPier
      run $ ResolveBurnDamage GameSource
      chooseLabel "$label.lakeTown.takeBurn" pid
      getDamage town `shouldReturn` 4

    it "exhausts a character once and lets it absorb damage up to its remaining hit points" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      _ <- stageLocation Locations.westPier
      beravor <- heroOf pid "01012"
      run $ DealDamage GameSource beravor 2
      run $ ResolveBurnDamage GameSource
      chooseTarget pid beravor
      isExhausted beravor `shouldReturn` True
      getDamage beravor `shouldReturn` 2
      chooseTarget pid beravor
      question <- getQuestion pid
      fmap questionChoices question `shouldSatisfy` maybe False (all (\case TargetLabel t _ -> t /= beravor; _ -> True))
      chooseLabel "$label.lakeTown.takeBurn" pid
      getsGame (any ((== beravor) . toTarget) . toList . (.entities.heroes)) `shouldReturn` False
      getDamage town `shouldReturn` 2

    it "lets one player exhaust multiple characters to absorb all Burn damage" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      beravor <- heroOf pid "01012"
      run $ ResolveBurnDamage GameSource
      chooseTarget pid ally
      chooseTarget pid beravor
      isExhausted ally `shouldReturn` True
      isExhausted beravor `shouldReturn` True
      getDamage ally `shouldReturn` 1
      getDamage beravor `shouldReturn` 1
      getDamage town `shouldReturn` 0

    it "does not offer exhausted characters for new Burn assignments" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      beravor <- heroOf pid "01012"
      run $ Exhaust beravor
      run $ ResolveBurnDamage GameSource
      question <- getQuestion pid
      fmap questionChoices question `shouldSatisfy` maybe False (all (\case TargetLabel t _ -> t /= beravor; _ -> True))
      chooseLabel "$label.lakeTown.takeBurn" pid
      getDamage town `shouldReturn` 2

    it "has no assignment prompt when there is no Burn damage" . gameTest $ \pid -> do
      town <- stageTown
      run $ ResolveBurnDamage GameSource
      getQuestion pid `shouldReturn` Nothing
      getDamage town `shouldReturn` 0

    it "counts Burn on active locations as well as staged locations" . gameTest $ \pid -> do
      town <- stageTown
      pier <- stageLocation Locations.southPier
      run $ PlaceLocation pier ActiveLocation
      run $ ResolveBurnDamage GameSource
      chooseLabel "$label.lakeTown.takeBurn" pid
      getDamage town `shouldReturn` 2

    it "loses the game when Lake-town reaches fifty damage" . gameTest $ \_ -> do
      town <- stageTown
      run $ DealDamage GameSource town 49
      getsGame (.outcome) `shouldReturn` Nothing
      run $ DealDamage GameSource town 1
      getsGame (.outcome) `shouldReturn` Just Lost

    it "resolves Burn when Great House is revealed" . gameTest $ \pid -> do
      town <- stageTown
      _ <- setEncounterDeck [Locations.greatHouse]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.lakeTown.takeBurn" pid
      getDamage town `shouldReturn` 3

    it "lets each player absorb Burn before sending the remainder to Lake-town" . twoPlayerTest testDeck $ \one two -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      mine <- heroOf one "01012"
      theirs <- heroOf two "01012"
      run $ ResolveBurnDamage GameSource
      chooseTarget one mine
      chooseLabel "$label.lakeTown.takeBurn" one
      chooseTarget two theirs
      getDamage mine `shouldReturn` 1
      getDamage theirs `shouldReturn` 1
      getDamage town `shouldReturn` 0

  describe "other Lake-town locations" do
    it "discards cards from every player's deck equal to Lake-town's damage" . twoPlayerTest testDeck $ \one two -> do
      town <- stageTown
      run $ DealDamage GameSource town 3
      mine <- length . (.deck) <$> getPlayer one
      theirs <- length . (.deck) <$> getPlayer two
      _ <- setEncounterDeck [Locations.greatBridge]
      run RevealEncounterCard
      resolveRevealed
      length . (.deck) <$> getPlayer one `shouldReturn` mine - 3
      length . (.deck) <$> getPlayer two `shouldReturn` theirs - 3

    it "surges from Great Bridge if Lake-town has no damage" . gameTest $ \_ -> do
      _ <- stageTown
      _ <- setEncounterDeck [Locations.greatBridge, Locations.northPier]
      run RevealEncounterCard
      resolveRevealed
      locations <- getsGame $ map toCardCode . toList . (.entities.locations)
      locations `shouldSatisfy` elem (toCardCode Locations.northPier)

    it "surges from Great Bridge if no player has any cards left to discard" . gameTest $ \pid -> do
      town <- stageTown
      run $ DealDamage GameSource town 3
      n <- length . (.deck) <$> getPlayer pid
      run $ DiscardTopOfDeck pid n
      _ <- setEncounterDeck [Locations.greatBridge, Locations.northPier]
      run RevealEncounterCard
      resolveRevealed
      locations <- getsGame $ map toCardCode . toList . (.entities.locations)
      locations `shouldSatisfy` elem (toCardCode Locations.northPier)

    it "moves all damage from Smaug to Lake-town when The Master's Manor is revealed" . gameTest $ \_ -> do
      town <- stageTown
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheMagnificent
      eid <- getsGame $ toId . fromJustNote "Smaug missing" . headMay . toList . (.entities.enemies)
      run $ DealDamage GameSource (toTarget eid) 5
      _ <- setEncounterDeck [Locations.theMastersManor]
      run RevealEncounterCard
      resolveRevealed
      (.damage) <$> getEnemy eid `shouldReturn` 0
      getDamage town `shouldReturn` 5

    for_ [True, False] $ \destroys ->
      it ("The Master's Manor discards the defender's hand only if the attack destroys a character: " <> show destroys) . gameTest $ \pid -> do
        run . AddToStaging =<< genEncounterCard Enemies.forestSpider
        eid <- getsGame $ toId . fromJustNote "enemy missing" . headMay . toList . (.entities.enemies)
        run $ EngageEnemy pid eid
        defender <- if destroys then putAllyIntoPlay pid Allies.guardOfTheCitadel else heroOf pid "01012"
        hand <- (.hand) <$> getPlayer pid
        run . AttachShadowCard eid =<< genEncounterCard Locations.theMastersManor
        run $ EnemyAttack pid eid
        passActions pid
        chooseTarget pid defender
        passActions pid
        passActions pid
        (.hand) <$> getPlayer pid `shouldReturn` if destroys then [] else hand

  describe "Smaug" do
    it "resolves Burn when Smaug the Terrible is revealed" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      _ <- setEncounterDeck [Enemies.smaugTheTerrible]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.lakeTown.takeBurn" pid
      getDamage town `shouldReturn` 2

    for_ [2, 5] $ \damage ->
      it ("Smaug the Magnificent moves at most three initial damage to Lake-town: " <> show damage) . gameTest $ \_ -> do
        town <- stageTown
        card <- genEncounterCard Enemies.smaugTheMagnificentLakeTown
        run $ RevealCardWithDamage Nothing card damage
        resolveRevealed
        eid <- onlySmaug
        (.damage) <$> getEnemy eid `shouldReturn` max 0 (damage - 3)
        getDamage town `shouldReturn` min 3 damage

    for_ [Enemies.smaugTheMightyLakeTown, Enemies.smaugTheTerrible, Enemies.smaugTheMagnificentLakeTown] $ \def ->
      it ("makes Smaug immune to player effects: " <> show (toCardCode def)) . gameTest $ \pid -> do
        run . AddToStaging =<< genEncounterCard def
        eid <- onlySmaug
        beravor <- heroOf pid "01012"
        let source = case beravor of HeroTarget hid -> HeroSource hid; _ -> GameSource
        run $ DealDamage source (toTarget eid) 2
        (.damage) <$> getEnemy eid `shouldReturn` 0

    it "assigns Smaug the Magnificent's excess attack damage to Lake-town" . gameTest $ \pid -> do
      town <- stageTown
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheMagnificentLakeTown
      eid <- onlySmaug
      defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ EngageEnemy pid eid
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid defender
      passActions pid
      passActions pid
      getDamage town `shouldReturn` 6

    it "makes Smaug the Mighty attack every player, dealing a separate shadow for each attack" . twoPlayerTest testDeck $ \one two -> do
      _ <- setEncounterDeck [Enemies.smaugTheMightyLakeTown, Locations.westPier, Locations.northPier]
      run RevealEncounterCard
      resolveRevealed
      for_ [one, two] $ \pid -> do
        passActions one
        passActions two
        chooseTarget pid =<< heroOf pid "01012"
        passActions one
        passActions two
      for_ [one, two] $ \pid -> do
        defender <- heroOf pid "01012"
        getDamage defender `shouldReturn` 3
      discardPile <- getEncounterDiscard
      map toCardCode discardPile `shouldSatisfy` (\cards -> all (`elem` cards) $ map toCardCode [Locations.westPier, Locations.northPier])

  describe "Smaug deck" do
    it "offers the destination as the quest phase begins" . gameTestIn theBattleOfLakeTown testDeck $ \pid -> do
      setSmaugDeck [Enemies.smaugTheTerrible]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      question <- getQuestion pid
      fmap (\case QuestionLabel label _ _ _ -> label; _ -> "") question `shouldBe` Just "$label.lakeTown.smaugDestination"

    for_ [False, True] $ \engaged ->
      it ("reveals the new Smaug at the chosen destination: " <> show engaged) . gameTestIn theBattleOfLakeTown testDeck $ \pid -> do
        setSmaugDeck [Enemies.smaugTheTerrible]
        run $ ScenarioSpecific "lakeTown.drawSmaug" (toJSON $ if engaged then Just pid else Nothing)
        resolveRevealed
        eid <- onlySmaug
        (.placement) <$> getEnemy eid `shouldReturn` if engaged then EngagedWith pid else StagingArea

    it "transfers damage before the new Smaug's When Revealed effect and keeps the old card in its own discard" . gameTestIn theBattleOfLakeTown testDeck $ \_ -> do
      town <- stageTown
      setSmaugDeck [Enemies.smaugTheMagnificentLakeTown, Enemies.smaugTheTerrible]
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheMightyLakeTown
      old <- onlySmaug
      run $ DealDamage GameSource (toTarget old) 5
      run $ ScenarioSpecific "lakeTown.drawSmaug" Null
      resolveRevealed
      eid <- onlySmaug
      (.damage) <$> getEnemy eid `shouldReturn` 2
      getDamage town `shouldReturn` 3
      getEncounterDiscard `shouldReturn` []
      m <- getsGame $ metaOf . toAttrs . (.scenario)
      map toCardCode (smaugDiscard m) `shouldBe` [toCardCode Enemies.smaugTheMightyLakeTown]

    it "reshuffles the Smaug discard when its deck becomes empty" . gameTestIn theBattleOfLakeTown testDeck $ \_ -> do
      setSmaugDeck [Enemies.smaugTheTerrible]
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheMightyLakeTown
      run $ ScenarioSpecific "lakeTown.drawSmaug" Null
      resolveRevealed
      m <- getsGame $ metaOf . toAttrs . (.scenario)
      map toCardCode (smaugDeck m) `shouldBe` [toCardCode Enemies.smaugTheMightyLakeTown]
      smaugDiscard m `shouldBe` []
      getEncounterDiscard `shouldReturn` []

  describe "Lake-town treacheries" do
    it "deals Close to the Flame's damage to every character" . gameTest $ \pid -> do
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Treacheries.closeToTheFlame]
      run RevealEncounterCard
      resolveRevealed
      for_ ["01012", "01007", "01002"] $ \code -> do
        target <- heroOf pid code
        getDamage target `shouldReturn` 1
      getDamage ally `shouldReturn` 1

    it "lets a player sacrifice a hero to cancel Close to the Flame's shadow" . gameTest $ \pid -> do
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      hero <- heroOf pid "01012"
      resolveCardShadow pid Treacheries.closeToTheFlame
      chooseTarget pid hero
      getsGame (any ((== hero) . toTarget) . toList . (.entities.heroes)) `shouldReturn` False
      getDamage ally `shouldReturn` 0

    it "resolves Close to the Flame's shadow when nobody sacrifices a hero" . gameTest $ \pid -> do
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      resolveCardShadow pid Treacheries.closeToTheFlame
      chooseLabel "$label.lakeTown.resolveShadow" pid
      getDamage ally `shouldReturn` 1

    it "resolves Burn when Scorching Blast is revealed" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      _ <- setEncounterDeck [Treacheries.scorchingBlast]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.lakeTown.takeBurn" pid
      getDamage town `shouldReturn` 2

    it "lets a hero cancel Scorching Blast's shadow without resolving Burn" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      hero <- heroOf pid "01012"
      resolveCardShadow pid Treacheries.scorchingBlast
      chooseTarget pid hero
      getDamage town `shouldReturn` 0
      getQuestion pid `shouldReturn` Nothing

    it "asks each player about their own heroes before resolving the shadow" . twoPlayerTest testDeck $ \one two -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      hero <- heroOf two "01012"
      resolveCardShadow one Treacheries.scorchingBlast
      chooseLabel "$label.lakeTown.resolveShadow" one
      chooseTarget two hero
      getsGame (any ((== hero) . toTarget) . toList . (.entities.heroes)) `shouldReturn` False
      getDamage town `shouldReturn` 0

    it "cannot sacrifice a hero protected from encounter-card discards" . gameTest $ \pid -> do
      town <- stageTown
      _ <- stageLocation Locations.southPier
      for_ ["01012", "01007", "01002"] $ \code -> do
        target <- heroOf pid code
        run $ CreateEffect $ Effect GameSource target [CannotBeDiscardedByEncounterCards] UntilEndOfRound
      resolveCardShadow pid Treacheries.scorchingBlast
      chooseLabel "$label.lakeTown.takeBurn" pid
      getDamage town `shouldReturn` 2

  describe "additional Lake-town treacheries" do
    it "discards an ally from each player able to pay, damaging Lake-town for the others" . twoPlayerTest testDeck $ \one two -> do
      town <- stageTown
      ally <- putAllyIntoPlay one Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Treacheries.direMagnificence]
      run RevealEncounterCard
      resolveRevealed
      getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False
      getDamage town `shouldReturn` 2
      getQuestion two `shouldReturn` Nothing

    it "treats an ally protected from encounter discards as unable to pay Dire Magnificence" . gameTest $ \pid -> do
      town <- stageTown
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource ally [CannotBeDiscardedByEncounterCards] UntilEndOfRound
      _ <- setEncounterDeck [Treacheries.direMagnificence]
      run RevealEncounterCard
      resolveRevealed
      getDamage town `shouldReturn` 2
      getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` True

    it "returns a Pier from victory without moving other victory cards" . gameTest $ \_ -> do
      pier <- stageLocation Locations.westPier
      bridge <- stageLocation Locations.greatBridge
      run $ PlaceTokens GameSource (toTarget pier) Progress 2
      run $ PlaceTokens GameSource (toTarget bridge) Progress 4
      _ <- setEncounterDeck [Treacheries.catchingFire]
      run RevealEncounterCard
      resolveRevealed
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.westPier]
      victory <- getsGame $ map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)
      victory `shouldBe` [toCardCode Locations.greatBridge]

    it "surges from Catching Fire when there is no Pier to return" . gameTest $ \_ -> do
      _ <- setEncounterDeck [Treacheries.catchingFire, Locations.northPier]
      run RevealEncounterCard
      resolveRevealed
      locations <- getsGame $ map toCardCode . toList . (.entities.locations)
      locations `shouldSatisfy` elem (toCardCode Locations.northPier)

    it "prevents allies defending Smaug after Reckless in His Rage" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Treacheries.recklessInHisRage]
      run RevealEncounterCard
      resolveRevealed
      mods <- getModifiers eid
      mods `shouldSatisfy` elem AlliesCannotBeDeclaredDefenders
      run $ EnemyAttack pid eid
      passActions pid
      question <- getQuestion pid
      fmap questionChoices question `shouldSatisfy` maybe False (all (\case TargetLabel target _ -> target /= ally; _ -> True))

    for_ [True, False] $ \destroys ->
      it ("Reckless in His Rage's shadow resolves Burn only when a character dies: " <> show destroys) . gameTest $ \pid -> do
        town <- stageTown
        _ <- stageLocation Locations.southPier
        run . AddToStaging =<< genEncounterCard Enemies.forestSpider
        eid <- onlySmaug
        run $ EngageEnemy pid eid
        defender <- if destroys then putAllyIntoPlay pid Allies.guardOfTheCitadel else heroOf pid "01012"
        run . AttachShadowCard eid =<< genEncounterCard Treacheries.recklessInHisRage
        run $ EnemyAttack pid eid
        passActions pid
        chooseTarget pid defender
        passActions pid
        passActions pid
        when destroys $ chooseLabel "$label.lakeTown.takeBurn" pid
        getDamage town `shouldReturn` if destroys then 2 else 0

    it "bars allies defending other enemies only while the affected Smaug remains in play" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      smaug <- onlySmaug
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      spider <- getsGame $ toId . fromJustNote "spider missing" . find ((== toCardCode Enemies.forestSpider) . toCardCode) . toList . (.entities.enemies)
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      canDefend spider ally `shouldReturn` True
      _ <- setEncounterDeck [Treacheries.recklessInHisRage]
      run RevealEncounterCard
      resolveRevealed
      canDefend spider ally `shouldReturn` False
      run $ Discard GameSource (toTarget smaug)
      canDefend spider ally `shouldReturn` True

    it "has Dire Magnificence's shadow move Smaug to the next player and attack after destroying a character" . twoPlayerTest testDeck $ \one two -> do
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      smaug <- onlySmaug
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      spider <- getsGame $ toId . fromJustNote "spider missing" . find ((== toCardCode Enemies.forestSpider) . toCardCode) . toList . (.entities.enemies)
      run $ EngageEnemy one spider
      ally <- putAllyIntoPlay one Allies.guardOfTheCitadel
      defender <- heroOf two "01012"
      run $ CreateEffect $ Effect GameSource defender [DefenseModifier 5] UntilEndOfRound
      _ <- setEncounterDeck [Locations.westPier]
      run . AttachShadowCard spider =<< genEncounterCard Treacheries.direMagnificence
      run $ EnemyAttack one spider
      passActions one
      passActions two
      chooseTarget one ally
      replicateM_ 6 do
        passActions one
        passActions two
      chooseTarget two defender
      replicateM_ 6 do
        passActions one
        passActions two
      (.placement) <$> getEnemy smaug `shouldReturn` EngagedWith two
      isExhausted defender `shouldReturn` True

    it "reduces the defending character's defense for Catching Fire's attack" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      eid <- onlySmaug
      run $ EngageEnemy pid eid
      defender <- heroOf pid "01012"
      run . AttachShadowCard eid =<< genEncounterCard Treacheries.catchingFire
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid defender
      passActions pid
      passActions pid
      getDamage defender `shouldReturn` 3

  describe "remaining Lake-town locations" do
    it "travels to Fisherman's Dock for free when no location is active" . gameTest $ \pid -> do
      dock <- stageLocation Locations.fishermansDock
      hero <- heroOf pid "01012"
      run $ TravelTo dock
      (.placement) <$> getLocation dock `shouldReturn` ActiveLocation
      isExhausted hero `shouldReturn` False

    it "pays Fisherman's Dock's hero cost before adding a second active location" . gameTest $ \pid -> do
      bridge <- stageLocation Locations.greatBridge
      dock <- stageLocation Locations.fishermansDock
      run $ PlaceLocation bridge ActiveLocation
      hero <- heroOf pid "01012"
      available <- travelableLocations
      available `shouldSatisfy` elem dock
      run $ TravelTo dock
      (.placement) <$> getLocation dock `shouldReturn` StagingArea
      chooseTarget pid hero
      isExhausted hero `shouldReturn` True
      (.placement) <$> getLocation dock `shouldReturn` ActiveLocation
      (.placement) <$> getLocation bridge `shouldReturn` ActiveLocation
      travelableLocations `shouldReturn` []

    it "bars travel to the second active location when no hero can exhaust" . gameTest $ \pid -> do
      bridge <- stageLocation Locations.greatBridge
      dock <- stageLocation Locations.fishermansDock
      run $ PlaceLocation bridge ActiveLocation
      for_ ["01012", "01007", "01002"] $ \code -> run . Exhaust =<< heroOf pid code
      travelableLocations `shouldReturn` []
      run $ TravelTo dock
      (.placement) <$> getLocation dock `shouldReturn` StagingArea

    it "adds Fisherman's Dock to staging when its shadow resolves" . gameTest $ \pid -> do
      resolveCardShadow pid Locations.fishermansDock
      locations <- getsGame $ map toCardCode . toList . (.entities.locations)
      locations `shouldSatisfy` elem (toCardCode Locations.fishermansDock)

    it "buys Merchant's District into victory with two damage per player" . twoPlayerTest testDeck $ \one _ -> do
      town <- stageTown
      district <- stageLocation Locations.merchantsDistrict
      run $ UseCardAbility one (LocationSource district) 1 []
      getDamage town `shouldReturn` 4
      victory <- getsGame $ map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)
      victory `shouldBe` [toCardCode Locations.merchantsDistrict]

    it "offers Merchant's District's action during travel" . gameTest $ \pid -> do
      _ <- stageTown
      district <- stageLocation Locations.merchantsDistrict
      run $ BeginPhase TravelPhase
      offersAbilityOf pid district `shouldReturn` True

    for_ [True, False] $ \undefended ->
      it ("Merchant's District's shadow deals the correct damage to Lake-town: " <> show undefended) . gameTest $ \pid -> do
        town <- stageTown
        run . AddToStaging =<< genEncounterCard Enemies.forestSpider
        eid <- onlySmaug
        [card] <- setEncounterDeck [Locations.merchantsDistrict]
        run $ DealShadowCard eid
        defenders <- if undefended then pure [] else pure . pure =<< heroOf pid "01012"
        run $ ResolveShadowCard card.id eid pid defenders
        getDamage town `shouldReturn` if undefended then 4 else 1

  describe "The Old Thrush" do
    it "claims a free Thrush by exhausting the hero it attaches to" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ActionWindow pid
      chooseAbilityOf pid oid
      chooseTarget pid hero
      isExhausted hero `shouldReturn` True
      objectivePlacement . toAttrs <$> getObjective oid `shouldReturn` AttachedTo hero

    it "cannot be claimed while guarded" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Objectives.theOldThrush, Enemies.forestSpider]
      run RevealEncounterCard
      resolveRevealed
      oid <- getsGame $ toId . fromJustNote "Thrush missing" . headMay . toList . (.entities.objectives)
      run $ ActionWindow pid
      offersAbilityOf pid oid `shouldReturn` False

    it "reduces Smaug's defense by the cards actually discarded for the attack" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ClaimObjective oid hero
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      run $ EngageEnemy pid eid
      run $ CreateEffect $ Effect GameSource hero [AttackModifier 8] UntilEndOfRound
      cards <- traverse (putCardIntoHand pid) [Allies.guardOfTheCitadel, Allies.gondorianSpearman]
      _ <- putCardIntoHand pid Allies.wanderingTook
      run $ DeclareAttackers pid eid
      chooseTarget pid hero
      chooseAbilityOf pid oid
      for_ cards $ \card -> chooseTarget pid (CardIdTarget card.id)
      chooseLabel "$label.doneDiscarding" pid
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      (.damage) <$> getEnemy eid `shouldReturn` 5

    it "does not respond when its hero attacks without exhausting" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ClaimObjective oid hero
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource hero [DoesNotExhaustToAttack] UntilEndOfRound
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      run $ AttackWithAgainst hero eid
      isExhausted hero `shouldReturn` False
      offersAbilityOf pid oid `shouldReturn` False

    it "does not respond when the hero exhausts to attack an enemy other than Smaug" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ClaimObjective oid hero
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      eid <- onlySmaug
      run $ AttackWithAgainst hero eid
      isExhausted hero `shouldReturn` True
      offersAbilityOf pid oid `shouldReturn` False

    it "does not offer a defense reduction with an empty hand" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ClaimObjective oid hero
      hand <- (.hand) <$> getPlayer pid
      for_ hand $ \card -> run $ DiscardFromHand pid card.id
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      run $ AttackWithAgainst hero eid
      offersAbilityOf pid oid `shouldReturn` False

    it "discards the Thrush when its attached hero leaves play" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ClaimObjective oid hero
      run $ Discard GameSource hero
      discarded <- map toCardCode <$> getEncounterDiscard
      discarded `shouldSatisfy` elem (toCardCode Objectives.theOldThrush)

    it "discards the Thrush if it is detached into staging" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ClaimObjective oid hero
      run $ PlaceObjective oid StagingArea
      discarded <- map toCardCode <$> getEncounterDiscard
      discarded `shouldSatisfy` elem (toCardCode Objectives.theOldThrush)

    it "responds to Quick Strike's required exhaustion even if the hero normally attacks without exhausting" . gameTest $ \pid -> do
      oid <- stageThrush
      hero <- heroOf pid "01012"
      run $ ClaimObjective oid hero
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource hero [DoesNotExhaustToAttack] UntilEndOfRound
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      run $ EngageEnemy pid eid
      card <- genPlayerCard Events.quickStrike (Just pid)
      run $ PlayAtNoCost pid card
      chooseTarget pid hero
      chooseTarget pid eid
      isExhausted hero `shouldReturn` True
      offersAbilityOf pid oid `shouldReturn` True

  describe "Town-Baiting" do
    it "puts an undefended attack directly on Lake-town without assigning it to a hero" . gameTest $ \pid -> do
      town <- stageTown
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      _ <- setEncounterDeck [Treacheries.townBaiting, Locations.westPier]
      run RevealEncounterCard
      resolveRevealed
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      getDamage town `shouldReturn` 7
      for_ ["01012", "01007", "01002"] $ \code -> do
        target <- heroOf pid code
        getDamage target `shouldReturn` 0

    for_ [(Enemies.smaugTheTerrible, 5), (Enemies.smaugTheMagnificentLakeTown, 6)] $ \(smaug, excess) ->
      it ("puts defended excess on Lake-town exactly once: " <> show (toCardCode smaug)) . gameTest $ \pid -> do
        town <- stageTown
        run . AddToStaging =<< genEncounterCard smaug
        ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
        _ <- setEncounterDeck [Treacheries.townBaiting, Locations.westPier]
        run RevealEncounterCard
        resolveRevealed
        passActions pid
        chooseTarget pid ally
        passActions pid
        passActions pid
        getDamage town `shouldReturn` excess

    it "gives the last player the defender choice" . twoPlayerTest testDeck $ \one two -> do
      _ <- stageTown
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      _ <- setEncounterDeck [Treacheries.townBaiting, Locations.westPier]
      run RevealEncounterCard
      resolveRevealed
      replicateM_ 6 do
        passActions one
        passActions two
      getQuestion one `shouldReturn` Nothing
      getQuestion two >>= (`shouldSatisfy` isJust)
      hero <- heroOf two "01012"
      question <- getQuestion two
      fmap questionChoices question `shouldSatisfy` maybe False (any (\case TargetLabel target _ -> target == hero; _ -> False))

    it "does not route later ordinary attacks to the town" . gameTest $ \pid -> do
      town <- stageTown
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      eid <- onlySmaug
      _ <- setEncounterDeck [Treacheries.townBaiting, Locations.westPier]
      run RevealEncounterCard
      resolveRevealed
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      run $ EnemyAttack pid eid
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      hero <- heroOf pid "01012"
      chooseTarget pid hero
      getDamage town `shouldReturn` 7

    it "blocks When Revealed cancellation while preserving whole-card response windows" . gameTest $ \_ -> do
      _ <- setEncounterDeck [Treacheries.townBaiting]
      run RevealEncounterCard
      target <- getsGame $ toTarget . fromJustNote "treachery missing" . headMay . toList . (.entities.treacheries)
      let window = mkWhen $ Revealed target
      for_ [CardRevealedWindow, CancelableTreacheryRevealedWindow] $ \matcher ->
        windowMatchesM (matcher When) window `shouldReturn` False
      windowMatchesM (EncounterCardRevealedWindow When) window `shouldReturn` True

    it "does nothing when Smaug is absent" . gameTest $ \_ -> do
      town <- stageTown
      _ <- setEncounterDeck [Treacheries.townBaiting]
      run RevealEncounterCard
      resolveRevealed
      getDamage town `shouldReturn` 0

    it "still allows canceling a shadow card during the attack" . gameTest $ \pid -> do
      town <- stageTown
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      stroke <- putCardIntoHand pid Events.hastyStroke
      eowyn <- heroOf pid "01007"
      run $ PlaceTokens GameSource eowyn Resource 1
      _ <- setEncounterDeck [Treacheries.townBaiting, Treacheries.closeToTheFlame]
      run RevealEncounterCard
      resolveRevealed
      passActions pid
      chooseTarget pid =<< heroOf pid "01012"
      passActions pid
      chooseTarget pid (CardIdTarget stroke.id)
      passActions pid
      getDamage town `shouldReturn` 1

    it "keeps the town routing when a player makes a nested Quick Strike attack" . gameTest $ \pid -> do
      town <- stageTown
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      spider <- getsGame $ toId . fromJustNote "spider missing" . find ((== toCardCode Enemies.forestSpider) . toCardCode) . toList . (.entities.enemies)
      run $ EngageEnemy pid spider
      hero <- heroOf pid "01012"
      quick <- putCardIntoHand pid Events.quickStrike
      run $ CreateEffect $ Effect GameSource hero [GainResourceIcon Tactics] UntilEndOfRound
      run $ PlaceTokens GameSource hero Resource 1
      _ <- setEncounterDeck [Treacheries.townBaiting, Locations.westPier]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid (CardIdTarget quick.id)
      chooseTarget pid hero
      chooseTarget pid spider
      passActions pid
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      getDamage town `shouldReturn` 7

    it "does not allow another player's Sentinel to defend the objective attack" . twoPlayerTest testDeck $ \one two -> do
      town <- stageTown
      _ <- putAllyIntoPlay one Allies.gondorianSpearman
      run . AddToStaging =<< genEncounterCard Enemies.smaugTheTerrible
      _ <- setEncounterDeck [Treacheries.townBaiting, Locations.westPier]
      run RevealEncounterCard
      resolveRevealed
      replicateM_ 6 do
        passActions one
        passActions two
      chooseLabel "$label.takeAttackUndefended" two
      replicateM_ 6 do
        passActions one
        passActions two
      getQuestion one `shouldReturn` Nothing
      getDamage town `shouldReturn` 7

stageTown :: GameT Target
stageTown = do
  run . AddToStaging =<< genEncounterCard Objectives.lakeTown
  getsGame $ toTarget . fromJustNote "Lake-town missing" . headMay . toList . (.entities.objectives)

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getHero hid
  AllyTarget aid -> allyExhausted . toAttrs <$> getAlly aid
  _ -> pure False

onlySmaug :: GameT EnemyId
onlySmaug = getsGame $ toId . fromJustNote "Smaug missing" . headMay . toList . (.entities.enemies)

setSmaugDeck :: [CardDef] -> GameT ()
setSmaugDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioMeta = toJSON $ Meta cards []})

resolveCardShadow :: PlayerId -> CardDef -> GameT ()
resolveCardShadow pid def = do
  run . AddToStaging =<< genEncounterCard Enemies.forestSpider
  eid <- getsGame $ toId . fromJustNote "enemy missing" . headMay . toList . (.entities.enemies)
  [card] <- setEncounterDeck [def]
  run $ DealShadowCard eid
  run $ ResolveShadowCard card.id eid pid []

stageThrush :: GameT ObjectiveId
stageThrush = do
  run . AddToStaging =<< genEncounterCard Objectives.theOldThrush
  getsGame $ toId . fromJustNote "Thrush missing" . headMay . toList . (.entities.objectives)
