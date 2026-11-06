module LordOfTheRings.Standalone.BattleOfLakeTownSpec (spec) where

import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario (theBattleOfLakeTown)
import LordOfTheRings.Scenario.Types (scenarioMeta)
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Meta (Meta (..), metaOf)
import LordOfTheRings.Window (WindowType (PhaseBegins), mkAfter)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Burn))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getBurn, getEnemy, getHero, getAlly)
import LordOfTheRings.Modifier (ModifierType (CannotBeDiscardedByEncounterCards, Blank, GainKeyword, ImmuneToPlayerCardEffects, WhenRevealedCannotBeCanceled))
import TestImport

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "location missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations)

spec :: Spec
spec = describe "Battle of Lake-town" do
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
