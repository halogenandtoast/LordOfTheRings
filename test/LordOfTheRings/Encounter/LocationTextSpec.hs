module LordOfTheRings.Encounter.LocationTextSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardType (AllyType, EventType), IsCard (toCard))
import LordOfTheRings.Criteria (shadowEffectsCancelable, whenRevealedCancelable)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Heading (Heading (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getStagingThreat)
import LordOfTheRings.Helpers.Window (getActionAbilities, getWindowAbilities, passesCriteria)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithType), cardIs, getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (PlanningPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (foundationsOfStone, theFateOfNumenor)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, turnHiddenCardsFaceup)
import LordOfTheRings.Scenario.Types (headingL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (QuestBypassed), mkWhen)
import TestImport

staged :: CardDef -> GameT EnemyId
staged def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== card.id) . (.id) . toCard) . toList . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  locationWithCard card.id

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  card <- genEncounterCard def
  run $ PutIntoPlayAsActiveLocation card
  locationWithCard card.id

locationWithCard :: CardId -> GameT LocationId
locationWithCard cid = getsGame (fromJustNote "no such location" . fmap toId . find ((== cid) . (.id) . toCard) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

effect :: Targetable t => t -> [ModifierType] -> GameT ()
effect t ms = run $ CreateEffect $ Effect GameSource (toTarget t) ms UntilEndOfRound

beravor :: GameT Target
beravor = toTarget <$> getHeroWithCode "01012"

eowyn :: GameT Target
eowyn = toTarget <$> getHeroWithCode "01007"

allHeroes :: GameT [Target]
allHeroes = getsGame (map toTarget . toList . (.entities.heroes))

exhausted :: Target -> GameT Bool
exhausted t = getsGame \g -> case t of
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) (Map.lookup hid g.entities.heroes)
  _ -> False

ship :: PlayerId -> CardDef -> GameT Target
ship pid def = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
  getsGame (toTarget . fromJustNote "no such ally" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

choiceTargets :: PlayerId -> GameT [Target]
choiceTargets pid = maybe [] (\q -> [t | TargetLabel t _ <- questionChoices q]) <$> getQuestion pid

fromSource :: Targetable a => a -> [Ability] -> [Ability]
fromSource (toTarget -> t) = filter ((== Just t) . sourceTarget . (.abilitySource))

spec :: Spec
spec = describe "Location card text" do
  it "Aft Cabin has each enemy grappled with a Deck location add 3 threat to it" . gameTest $ \_ -> do
    _ <- stagedLocation Locations.aftCabin
    deck <- stagedLocation Locations.quarterDeck
    spider <- staged Enemies.forestSpider
    run $ PlaceEnemy spider (AttachedTo (LocationTarget deck))
    run $ Run []
    getLocationThreat deck `shouldReturn` 7

  it "Abandoned Camp damages each exhausted character as it leaves play, explored or not" . gameTest $ \_ -> do
    camp <- stagedLocation Locations.abandonedCampNightmare
    b <- beravor
    run $ Exhaust b
    run $ Discard (LocationTarget camp)
    getDamage b `shouldReturn` 2

  it "Steep Plateau answers its being explored once it has left play" . gameTestIn theFateOfNumenor testDeck $ \pid -> do
    _ <- stagedLocation Locations.forsakenCity
    plateau <- activeLocation Locations.steepPlateau
    -- it enters Lost Island side up
    run $ Flip (LocationTarget plateau)
    run $ PlaceTokens GameSource (LocationTarget plateau) Progress 3
    getsGame (Map.member plateau . (.entities.locations)) `shouldReturn` False
    offersAbilityOf pid (LocationTarget plateau) `shouldReturn` True

  it "Captain's Quarters adds Captain Sahír's resources while he is out of play" . gameTest $ \_ -> do
    run . SetAside =<< genEncounterCard Enemies.captainSahir
    quarters <- stagedLocation Locations.captainsQuarters
    run $ TravelTo quarters
    run $ AddSetAsideToStaging (cardIs Enemies.captainSahir)
    (.resources) <$> (getEnemy =<< enemyOf Enemies.captainSahir) `shouldReturn` 2

  it "Captain's Quarters, grappled, leaves the Raiders with no resources" . gameTest $ \_ -> do
    quarters <- stagedLocation Locations.captainsQuarters
    duelist <- staged Enemies.corsairDuelist
    run $ PlaceTokens GameSource (EnemyTarget duelist) Resource 2
    getModifiers duelist >>= (`shouldSatisfy` elem (GainKeyword Indestructible))
    spider <- staged Enemies.forestSpider
    run $ PlaceEnemy spider (AttachedTo (LocationTarget quarters))
    run $ Run []
    getModifiers duelist >>= (`shouldSatisfy` notElem (GainKeyword Indestructible))

  it "City Under Siege has archery damage dealt to Ship objectives" . gameTest $ \pid -> do
    dreamChaser <- ship pid Objectives.dreamChaser
    _ <- stagedLocation Locations.cityUnderSiege
    run ResolveArchery
    choiceTargets pid `shouldReturn` [dreamChaser]

  it "Corsair Waters lets only 1 ally enter play each round, however it enters" . gameTest $ \pid -> do
    _ <- activeLocation Locations.corsairWaters
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    card <- putCardIntoHand pid Allies.snowbournScout
    run $ PutAllyIntoPlay pid card.id
    length <$> getsGame (.entities.allies) `shouldReturn` 1
    any ((== card.id) . (.id)) . (.hand) <$> getPlayer pid `shouldReturn` True

  it "Decrepit City bars cancelling When Revealed effects, not shadow effects" . gameTest $ \_ -> do
    _ <- activeLocation Locations.decrepitCity
    passesCriteria whenRevealedCancelable `shouldReturn` False
    passesCriteria shadowEffectsCancelable `shouldReturn` True

  it "Stronghold Tower bars cancelling shadow effects, not When Revealed effects" . gameTest $ \_ -> do
    _ <- stagedLocation Locations.strongholdTower
    passesCriteria shadowEffectsCancelable `shouldReturn` False
    passesCriteria whenRevealedCancelable `shouldReturn` True

  it "Deep-mines gives a resource to a Goblin put into play engaged" . gameTest $ \pid -> do
    _ <- stagedLocation Locations.deepMines
    run . PutEnemyIntoPlayEngaged pid =<< genEncounterCard Enemies.goblinScout
    (.resources) <$> (getEnemy =<< enemyOf Enemies.goblinScout) `shouldReturn` 1

  it "Forgotten Courtyard takes a resource for an Undead put into play engaged" . gameTest $ \pid -> do
    courtyard <- stagedLocation Locations.forgottenCourtyard
    run . PutEnemyIntoPlayEngaged pid =<< genEncounterCard Enemies.cavernGuardian
    getLocationThreat courtyard `shouldReturn` 2

  it "Glade of the Spawn exhausts a character for a Spider put into play engaged" . gameTest $ \pid -> do
    _ <- activeLocation Locations.gladeOfTheSpawn
    run . PutEnemyIntoPlayEngaged pid =<< genEncounterCard Enemies.forestSpider
    b <- beravor
    chooseTarget pid b
    exhausted b `shouldReturn` True

  it "Eastern Shore bars a second event only while it is in the staging area" . gameTest $ \pid -> do
    shore <- stagedLocation Locations.easternShore
    effect (PlayerTarget pid) [PlayedEventThisRound]
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (CannotPlay $ CardWithType EventType))
    run $ PlaceLocation shore ActiveLocation
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem (CannotPlay $ CardWithType EventType))

  it "Lost Island and Edge of the Temple bar only player card attachments" . gameTest $ \_ -> do
    island <- stagedLocation Locations.lostIsland
    edge <- stagedLocation Locations.edgeOfTheTemple
    for_ [island, edge] \l ->
      getModifiers (LocationTarget l) >>= (`shouldSatisfy` \ms -> CannotHavePlayerAttachments `elem` ms && CannotHaveAttachments `notElem` ms)

  it "Elven Wave-runner's draw may be declined" . gameTest $ \pid -> do
    runner <- stagedLocation Locations.elvenWaveRunner
    hand <- length . (.hand) <$> getPlayer pid
    run $ AddToVictoryDisplay (LocationTarget runner)
    chooseLabel "$label.elvenWaveRunner.decline" pid
    length . (.hand) <$> getPlayer pid `shouldReturn` hand

  it "Fiery Depths moves Durin's Bane's damage, not dealing it" . gameTest $ \pid -> do
    bane <- staged Enemies.durinsBane
    run $ PlaceTokens GameSource (EnemyTarget bane) Damage 1
    depths <- stagedLocation Locations.fieryDepths
    b <- beravor
    e <- eowyn
    effect b [DamageRedirectedTo e]
    run $ TravelTo depths
    chooseTarget pid b
    getDamage b `shouldReturn` 1
    getDamage e `shouldReturn` 0
    getDamage (EnemyTarget bane) `shouldReturn` 0

  it "Flooded Ruins adds its threat as modified while active" . gameTest $ \_ -> do
    ruins <- activeLocation Locations.floodedRuins
    run $ Flip (LocationTarget ruins)
    effect (LocationTarget ruins) [ThreatModifier 2]
    getStagingThreat `shouldReturn` 5

  it "Gladden Marshlands' damage is a cost: no hero that can take it, no action" . gameTest $ \pid -> do
    marsh <- stagedLocation Locations.gladdenMarshlands
    fromSource (LocationTarget marsh) <$> getActionAbilities pid >>= (`shouldSatisfy` notNull)
    traverse_ (`effect` [CannotTakeDamage]) =<< allHeroes
    fromSource (LocationTarget marsh) <$> getActionAbilities pid >>= (`shouldSatisfy` null)

  it "Hideous Depths shuffles the encounter deck after a player looks at it" . gameTest $ \pid -> do
    _ <- stagedLocation Locations.hideousDepths
    cards <- setEncounterDeck (replicate 20 Locations.oldForestRoad)
    run $ LookAtTopOfEncounterDeck pid GameSource False
    chooseLabel "$label.continue" pid
    map (.id) <$> getEncounterDeck >>= (`shouldSatisfy` (/= map (.id) cards))

  it "Jagged Cavern dooms no treachery once the round's first was revealed before it was active" . gameTest $ \pid -> do
    reveal Treacheries.eyesOfTheForest []
    _ <- activeLocation Locations.jaggedCavern
    was <- threat pid
    reveal Treacheries.eyesOfTheForest [Locations.oldForestRoad, Locations.oldForestRoad]
    threat pid `shouldReturn` was

  it "Mountains of Shadow takes 1 hidden card for each time hidden cards are turned faceup" . gameTest $ \pid -> do
    _ <- activeLocation Locations.mountainsOfShadow
    _ <- setEncounterDeck (replicate 5 Locations.oldForestRoad)
    run $ TakeHiddenCard pid
    run $ TakeHiddenCard pid
    runQueueT (void $ turnHiddenCardsFaceup pid)
    void mainLoop
    length <$> getHiddenCards pid `shouldReturn` 1

  it "Palace Ruins lets X be as large as the deck" . gameTest $ \pid -> do
    ruins <- stagedLocation Locations.palaceRuins
    deck <- length . (.deck) <$> getPlayer pid
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid (LocationTarget ruins)
    q <- fromJustNote "no question" <$> getQuestion pid
    length (questionChoices q) `shouldBe` deck
    deck `shouldSatisfy` (> 9)

  it "Plains of Dunland cannot be traveled to with no enemy in the staging area" . gameTest $ \_ -> do
    plains <- stagedLocation Locations.plainsOfDunland
    getModifiers (LocationTarget plains) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    _ <- staged Enemies.dunlandRaider
    getModifiers (LocationTarget plains) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

  it "Rolling Seas reads the damage the attack deals a Ship objective" . gameTest $ \pid -> do
    g <- (scenarioL %~ overAttrs (headingL ?~ Sunny)) <$> getGame
    putGame g
    putGame =<< preloadModifiers g
    dreamChaser <- ship pid Objectives.dreamChaser
    effect dreamChaser [CannotTakeDamage]
    run . PutEnemyIntoPlayEngaged pid =<< genEncounterCard Enemies.patrolShip
    sailor <- enemyOf Enemies.patrolShip
    effect (EnemyTarget sailor) [AttackModifier 10]
    _ <- setEncounterDeck [Locations.rollingSeas]
    run $ DealShadowCard sailor
    run $ EnemyAttack pid sailor
    passActions pid
    chooseTarget pid dreamChaser
    passActions pid
    getsGame ((.heading) . toAttrs . (.scenario)) `shouldReturn` Just Sunny

  it "Sacred Forest cannot be traveled to with no time counter on an enemy" . gameTest $ \_ -> do
    forest <- stagedLocation Locations.sacredForest
    getModifiers (LocationTarget forest) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  it "Sacred Forest gives its Guardians a forced ability, gone with its text" . gameTest $ \_ -> do
    forest <- stagedLocation Locations.sacredForest
    boar <- staged Enemies.boarsGuardian
    run $ PlaceTokens GameSource (EnemyTarget boar) Damage 2
    run $ PlaceTokens GameSource (EnemyTarget boar) TimeCounter 2
    effect (LocationTarget forest) [Blank]
    run $ RemoveTokens GameSource (EnemyTarget boar) TimeCounter 1
    getDamage (EnemyTarget boar) `shouldReturn` 2

  it "Shadowed Corridor's threat is a cost of the bypass, not a forced effect" . gameTest $ \_ -> do
    corridor <- stagedLocation Locations.shadowedCorridor
    getModifiers GameTarget >>= (`shouldSatisfy` elem (BypassCostRaisesThreat 2))
    fromSource (LocationTarget corridor) <$> getWindowAbilities [mkWhen $ QuestBypassed GameTarget] >>= (`shouldSatisfy` null)

  it "Shivering River adds its threat to each other area's staging area" $ do
    seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", testDeck)])
    env <- newGameEnv (newTableIn foundationsOfStone seats) (const $ pure ()) 1
    runGameT env do
      [one, two] <- getsGame (.playerOrder)
      run $ SeparatePlayers [[one], [two]]
      run $ FocusArea two
      _ <- stagedLocation Locations.shiveringRiver
      getStagingThreat `shouldReturn` 3
      run $ FocusArea one
      getStagingThreat `shouldReturn` 3

  it "Shrine to Morgoth cannot enter the staging area" . gameTest $ \_ -> do
    shrine <- activeLocation Locations.shrineToMorgothTheDrownedRuins
    run $ PlaceLocation shrine StagingArea
    (.placement) <$> getLocation shrine `shouldReturn` ActiveLocation

  it "Snowy Badlands cannot be traveled to by a player with no exhausted character" . gameTest $ \_ -> do
    badlands <- stagedLocation Locations.snowyBadlands
    getModifiers (LocationTarget badlands) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run . Exhaust =<< beravor
    getModifiers (LocationTarget badlands) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

  it "Stonewain Valley takes no more resources than the pool held" . gameTest $ \pid -> do
    valley <- stagedLocation Locations.stonewainValley
    b <- beravor
    run $ PlaceTokens GameSource b Resource 1
    run $ RemoveTokens (PlayerSource pid) b Resource 3
    getLocationThreat valley `shouldReturn` 2

  it "Twisting Hollow bars card effects readying characters in the refresh phase, not the refresh itself" . gameTest $ \_ -> do
    _ <- activeLocation Locations.twistingHollow
    b <- beravor
    run $ Exhaust b
    g <- getGame
    putGame g {gamePhase = RefreshPhase}
    run $ Ready b
    exhausted b `shouldReturn` True
    run $ BeginPhase RefreshPhase
    exhausted b `shouldReturn` False

  it "Umbar Bridge cancels only the first X progress each round" . gameTest $ \_ -> do
    _ <- stagedLocation Locations.umbarBridge
    raider <- staged Enemies.umbarRaider
    run $ PlaceTokens GameSource (EnemyTarget raider) Resource 2
    q <- putQuestStage Quests.fliesAndSpiders
    run $ PlaceTokens GameSource q Progress 1
    run $ PlaceTokens GameSource q Progress 3
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "Undersea Grotto's discount lasts only while it is active" . gameTest $ \pid -> do
    grotto <- stagedLocation Locations.underseaGrotto
    run $ PlaceLocation grotto ActiveLocation
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (ReduceCostOf (CardWithType AllyType) 1))
    run $ PlaceLocation grotto StagingArea
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem (ReduceCostOf (CardWithType AllyType) 1))
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (ReduceNextCostOf (CardWithType AllyType) 1))

  it "Vast Troll-fells bars traveling to a safe location while a player has no resource to spend" . gameTest $ \_ -> do
    _ <- stagedLocation Locations.vastTrollFells
    cave <- stagedLocation Locations.secludedCave
    getModifiers (LocationTarget cave) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    b <- beravor
    run $ PlaceTokens GameSource b Resource 1
    getModifiers (LocationTarget cave) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

  it "Watched Path bars drawing through any card effect" . gameTest $ \pid -> do
    _ <- stagedLocation Locations.watchedPath
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotDrawCardsFromCardEffects)

  it "Weather Deck's travel cost is paid for each copy in the staging area" . gameTest $ \pid -> do
    _ <- stagedLocation Locations.weatherDeck
    _ <- stagedLocation Locations.weatherDeck
    forecastle <- stagedLocation Locations.forecastleDeck
    run $ TravelTo forecastle
    b <- beravor
    e <- eowyn
    chooseTarget pid b
    chooseTarget pid e
    exhausted b `shouldReturn` True
    exhausted e `shouldReturn` True
