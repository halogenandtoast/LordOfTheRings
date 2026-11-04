module LordOfTheRings.DreamChaser.NightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Heading (Heading (..))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Grid (Pos (..))
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario (aStormOnCobasHaven, flightOfTheStormcaller, templeOfTheDeceived, theCityOfCorsairs, theDrownedRuins, theThingInTheDepths)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Types (GameMode (..), NightmareDeck (..), ScenarioAttrs (scenarioSetAside), headingL, modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

setHeading :: Heading -> GameT ()
setHeading h = do
  g <- (scenarioL %~ overAttrs (headingL ?~ h)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

heading :: GameT (Maybe Heading)
heading = getsGame ((.heading) . toAttrs . (.scenario))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyPlacement :: CardDef -> GameT (Maybe Placement)
enemyPlacement def = getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  run . PutEnemyIntoPlayEngaged pid =<< genEncounterCard def
  enemyOf def

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

-- | The enemy attacks the player with the shadow card dealt it, undefended, its damage
-- going to the character.
undefendedAttackWith :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
undefendedAttackWith pid eid shadow' character = do
  _ <- setEncounterDeck [shadow']
  run $ DealShadowCard eid
  run $ EnemyAttack pid eid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid
  chooseTarget pid character
  passActions pid

ship :: PlayerId -> CardDef -> GameT Target
ship pid def = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
  getsGame (toTarget . fromJustNote "no such ally" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

beravor :: GameT Target
beravor = toTarget <$> getHeroWithCode "01012"

-- | Commit the characters to the Sailing test, then see what it finds.
commitAll :: PlayerId -> [Target] -> GameT ()
commitAll pid cs = do
  for_ cs (chooseTarget pid)
  chooseLabel "$label.sailingTest.done" pid

io :: IO () -> IO ()
io = id

spec :: Spec
spec = describe "Dream-chaser Nightmare mode" do
  describe "Flight of the Stormcaller" do
    it "builds the nightmare deck" . nightmare flightOfTheStormcaller $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck)
        [ Locations.calmWaters
        , Enemies.corsairSkiff
        , Enemies.southronSailor
        , Enemies.serpentsBlade
        , Locations.hiddenCove
        , Locations.jaggedReef
        , Enemies.raiderOfTheStorm
        , Enemies.tideRunner
        , Enemies.venomsBite
        , Locations.cliffsideInlet
        , Treacheries.sahirsGettingAway
        ]
        `shouldBe` [0, 0, 0, 0, 1, 0, 4, 2, 1, 3, 3]
      copies Nightmare.flightOfTheStormcallerNightmare deck `shouldBe` 0

    it "puts Venom's Bite into play as a Sailing test discards it: the staging area off-course" . nightmare flightOfTheStormcaller $ \_ -> do
      setHeading Rainy
      bite <- genEncounterCard Enemies.venomsBite
      run $ AddToEncounterDiscard [bite]
      run $ SailingTestEnded [bite] []
      enemyPlacement Enemies.venomsBite `shouldReturn` Just StagingArea
      getEncounterDiscard `shouldReturn` []

    it "and the Stormcaller's area on-course" . nightmare flightOfTheStormcaller $ \_ -> do
      setHeading Sunny
      bite <- genEncounterCard Enemies.venomsBite
      run $ AddToEncounterDiscard [bite]
      run $ SailingTestEnded [bite] []
      enemyPlacement Enemies.venomsBite `shouldReturn` Just StormcallerArea

    it "Raider of the Storm strengthens the Ship enemies engaged with its player, who add their threat" . nightmare flightOfTheStormcaller $ \pid -> do
      tide <- engaged pid Enemies.tideRunner
      _ <- engaged pid Enemies.raiderOfTheStorm
      getEnemyAttack tide `shouldReturn` 5
      getStagingThreat `shouldReturn` 5

    it "Cliffside Inlet's travel puts the first Ship enemy discarded into the Stormcaller's area" . nightmare flightOfTheStormcaller $ \_ -> do
      run . AddToStaging =<< genEncounterCard Locations.cliffsideInlet
      inlet <- getsGame (fromJustNote "no inlet" . fmap toId . headMay . toList . (.entities.locations))
      _ <- setEncounterDeck [Locations.hiddenCove, Enemies.swiftRaider, Locations.vastCoastland]
      run $ TravelTo inlet
      enemyPlacement Enemies.swiftRaider `shouldReturn` Just StormcallerArea
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.hiddenCove]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.vastCoastland]

  describe "The Thing in the Depths" do
    it "builds the nightmare deck, the Tentacles out of play with The Thing" . nightmare theThingInTheDepths $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.corsairInfiltrator, Enemies.corsairDuelist, Locations.aftCabin, Locations.captainsQuarters, Locations.crewQuarters, Treacheries.tumultuousWaters]
        `shouldBe` [0, 3, 3, 1, 1, 3]
      aside <- scenarioSetAside <$> scenarioAttrs
      map (`copies` aside) [Enemies.crushingArm, Enemies.graspingArm, Enemies.giantArm, Enemies.slitheringArm, Enemies.crashingArm, Treacheries.deepMaelstrom]
        `shouldBe` [2, 2, 2, 3, 3, 3]

    it "Deep Maelstrom gains Doomed 5 when no location reveals a card" . nightmare theThingInTheDepths $ \pid -> do
      was <- threat pid
      reveal Treacheries.deepMaelstrom []
      threat pid `shouldReturn` was + 5

    it "Corsair Duelist trades a resource for its damage instead of being defeated" . nightmare theThingInTheDepths $ \pid -> do
      duelist <- engaged pid Enemies.corsairDuelist
      run $ DealDamage GameSource (EnemyTarget duelist) 3
      e <- getEnemy duelist
      (e.damage, e.resources) `shouldBe` (0, 1)

  describe "Temple of the Deceived" do
    it "lays out the Island Map in six columns, the Edges of the Temple in the middle two" . nightmare templeOfTheDeceived $ \_ -> do
      run $ DoStep 1 Setup
      locations <- getsGame (toList . (.entities.locations))
      length locations `shouldBe` 18
      let column c = [toCardCode l | l <- locations, fmap (.column) l.position == Just c]
      column 3 <> column 4 `shouldBe` replicate 6 (toCardCode Locations.edgeOfTheTemple)
      length (column 5) `shouldBe` 3
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.faithlessDesecrator, Enemies.kingsKnight, Treacheries.sunkenRuins, Treacheries.callOfTheCurse]
        `shouldBe` [0, 2, 0, 3]

    it "Cursed Soldier gets +3 attack while the active location is in the middle row" . nightmare templeOfTheDeceived $ \_ -> do
      run $ DoStep 1 Setup
      middle <- getsGame (fromJustNote "no location" . fmap toId . find ((== Just (Pos 0 1)) . (.position)) . toList . (.entities.locations))
      run $ PlaceLocation middle ActiveLocation
      soldier <- staged Enemies.cursedSoldier
      getEnemyAttack soldier `shouldReturn` 6

  describe "The Drowned Ruins" do
    it "builds the nightmare deck and Grotto deck" . nightmare theDrownedRuins $ \_ -> do
      run $ DoStep 1 Setup
      grotto <- (.grottoDeck) <$> scenarioAttrs
      map (`copies` grotto) [Locations.overgrownPassage, Locations.sharpPrecipice, Locations.cursedCaverns, Locations.underseaGrotto, Locations.jaggedCavern]
        `shouldBe` [2, 2, 2, 2, 0]
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.floodedCaverns, Locations.drownedCave, Enemies.submergedDead, Treacheries.intoTheAbyss, Enemies.seaScorpion]
        `shouldBe` [2, 0, 4, 0, 1]

    it "keeps encounter effects from being canceled while an Underwater location is active" . nightmare theDrownedRuins $ \_ -> do
      getModifiers GameTarget >>= (`shouldSatisfy` notElem CannotCancelEncounterEffects)
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.jaggedCavern
      getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)

    it "Great Shark cannot be damaged unless the active location is Underwater" . nightmare theDrownedRuins $ \_ -> do
      shark <- staged Enemies.greatShark
      run $ DealDamage GameSource (EnemyTarget shark) 2
      (.damage) <$> getEnemy shark `shouldReturn` 0

    it "Pit into the Depths gives each ally -1 willpower this quest phase, one entering play later too" . nightmare theDrownedRuins $ \pid -> do
      putGame . (phaseL .~ QuestPhase) =<< getGame
      held <- putAllyIntoPlay pid Allies.wanderingTook
      base <- getWillpower held
      reveal Treacheries.pitIntoTheDepths []
      getWillpower held `shouldReturn` base - 1
      later <- putAllyIntoPlay pid Allies.wanderingTook
      getWillpower later `shouldReturn` base - 1

    it "Sulfur Cloud gains surge when the active location is not Underwater" . nightmare theDrownedRuins $ \_ -> do
      reveal Treacheries.sulfurCloud [Enemies.greatShark]
      enemyPlacement Enemies.greatShark `shouldReturn` Just StagingArea

    it "Sulfur Cloud limits the progress placed on all Underwater locations together this phase" . nightmare theDrownedRuins $ \pid -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.jaggedCavern
      run . AddToStaging =<< genEncounterCard Locations.jaggedCavern
      reveal Treacheries.sulfurCloud []
      -- choose 1: no more than 2 progress on Underwater locations this phase
      void $ answerQuestion pid 1
      [active, staged'] <- getsGame (sortOn (\l -> l.placement /= ActiveLocation) . toList . (.entities.locations))
      run $ PlaceTokens GameSource (toTarget active) Progress 2
      run $ PlaceTokens GameSource (toTarget staged') Progress 1
      (.progress) <$> getLocation (toId staged') `shouldReturn` 0

  describe "A Storm on Cobas Haven" do
    it "builds the nightmare deck, Aearlang and the Docks among the Dol Amroth objectives" . nightmare aStormOnCobasHaven $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck)
        [Objectives.aearlang, Objectives.docksOfDolAmroth, Locations.belfalasIslet, Locations.cobasHaven, Enemies.umbarRaider, Enemies.rammingShip, Treacheries.rammingSpeed]
        `shouldBe` [1, 1, 0, 0, 2, 2, 1]

    it "Grappled! keeps its Ship objective from readying" . nightmare aStormOnCobasHaven $ \pid -> do
      dreamChaser <- ship pid Objectives.dreamChaser
      reveal Treacheries.grappled []
      getModifiers dreamChaser >>= (`shouldSatisfy` elem CannotReady)

    it "Broken Rudder cancels each success symbol its Ship takes no damage for" . nightmare aStormOnCobasHaven $ \pid -> do
      dreamChaser <- ship pid Objectives.dreamChaser
      reveal Treacheries.brokenRudder []
      setHeading Rainy
      _ <- setEncounterDeck [Locations.calmWaters]
      b <- beravor
      run SailingTest
      commitAll pid [b]
      chooseLabel "$label.amount n=i:1" pid
      chooseLabel "$label.continue" pid
      -- Stormy, then 1 of Calm Waters' 2 icons kept
      heading `shouldReturn` Just Rainy
      getCharacterDamage dreamChaser `shouldReturn` 1

  describe "The City of Corsairs" do
    it "builds the nightmare deck: the City of Corsairs icon's cards in the second deck" . nightmare theCityOfCorsairs $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.patrolShip, Locations.umbarBridge, Enemies.corsairInfiltrator, Enemies.scoutingShip, Enemies.umbarPatrolNightmare]
        `shouldBe` [2, 2, 0, 1, 0]
      secondDeck <- scenarioSetAside <$> scenarioAttrs
      map (`copies` secondDeck) [Enemies.umbarPatrolNightmare, Objectives.theCursedSword, Locations.streetsOfUmbar, Enemies.patrolShip]
        `shouldBe` [3, 1, 2, 0]

    it "leaves a standard game's second deck alone" . gameTestIn theCityOfCorsairs testDeck $ \_ -> do
      run $ DoStep 1 Setup
      secondDeck <- scenarioSetAside <$> scenarioAttrs
      map (`copies` secondDeck) [Enemies.umbarPatrolNightmare, Locations.streetsOfUmbar] `shouldBe` [0, 3]

    it "Patrol Ship, discarded by a Sailing test, may be added to the staging area to keep its symbol" . nightmare theCityOfCorsairs $ \pid -> do
      setHeading Rainy
      _ <- setEncounterDeck [Enemies.patrolShip]
      b <- beravor
      run SailingTest
      commitAll pid [b]
      chooseLabel "$label.patrolShip.stage" pid
      chooseLabel "$label.continue" pid
      heading `shouldReturn` Just Rainy
      enemyPlacement Enemies.patrolShip `shouldReturn` Just StagingArea

    it "or cancels each symbol found" . nightmare theCityOfCorsairs $ \pid -> do
      setHeading Rainy
      _ <- setEncounterDeck [Enemies.patrolShip]
      b <- beravor
      run SailingTest
      commitAll pid [b]
      chooseLabel "$label.patrolShip.cancel" pid
      chooseLabel "$label.continue" pid
      heading `shouldReturn` Just Stormy
      enemyPlacement Enemies.patrolShip `shouldReturn` Nothing

    it "Walls of Umbar, discarded by a Sailing test, raises threat for each ally committed" . nightmare theCityOfCorsairs $ \pid -> do
      setHeading Sunny
      ally <- putAllyIntoPlay pid Allies.veteranAxehand
      _ <- setEncounterDeck [Locations.wallsOfUmbar]
      was <- threat pid
      run SailingTest
      commitAll pid [ally]
      chooseLabel "$label.continue" pid
      threat pid `shouldReturn` was + 1

    it "Raise the Alarm! stages the first Raider enemy discarded, with 3 resources" . nightmare theCityOfCorsairs $ \_ -> do
      reveal Treacheries.raiseTheAlarm [Locations.wallsOfUmbar, Enemies.umbarRaider]
      raider <- enemyOf Enemies.umbarRaider
      e <- getEnemy raider
      (e.placement, e.resources) `shouldBe` (StagingArea, 3)

    it "Raise the Alarm!'s shadow adds a resource per damage the attack dealt, none when it is prevented" . nightmare theCityOfCorsairs $ \pid -> do
      raider <- engaged pid Enemies.umbarRaider
      hero <- getsGame (toTarget . fromJustNote "no hero" . headMay . toList . (.entities.heroes))
      run $ CreateEffect $ Effect GameSource hero [CannotTakeDamage] UntilEndOfPhase
      held <- (.resources) <$> getEnemy raider
      undefendedAttackWith pid raider Treacheries.raiseTheAlarm hero
      (.resources) <$> getEnemy raider `shouldReturn` held

    it "Raise the Alarm!'s shadow adds a resource per damage the attack dealt" . nightmare theCityOfCorsairs $ \pid -> do
      raider <- engaged pid Enemies.umbarRaider
      hero <- getsGame (toTarget . fromJustNote "no hero" . headMay . toList . (.entities.heroes))
      held <- (.resources) <$> getEnemy raider
      attack <- getEnemyAttack raider
      undefendedAttackWith pid raider Treacheries.raiseTheAlarm hero
      (.resources) <$> getEnemy raider `shouldReturn` held + attack

    it "Slave Trade pays a Raider the cost of an ally its damage destroys" . nightmare theCityOfCorsairs $ \pid -> do
      raider <- staged Enemies.umbarRaider
      held <- (.resources) <$> getEnemy raider
      _ <- putAllyIntoPlay pid Allies.lorienGuide
      reveal Treacheries.slaveTrade []
      (.resources) <$> getEnemy raider `shouldReturn` held + 3

    it "Slave Trade pays nothing for an ally the damage does not destroy" . nightmare theCityOfCorsairs $ \pid -> do
      raider <- staged Enemies.umbarRaider
      held <- (.resources) <$> getEnemy raider
      guide <- putAllyIntoPlay pid Allies.lorienGuide
      run $ CreateEffect $ Effect GameSource guide [CannotTakeDamage] UntilEndOfPhase
      reveal Treacheries.slaveTrade []
      (.resources) <$> getEnemy raider `shouldReturn` held

    it "Foes of Ecthelion gives a Raider +1 threat per resource, counted as they change" . nightmare theCityOfCorsairs $ \_ -> do
      raider <- staged Enemies.umbarRaider
      base <- getEnemyThreat raider
      reveal Treacheries.foesOfEcthelion []
      getEnemyThreat raider `shouldReturn` base + 2
      run $ PlaceTokens GameSource (toTarget raider) Resource 1
      getEnemyThreat raider `shouldReturn` base + 3

  it "keeps the deck static" $ io do
    map (fmap nightmareSetupCard . (.nightmareDeck) . toAttrs) [flightOfTheStormcaller, theThingInTheDepths, templeOfTheDeceived, theDrownedRuins, aStormOnCobasHaven, theCityOfCorsairs]
      `shouldBe` map
        (Just . toCardCode)
        [ Nightmare.flightOfTheStormcallerNightmare
        , Nightmare.theThingInTheDepthsNightmare
        , Nightmare.templeOfTheDeceivedNightmare
        , Nightmare.theDrownedRuinsNightmare
        , Nightmare.aStormOnCobasHavenNightmare
        , Nightmare.theCityOfCorsairsNightmare
        ]
