module LordOfTheRings.Standalone.BattleOfLakeTownSpec (spec) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Burn))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getBurn, getEnemy)
import LordOfTheRings.Modifier (ModifierType (Blank, GainKeyword, ImmuneToPlayerCardEffects, WhenRevealedCannotBeCanceled))
import TestImport

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "location missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations)

spec :: Spec
spec = describe "Battle of Lake-town locations" do
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
