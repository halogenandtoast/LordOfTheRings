module LordOfTheRings.DreamChaser.TheDrownedRuinsSpec (spec) where

import LordOfTheRings.Card (CardDef (cdCardTraits), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotPlay))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theDrownedRuins)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), grottoDeckL, setAsideL, victoryDisplayL)
import LordOfTheRings.Trait (Trait (Grotto))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins), mkAfter)
import TestImport

ruinsTest :: (PlayerId -> GameT ()) -> IO ()
ruinsTest = gameTestIn theDrownedRuins testDeck

locations :: GameT [LocationAttrs]
locations = getsGame (map toAttrs . toList . (.entities.locations))

locationAt :: LocationId -> GameT (Maybe LocationAttrs)
locationAt lid = find ((== lid) . locationId) <$> locations

grottoDeck :: GameT [CardCode]
grottoDeck = getsGame (map toCardCode . scenarioGrottoDeck . toAttrs . (.scenario))

setGrottoDeck :: [CardDef] -> GameT ()
setGrottoDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (grottoDeckL .~ cards)

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

-- | The location card added to the staging area, and its id.
staged :: CardDef -> GameT LocationId
staged def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such location" . fmap toId . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

-- | The location made active, as the players travel there without its cost.
activate :: CardDef -> GameT LocationId
activate def = do
  lid <- staged def
  run $ PlaceLocation lid ActiveLocation
  pure lid

allyWithCode :: CardDef -> GameT (Maybe AllyAttrs)
allyWithCode def = getsGame (fmap toAttrs . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

spec :: Spec
spec = describe "The Drowned Ruins" do
  it "is a scenario the game can be dealt" . ruinsTest $ \_ ->
    isJust (lookupScenario "the-drowned-ruins") `shouldBe` True

  it "prepares the Grotto deck, sets Shrine to Morgoth aside, adds 2 Grotto locations and gives Sahír and Na'asiyah to the players" . ruinsTest $ \pid -> do
    flipFirstQuest pid
    ls <- locations
    length ls `shouldBe` 2
    all (\l -> l.placement == StagingArea && Grotto `member` cdCardTraits (toCardDef l)) ls `shouldBe` True
    -- 3 Undersea Grotto, 3 Cursed Caverns, 2 Water-logged Halls
    length <$> grottoDeck `shouldReturn` 6
    deck <- getEncounterDeck
    filter ((Grotto `member`) . cdCardTraits . toCardDef) deck `shouldBe` []
    any ((== toCardCode Locations.drownedCave) . toCardCode) deck `shouldBe` True
    getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.shrineToMorgothTheDrownedRuins]
    sahir <- allyWithCode Objectives.captainSahirObjective
    naasiyah <- allyWithCode Objectives.naasiyahObjective
    (allyPlacement <$> sahir, allyPlacement <$> naasiyah) `shouldBe` (Just (InPlayArea pid), Just (InPlayArea pid))

  -- \* The Grotto deck

  it "may flip a Grotto location to its Underwater side after the players travel there, barring allies and attachments" . ruinsTest $ \pid -> do
    q <- putQuestStage Quests.theGrotto
    run $ Flip q
    lid <- staged Locations.underseaGrotto
    run $ TravelTo lid
    chooseAbilityOf pid lid
    (fmap locationCardCode <$> locationAt lid) `shouldReturn` Just (toCardCode Locations.darkAbyss)
    mods <- getModifiers GameTarget
    length [() | CannotPlay _ <- mods] `shouldBe` 1

  it "adds an explored Underwater location to the victory display, and the top of the Grotto deck to the staging area" . ruinsTest $ \_ -> do
    q <- putQuestStage Quests.theGrotto
    run $ Flip q
    setGrottoDeck [Locations.cursedCaverns]
    lid <- activate Locations.darkAbyss
    run $ PlaceTokens GameSource (toTarget lid) Progress 12
    victoryDisplay `shouldReturn` [toCardCode Locations.darkAbyss]
    map locationCardCode <$> locations `shouldReturn` [toCardCode Locations.cursedCaverns]
    grottoDeck `shouldReturn` []

  it "shuffles an explored Grotto location back into the Grotto deck, Grotto side up" . ruinsTest $ \_ -> do
    q <- putQuestStage Quests.theGrotto
    run $ Flip q
    setGrottoDeck []
    lid <- activate Locations.waterLoggedHalls
    run $ PlaceTokens GameSource (toTarget lid) Progress 4
    grottoDeck `shouldReturn` [toCardCode Locations.waterLoggedHalls]
    victoryDisplay `shouldReturn` []

  it "adds the top of the Grotto deck to the staging area Underwater side up as Ancient Depths is revealed" . ruinsTest $ \_ -> do
    setGrottoDeck [Locations.cursedCaverns]
    _ <- setEncounterDeck [Treacheries.ancientDepths]
    run RevealEncounterCard
    resolveRevealed
    ls <- locations
    map locationCardCode ls `shouldBe` [toCardCode Locations.twistingHollow]
    map (.otherSide) ls `shouldBe` [Just (toCardCode Locations.cursedCaverns)]

  it "resolves the active Underwater location's Forced effect as Powerful Undertow is revealed, and the location cannot flip" . ruinsTest $ \_ -> do
    lid <- activate Locations.darkAbyss
    _ <- setEncounterDeck [Treacheries.powerfulUndertow]
    run RevealEncounterCard
    resolveRevealed
    damage <- getDamage . toTarget =<< getHeroWithCode "01012"
    damage `shouldBe` 1
    -- no offer to flip it back to its Grotto side
    (fmap locationCardCode <$> locationAt lid) `shouldReturn` Just (toCardCode Locations.darkAbyss)

  -- \* Advancing

  it "cannot advance until 3 Underwater locations are in the victory display" . ruinsTest $ \pid -> do
    flipFirstQuest pid
    passActions pid
    q <- getActiveQuest
    run $ PlaceTokens GameSource (toTarget q) Progress 15
    run $ CheckStage (toId q)
    activeQuestCode `shouldReturn` toCardCode Quests.theGrotto
    underwater <- traverse genEncounterCard [Locations.darkAbyss, Locations.twistingHollow, Locations.sunkenTemple]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (victoryDisplayL .~ map EncounterCard underwater)
    run $ CheckStage (toId q)
    activeQuestCode `shouldReturn` toCardCode Quests.sahirsBetrayal

  -- \* Sahír's Betrayal

  it "makes Shrine to Morgoth active, turns Sahír on the first player and removes him, and adds an Undead enemy" . ruinsTest $ \pid -> do
    shrine <- genEncounterCard Locations.shrineToMorgothTheDrownedRuins
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [shrine])
    run . AddToStaging =<< genEncounterCard Objectives.captainSahirObjective
    _ <- setEncounterDeck [Enemies.seaScorpion, Enemies.drownedDead]
    q <- putQuestStage Quests.sahirsBetrayal
    run $ Flip q
    resolveWhenRevealed pid
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01012"
    passActions pid
    -- the one Undead enemy in the encounter deck
    chooseOptionMatching "add the Undead enemy" pid (const True)
    ls <- locations
    map (\l -> (locationCardCode l, l.placement)) ls `shouldBe` [(toCardCode Locations.shrineToMorgothTheDrownedRuins, ActiveLocation)]
    allyWithCode Objectives.captainSahirObjective `shouldReturn` Nothing
    enemies <- getsGame (map toCardCode . toList . (.entities.enemies))
    enemies `shouldBe` [toCardCode Enemies.drownedDead]
    getsGame (.outcome) `shouldReturn` Nothing

  it "floods Shrine to Morgoth at the beginning of the quest phase, keeping its progress" . ruinsTest $ \_ -> do
    lid <- activate Locations.shrineToMorgothTheDrownedRuins
    run $ PlaceTokens GameSource (toTarget lid) Progress 5
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    l <- locationAt lid
    (locationCardCode <$> l, (.progress) <$> l) `shouldBe` (Just (toCardCode Locations.shrineToMorgothUnderwater), Just 5)

  it "wins the game as Shrine to Morgoth is placed in the victory display" . ruinsTest $ \_ -> do
    lid <- activate Locations.shrineToMorgothUnderwater
    run $ PlaceTokens GameSource (toTarget lid) Progress 18
    getsGame (.outcome) `shouldReturn` Just Won
