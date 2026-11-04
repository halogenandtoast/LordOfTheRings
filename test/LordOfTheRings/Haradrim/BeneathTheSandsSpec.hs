module LordOfTheRings.Haradrim.BeneathTheSandsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (Blank, CannotCancelEncounterEffects, ProgressLimit))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (beneathTheSands, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), encounterDiscardL, questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (FlippedTo, PhaseEnds, QuestedUnsuccessfully, RoundEnds), mkAfter, mkWhen)
import TestImport

sandsTest :: (PlayerId -> GameT ()) -> IO ()
sandsTest = gameTestIn beneathTheSands testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | The Search objective, added to the staging area Off Track faceup.
search :: GameT ObjectiveId
search = do
  run . AddToStaging =<< genEncounterCard Objectives.offTrack
  getsGame (fromJustNote "no Search objective" . headMay . map toId . toList . (.entities.objectives))

-- | The Search objective, On Track faceup.
onTrack :: GameT ObjectiveId
onTrack = do
  o <- search
  run $ Flip (toTarget o)
  pure o

searchSide :: GameT [CardCode]
searchSide = getsGame (map toCardCode . toList . (.entities.objectives))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemiesInPlay :: GameT [CardCode]
enemiesInPlay = getsGame (map toCardCode . toList . (.entities.enemies))

locationsInPlay :: GameT [CardCode]
locationsInPlay = getsGame (map toCardCode . toList . (.entities.locations))

questOf :: CardDef -> GameT Target
questOf def = getsGame (toTarget . fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questProgress :: CardDef -> GameT Int
questProgress def = getsGame (sum . map (.progress) . filter ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

-- | The stage in play, on its B side.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  q <- putQuestStage def
  run $ Flip q
  resolveRevealed
  pure q

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

spec :: Spec
spec = describe "Beneath the Sands" do
  it "is a scenario the game can be dealt" . sandsTest $ \_ ->
    isJust (lookupScenario "beneath-the-sands") `shouldBe` True

  it "sets Brood Mother aside, adds the Search objective Off Track faceup, and discards until a location for the staging area" . sandsTest $ \pid -> do
    flipFirstQuest pid
    resolveWhenRevealed pid
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    map toCardCode . scenarioSetAside <$> scenarioAttrs `shouldReturn` [toCardCode Enemies.broodMother]
    ls <- getsGame (toList . (.entities.locations))
    map (.placement) ls `shouldBe` [StagingArea]
    s <- scenarioAttrs
    -- 29 Beneath the Sands cards, 6 Desert Creatures, 6 Harad Territory, less Brood Mother,
    -- the Search objective and the location staged
    length (s.encounterDeck <> s.encounterDiscard) `shouldBe` 38
    getsGame (map toCardCode . toList . (.entities.quests)) `shouldReturn` [toCardCode Quests.searchingTheCaves]

  -- \* The Search objective

  it "lets no progress be placed on the main quest while Off Track" . sandsTest $ \_ -> do
    q <- flippedStage Quests.searchingTheCaves
    o <- search
    run $ PlaceTokens GameSource q Progress 3
    questProgress Quests.searchingTheCaves `shouldReturn` 0
    run $ Flip (toTarget o)
    run $ PlaceTokens GameSource q Progress 3
    questProgress Quests.searchingTheCaves `shouldReturn` 3

  it "resolves Track after travel: flips On Track if a discarded card has On Track printed, discarding its resources" . sandsTest $ \_ -> do
    o <- search
    run $ PlaceTokens GameSource (toTarget o) Resource 2
    cavern <- stageLocation Locations.cobwebbedCavern
    _ <- setEncounterDeck [Locations.spiderBurrow, Enemies.hiveGuardian, Locations.cobwebbedCavern]
    run $ TravelTo cavern
    searchSide `shouldReturn` [toCardCode Objectives.onTrack]
    getsGame (sum . map (.resources) . toList . (.entities.objectives)) `shouldReturn` 0
    map toCardCode . (.encounterDiscard) <$> scenarioAttrs `shouldReturn` [toCardCode Locations.spiderBurrow, toCardCode Enemies.hiveGuardian]

  it "stays Off Track when no card discarded by Track has On Track printed" . sandsTest $ \_ -> do
    _ <- search
    cavern <- stageLocation Locations.cobwebbedCavern
    _ <- setEncounterDeck [Locations.spiderBurrow, Locations.cobwebbedCavern, Enemies.hiveGuardian]
    run $ TravelTo cavern
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    length <$> getEncounterDeck `shouldReturn` 1

  it "does not resolve Track while On Track" . sandsTest $ \_ -> do
    _ <- onTrack
    cavern <- stageLocation Locations.cobwebbedCavern
    _ <- setEncounterDeck [Locations.spiderBurrow, Enemies.hiveGuardian]
    run $ TravelTo cavern
    length <$> getEncounterDeck `shouldReturn` 2

  it "has Off Track raise each player's threat by its resources at the end of the quest phase, with no location to return" . sandsTest $ \pid -> do
    o <- search
    run $ PlaceTokens GameSource (toTarget o) Resource 1
    threat0 <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    (.threat) <$> getPlayer pid `shouldReturn` (threat0 + 2)

  it "lets the first player return the topmost location in the encounter discard pile instead" . sandsTest $ \pid -> do
    _ <- search
    burrow <- genEncounterCard Locations.spiderBurrow
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [burrow])
    threat0 <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    chooseTarget pid (toTarget burrow)
    locationsInPlay `shouldReturn` [toCardCode Locations.spiderBurrow]
    (.threat) <$> getPlayer pid `shouldReturn` threat0

  it "has On Track add a card with On Track from the top 3 at the end of the quest phase, discarding the rest" . sandsTest $ \_ -> do
    _ <- onTrack
    _ <- setEncounterDeck [Locations.spiderBurrow, Enemies.spiderBroodling, Treacheries.grabbedBySpiders, Locations.cobwebbedCavern]
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    enemiesInPlay `shouldReturn` [toCardCode Enemies.spiderBroodling]
    length . (.encounterDiscard) <$> scenarioAttrs `shouldReturn` 2
    searchSide `shouldReturn` [toCardCode Objectives.onTrack]

  it "flips On Track when none of the top 3 has On Track, and after the players quest unsuccessfully" . sandsTest $ \_ -> do
    o <- onTrack
    _ <- setEncounterDeck [Locations.spiderBurrow, Treacheries.grabbedBySpiders, Locations.cobwebbedCavern]
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    run $ Flip (toTarget o)
    run $ CheckWindows [mkAfter QuestedUnsuccessfully]
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]

  -- \* The stages

  it "advances from Searching the Caves once On Track, flipping back Off Track and adding an enemy as Getting Closer is revealed" . sandsTest $ \pid -> do
    q <- flippedStage Quests.searchingTheCaves
    next <- genEncounterCard Quests.gettingCloser
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (questDeckL .~ [next])
    _ <- onTrack
    _ <- setEncounterDeck [Locations.spiderBurrow, Enemies.hiveGuardian]
    run $ PlaceTokens GameSource q Progress 5
    resolveWhenRevealed pid
    getsGame (map toCardCode . toList . (.entities.quests)) `shouldReturn` [toCardCode Quests.gettingCloser]
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    enemiesInPlay `shouldReturn` [toCardCode Enemies.hiveGuardian]

  it "bars canceling encounter effects at Getting Closer while On Track" . sandsTest $ \_ -> do
    _ <- flippedStage Quests.gettingCloser
    _ <- search
    getModifiers GameTarget >>= (`shouldSatisfy` notElem CannotCancelEncounterEffects)
    _ <- getsGame (headMay . toList . (.entities.objectives)) >>= traverse_ (run . Flip . toTarget . toId)
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)

  it "adds Brood Mother as The Spiders' Hive is revealed, and cannot be defeated while she is in play" . sandsTest $ \pid -> do
    mother <- genEncounterCard Enemies.broodMother
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [mother])
    _ <- onTrack
    q <- putQuestStage Quests.theSpidersHive
    run $ Flip q
    resolveWhenRevealed pid
    enemiesInPlay `shouldReturn` [toCardCode Enemies.broodMother]
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    run . Flip . toTarget =<< getsGame (fromJustNote "no Search" . headMay . map toId . toList . (.entities.objectives))
    run $ PlaceTokens GameSource q Progress 5
    getsGame (.outcome) `shouldReturn` Nothing
    eid <- getsGame (fromJustNote "no Brood Mother" . headMay . map toId . toList . (.entities.enemies))
    run $ DealDamage GameSource (toTarget eid) 12
    getsGame (.outcome) `shouldReturn` Just Won

  it "gives Brood Mother no damage while The Spiders' Hive has fewer than 5 progress, and removes its progress as Off Track flips up" . sandsTest $ \_ -> do
    q <- flippedStage Quests.theSpidersHive
    o <- onTrack
    eid <- stage Enemies.broodMother
    run $ DealDamage GameSource (toTarget eid) 2
    getDamage (toTarget eid) `shouldReturn` 0
    run $ PlaceTokens GameSource q Progress 4
    questProgress Quests.theSpidersHive `shouldReturn` 4
    run $ CheckWindows [mkAfter $ FlippedTo (toTarget o) (toCardCode Objectives.offTrack)]
    questProgress Quests.theSpidersHive `shouldReturn` 0
    run $ PlaceTokens GameSource q Progress 5
    run $ DealDamage GameSource (toTarget eid) 2
    getDamage (toTarget eid) `shouldReturn` 2

  -- \* Encounter cards

  it "has Brood Mother add a Spider Broodling to the staging area at the end of the round" . sandsTest $ \_ -> do
    _ <- stage Enemies.broodMother
    _ <- setEncounterDeck [Locations.spiderBurrow, Enemies.spiderBroodling]
    run $ CheckWindows [mkWhen RoundEnds]
    lead <- getLead
    chooseOptionMatching "add the Broodling" lead (const True)
    sort <$> enemiesInPlay `shouldReturn` sort [toCardCode Enemies.broodMother, toCardCode Enemies.spiderBroodling]

  it "gives Hive Guardian +2 attack and Cobwebbed Cavern +2 threat while On Track" . sandsTest $ \_ -> do
    o <- search
    guardian <- stage Enemies.hiveGuardian
    cavern <- stageLocation Locations.cobwebbedCavern
    getEnemyAttack guardian `shouldReturn` 3
    getLocationThreat cavern `shouldReturn` 2
    run $ Flip (toTarget o)
    getEnemyAttack guardian `shouldReturn` 5
    getLocationThreat cavern `shouldReturn` 4

  it "has Pitch Black Tunnel's threat be the first player's character count" . sandsTest $ \_ -> do
    tunnel <- stageLocation Locations.pitchBlackTunnel
    getLocationThreat tunnel `shouldReturn` 3

  it "lets no progress be placed on staged locations while Blocked Passage is in play On Track" . sandsTest $ \_ -> do
    _ <- onTrack
    passage <- stageLocation Locations.blockedPassage
    hid <- getsGame (fromJustNote "no hero" . headMay . map toId . toList . (.entities.heroes))
    run $ PlaceTokens (HeroSource hid) (toTarget passage) Progress 2
    getsGame (sum . map (.progress) . toList . (.entities.locations)) `shouldReturn` 0

  it "flips On Track and adds Brood Mother as Spider Burrow becomes active, and sets her aside as it is explored" . sandsTest $ \_ -> do
    mother <- genEncounterCard Enemies.broodMother
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [mother])
    _ <- search
    burrow <- stageLocation Locations.spiderBurrow
    run $ PlaceLocation burrow ActiveLocation
    searchSide `shouldReturn` [toCardCode Objectives.onTrack]
    enemiesInPlay `shouldReturn` [toCardCode Enemies.broodMother]
    run $ PlaceTokens GameSource (toTarget burrow) Progress 2
    enemiesInPlay `shouldReturn` []
    map toCardCode . scenarioSetAside <$> scenarioAttrs `shouldReturn` [toCardCode Enemies.broodMother]

  it "has Nest of Vermin add the first Creature enemy discarded as the players travel there" . sandsTest $ \_ -> do
    _ <- onTrack
    nest <- stageLocation Locations.nestOfVermin
    _ <- setEncounterDeck [Enemies.spiderBroodling]
    run $ TravelTo nest
    enemiesInPlay `shouldReturn` [toCardCode Enemies.spiderBroodling]

  it "lets the first player flip the Search objective Off Track for Forced Off Track" . sandsTest $ \pid -> do
    _ <- onTrack
    reveal [Treacheries.forcedOffTrackBeneathTheSands]
    chooseLabel "$label.forcedOffTrack.flip" pid
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]

  it "has Grabbed by Spiders discard each player's ally once the Search objective flips Off Track" . sandsTest $ \pid -> do
    _ <- putQuestStage Quests.searchingTheCaves
    o <- onTrack
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    reveal [Treacheries.grabbedBySpiders, Locations.spiderBurrow]
    getsGame (map (.placement) . toList . (.entities.treacheries)) >>= (`shouldSatisfy` all (/= StagingArea))
    run $ Flip (toTarget o)
    chooseTarget pid guard'
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "blanks On Track, and not Off Track should it flip back, for the phase after Lost Underground is defeated" . sandsTest $ \_ -> do
    main <- putQuestStage Quests.fliesAndSpiders
    o <- onTrack
    reveal [Quests.lostUnderground, Locations.spiderBurrow]
    q <- questOf Quests.lostUnderground
    run $ PlaceTokens GameSource q Progress 5
    searchSide `shouldReturn` [toCardCode Objectives.onTrack]
    getModifiers (ObjectiveTarget o) >>= (`shouldSatisfy` elem Blank)
    run $ Flip (ObjectiveTarget o)
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    getModifiers (ObjectiveTarget o) >>= (`shouldSatisfy` notElem Blank)
    getModifiers main >>= (`shouldSatisfy` elem (ProgressLimit 0))

  it "keeps the Search objective from flipping while Lost Underground is in play, then flips it On Track as it is defeated" . sandsTest $ \_ -> do
    _ <- onTrack
    reveal [Quests.lostUnderground, Locations.spiderBurrow]
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    cavern <- stageLocation Locations.cobwebbedCavern
    _ <- setEncounterDeck [Enemies.hiveGuardian, Enemies.hiveGuardian]
    run $ TravelTo cavern
    searchSide `shouldReturn` [toCardCode Objectives.offTrack]
    length <$> getEncounterDeck `shouldReturn` 2
    q <- questOf Quests.lostUnderground
    run $ PlaceTokens GameSource q Progress 5
    searchSide `shouldReturn` [toCardCode Objectives.onTrack]
