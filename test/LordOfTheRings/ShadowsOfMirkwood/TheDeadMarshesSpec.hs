module LordOfTheRings.ShadowsOfMirkwood.TheDeadMarshesSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theDeadMarshes)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

marshesTest :: (PlayerId -> GameT ()) -> IO ()
marshesTest = gameTestIn theDeadMarshes testDeck

putGollum :: GameT Target
putGollum = do
  run . AddToStaging =<< genEncounterCard Objectives.gollumTheDeadMarshes
  getsGame (toTarget . fromJustNote "no Gollum" . headMay . toList . (.entities.objectives))

gollumResources :: GameT Int
gollumResources = getsGame (sum . map (.resources) . toList . (.entities.objectives))

-- | Commit the hero to the open escape test, then deal the cards.
commitAndDeal :: PlayerId -> CardCode -> GameT ()
commitAndDeal pid code = do
  chooseTarget pid =<< getHeroWithCode code
  chooseLabel "$label.doneCommittingToEscape" pid

spec :: Spec
spec = describe "The Dead Marshes" do
  it "fails Nightfall's escape test when the escape values beat the willpower" . marshesTest $ \pid -> do
    _ <- putGollum
    -- Éowyn's 4 willpower against 2 + 5
    _ <- setEncounterDeck [Treacheries.nightfall, Treacheries.nightfall, Treacheries.aWispOfPaleSheen, Treacheries.theLightsOfTheDead]
    run RevealEncounterCard
    resolveRevealed
    threat <- (.threat) <$> getPlayer pid
    -- the deck is now the Wisp (4) and the Lights (5)
    commitAndDeal pid "01007"
    gollumResources `shouldReturn` 1
    (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  it "passes an escape test when the willpower is at least the escape values" . marshesTest $ \pid -> do
    _ <- putGollum
    _ <- setEncounterDeck [Treacheries.nightfall, Locations.fensAndMires, Locations.theHeartOfTheMarshes]
    run RevealEncounterCard
    resolveRevealed
    commitAndDeal pid "01007"
    gollumResources `shouldReturn` 0

  it "adds 1 to each card's escape value while The Heart of the Marshes is active" . marshesTest $ \pid -> do
    _ <- putGollum
    lid <- do
      _ <- setEncounterDeck [Locations.theHeartOfTheMarshes]
      run RevealEncounterCard
      getsGame (fromJustNote "no Heart" . headMay . map toId . toList . (.entities.locations))
    run $ PlaceLocation lid ActiveLocation
    -- 2 + 1 and 1 + 1 against Éowyn's 4
    _ <- setEncounterDeck [Treacheries.nightfall, Locations.fensAndMires, Locations.theHeartOfTheMarshes]
    run RevealEncounterCard
    resolveRevealed
    commitAndDeal pid "01007"
    gollumResources `shouldReturn` 1

  it "shuffles Gollum into the encounter deck at 8 resources" . marshesTest $ \_ -> do
    gollum <- putGollum
    run $ PlaceTokens GameSource gollum Resource 8
    getsGame (length . (.entities.objectives)) `shouldReturn` 0
    any ((== toCardCode Objectives.gollumTheDeadMarshes) . toCardCode) <$> getEncounterDeck `shouldReturn` True

  it "wins at The Capture when the final escape test is passed" . marshesTest $ \pid -> do
    gollum <- putGollum
    run $ PlaceTokens GameSource gollum Resource 1
    _ <- setEncounterDeck [Locations.fensAndMires]
    stage <- putQuestStage Quests.theCapture
    run $ PlaceTokens GameSource stage Progress 3
    commitAndDeal pid "01007"
    getsGame (.outcome) `shouldReturn` Just Won

  it "resets to Into the Marshes when Gollum escapes The Capture" . marshesTest $ \pid -> do
    gollum <- putGollum
    run $ PlaceTokens GameSource gollum Resource 2
    _ <- setEncounterDeck [Treacheries.theLightsOfTheDead, Treacheries.theLightsOfTheDead]
    stage <- putQuestStage Quests.theCapture
    run $ PlaceTokens GameSource stage Progress 3
    commitAndDeal pid "01007"
    getsGame (.outcome) `shouldReturn` Nothing
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.intoTheMarshes

  it "removes 2 damage from Giant Marsh Worm at the end of the round" . marshesTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.giantMarshWorm]
    run RevealEncounterCard
    worm <- getsGame (fromJustNote "no worm" . headMay . map toId . toList . (.entities.enemies))
    run $ DealDamage GameSource (toTarget worm) 3
    run EndRound
    getsGame (fmap (.damage) . find ((== worm) . toId) . toList . (.entities.enemies)) `shouldReturn` Just 1

  it "has Song of Mocking's hero take the damage dealt to the chosen hero" . marshesTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    eowyn <- toTarget <$> getHeroWithCode "01007"
    song <- putAttachmentOn pid Attachments.songOfMocking beravor
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid song
    chooseTarget pid eowyn
    passActions pid
    run $ DealDamage GameSource eowyn 2
    getCharacterDamage eowyn `shouldReturn` 0
    getCharacterDamage beravor `shouldReturn` 2

  it "discards Vassal of the Windlord after it attacks" . marshesTest $ \pid -> do
    vassal <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    _ <- setEncounterDeck [Enemies.giantMarshWorm]
    run RevealEncounterCard
    worm <- getsGame (fromJustNote "no worm" . headMay . map toId . toList . (.entities.enemies))
    run $ EngageEnemy pid worm
    run $ AttackWith vassal
    run $ ResolveAttack pid worm
    getsGame (any ((== vassal) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "readies Boromir for 1 threat" . gameTestIn theDeadMarshes boromirDeck $ \pid -> do
    boromir <- getHeroWithCode (toCardCode Heroes.boromirTactics)
    run $ Exhaust (toTarget boromir)
    threat <- (.threat) <$> getPlayer pid
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid boromir
    (.threat) <$> getPlayer pid `shouldReturn` threat + 1
    getsGame (fmap (heroExhausted . toAttrs) . find ((== boromir.id) . toId) . toList . (.entities.heroes)) `shouldReturn` Just False
 where
  boromirDeck = DeckList [toCardCode Heroes.boromirTactics, "01007", "01002"] testDeck.cards
