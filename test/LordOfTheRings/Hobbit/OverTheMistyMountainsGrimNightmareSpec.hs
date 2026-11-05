module LordOfTheRings.Hobbit.OverTheMistyMountainsGrimNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (HasCardCode)
import LordOfTheRings.Effect (EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (overTheMistyMountainsGrim)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport hiding (before, first, second)

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) overTheMistyMountainsGrim

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

codes :: HasCardCode c => [c] -> [CardCode]
codes = sort . map toCardCode

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "no enemy"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.enemies)
    )

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "no location"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.locations)
    )

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

withBilbo :: GameT Hero
withBilbo = run (DoStep 3 Setup) >> getHeroWithCode "131001"

switchToGoblinTown :: PlayerId -> GameT ()
switchToGoblinTown pid = do
  second <-
    traverse genEncounterCard (Enemies.theGreatGoblinNightmare : replicate 5 Enemies.goblinBentSwords)
  putGame . (scenarioL %~ overAttrs (\a -> a {scenarioSetAside = second})) =<< getGame
  run . AdvanceToQuest =<< genEncounterCard Quests.downDownToGoblinTown
  resolveRevealed
  getQuestion pid >>= \case
    Just q | any (\case TargetLabel {} -> True; _ -> False) (questionChoices q) ->
      chooseOptionMatching "find The Great Goblin" pid \case TargetLabel {} -> True; _ -> False
    _ -> pure ()

spec :: Spec
spec = describe "Over the Misty Mountains Grim Nightmare" do
  it "separates the Nightmare additions and removes the specified copies from both decks" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    attrs <- getsGame (toAttrs . (.scenario))
    map
      (`copies` attrs.encounterDeck)
      [ Locations.caveEntranceWesternLands
      , Locations.loneLands
      , Locations.drearyHills
      , Locations.overhangingRock
      ]
      `shouldBe` [0, 0, 0, 0]
    map
      (`copies` attrs.scenarioSetAside)
      [Enemies.theGreatGoblin, Enemies.goblinMiners, Enemies.goblinAxeman, Locations.greatCavernRoom]
      `shouldBe` [0, 0, 0, 2]
    map
      (`copies` attrs.encounterDeck)
      [Locations.narrowLedge, Locations.dimValley, Treacheries.lightningSplinters]
      `shouldBe` [3, 4, 2]
    map
      (`copies` attrs.scenarioSetAside)
      [ Enemies.theGreatGoblinNightmare
      , Enemies.goblinProwler
      , Locations.darkPassages
      , Treacheries.swishSmackWhipCrack
      ]
      `shouldBe` [1, 4, 3, 2]
    all
      (\d -> copies d attrs.encounterDeck == 0)
      [ Enemies.theGreatGoblinNightmare
      , Enemies.goblinProwler
      , Locations.darkPassages
      , Treacheries.swishSmackWhipCrack
      ]
      `shouldBe` True
    all
      (\d -> copies d attrs.scenarioSetAside == 0)
      [Locations.narrowLedge, Locations.dimValley, Treacheries.lightningSplinters]
      `shouldBe` True

  it "keeps the standard Great Goblin and encounter sets in normal mode" . gameTestIn overTheMistyMountainsGrim testDeck $ \_ -> do
    run $ DoStep 1 Setup
    attrs <- getsGame (toAttrs . (.scenario))
    copies Enemies.theGreatGoblin attrs.scenarioSetAside `shouldBe` 1
    copies Enemies.theGreatGoblinNightmare attrs.scenarioSetAside `shouldBe` 0
    copies Locations.narrowLedge attrs.encounterDeck `shouldBe` 0

  it "searches for the Nightmare Great Goblin and limits Bilbo to 1 resource in a one-player game" . nightmare $ \pid -> do
    bilbo <- withBilbo
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 5
    switchToGoblinTown pid
    Just question <- getQuestion pid
    length (questionChoices question) `shouldBe` 2
    void $ answerQuestion pid 1
    resolveRevealed
    getHeroResources bilbo.id `shouldReturn` 4
    getsGame (codes . toList . (.entities.enemies))
      `shouldReturn` codes [Enemies.theGreatGoblinNightmare, Enemies.goblinBentSwords, Enemies.goblinBentSwords]
    getsGame (length . (.scenarioSetAside) . toAttrs . (.scenario)) `shouldReturn` 0

  it "limits Bilbo to the player count rather than a fixed 1 resource" . twoPlayerTestIn nightmareScenario testDeck $ \one _ -> do
    bilbo <- withBilbo
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 5
    switchToGoblinTown one
    Just question <- getQuestion one
    length (questionChoices question) `shouldBe` 3

  it "recognizes the Nightmare Great Goblin in the victory display for the stage's defeat restriction" . nightmare $ \_ -> do
    q <- putQuestStage Quests.downDownToGoblinTown
    run $ Flip q
    getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
    -- Isolate the continuous restriction from the stage's separately tested revelation.
    putGame . (questionL .~ mempty) =<< getGame
    clearQueue
    goblin <- stageEnemy Enemies.theGreatGoblinNightmare
    run $ Destroy GameSource (EnemyTarget goblin)
    getModifiers q >>= (`shouldSatisfy` notElem CannotBeDefeated)

  it "caps commitments at 3 while Narrow Ledge is active, including commitments by card effects" . nightmare $ \pid -> do
    bilbo <- withBilbo
    ledge <- stageLocation Locations.narrowLedge
    run $ PlaceLocation ledge ActiveLocation
    heroes <- getsGame (filter ((== pid) . (.owner)) . toList . (.entities.heroes))
    for_ (filter ((/= bilbo.id) . toId) heroes) $ run . CommitToQuest . toTarget
    getModifiers bilbo >>= (`shouldSatisfy` elem CannotCommitToQuest)
    run $ CommitToQuest (toTarget bilbo)
    (.heroCommitted) . toAttrs <$> getHeroWithCode "131001" `shouldReturn` False
    run
      $ RemoveFromQuest
        (toTarget $ fromJustNote "no committed hero" $ headMay $ filter ((/= bilbo.id) . toId) heroes)
    run $ CommitToQuest (toTarget bilbo)
    (.heroCommitted) . toAttrs <$> getHeroWithCode "131001" `shouldReturn` True

  it "applies Narrow Ledge's cap separately to each player and bars allies addressed by card ID" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    ledge <- stageLocation Locations.narrowLedge
    run $ PlaceLocation ledge ActiveLocation
    heroes <- getsGame (filter ((== one) . (.owner)) . toList . (.entities.heroes))
    for_ heroes $ run . CommitToQuest . toTarget
    own <- putAllyIntoPlay one Allies.guardOfTheCitadel
    other <- putAllyIntoPlay two Allies.guardOfTheCitadel
    ownCard <-
      getsGame (fromJustNote "no own ally" . find ((== own) . toTarget) . toList . (.entities.allies))
    run $ CommitToQuest (CardIdTarget $ allyCardId $ toAttrs ownCard)
    getsGame
      ( allyCommitted
          . toAttrs
          . fromJustNote "no own ally"
          . find ((== own) . toTarget)
          . toList
          . (.entities.allies)
      )
      `shouldReturn` False
    getModifiers other >>= (`shouldSatisfy` notElem CannotCommitToQuest)
    run $ CommitToQuest other
    getsGame
      ( allyCommitted
          . toAttrs
          . fromJustNote "no other ally"
          . find ((== other) . toTarget)
          . toList
          . (.entities.allies)
      )
      `shouldReturn` True

  it "does not impose Narrow Ledge's cap in staging" . nightmare $ \pid -> do
    _ <- stageLocation Locations.narrowLedge
    heroes <- getsGame (filter ((== pid) . (.owner)) . toList . (.entities.heroes))
    for_ heroes $ run . CommitToQuest . toTarget
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    getModifiers ally >>= (`shouldSatisfy` notElem CannotCommitToQuest)

  it "shuffles only Galloping Boulders from the discard pile for Narrow Ledge's shadow" . nightmare $ \pid -> do
    enemy <- stageEnemy Enemies.goblinMiners
    discarded <-
      traverse
        genEncounterCard
        [Treacheries.gallopingBoulders, Locations.theHighPass, Treacheries.gallopingBoulders]
    run $ AddToEncounterDiscard discarded
    _ <- setEncounterDeck [Locations.overhangingRock]
    shadow <- genEncounterCard Locations.narrowLedge
    run $ AttachShadowCard enemy shadow
    run $ ResolveShadow enemy pid []
    codes
      <$> getEncounterDeck
      `shouldReturn` codes [Locations.overhangingRock, Treacheries.gallopingBoulders, Treacheries.gallopingBoulders]
    codes <$> getEncounterDiscard `shouldReturn` codes [Locations.theHighPass]

  it "pays Bilbo's exhaust and Baggins resource costs to place 5 progress on Dim Valley" . nightmare $ \pid -> do
    bilbo <- withBilbo
    run $ PlaceTokens GameSource (toTarget bilbo) Resource 1
    valley <- stageLocation Locations.dimValley
    run $ ActionWindow pid
    chooseAbilityOf pid (LocationTarget valley)
    chooseTarget pid bilbo
    (.progress) <$> getLocation valley `shouldReturn` 5
    getHeroResources bilbo.id `shouldReturn` 0
    (.heroExhausted) . toAttrs <$> getHeroWithCode "131001" `shouldReturn` True

  it "discards 1 random card per player at round end for Dim Valley" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    _ <- stageLocation Locations.dimValley
    _ <- putCardIntoHand one Events.feint
    _ <- putCardIntoHand two Events.feint
    run $ CheckWindows [mkAfter RoundEnds]
    (.hand) <$> getPlayer one `shouldReturn` []
    (.hand) <$> getPlayer two `shouldReturn` []

  it "gives Goblin Prowler its printed engagement cost until the Nightmare Great Goblin is defeated" . nightmare $ \_ -> do
    prowler <- stageEnemy Enemies.goblinProwler
    getEngagementCost prowler `shouldReturn` 45
    goblin <- stageEnemy Enemies.theGreatGoblinNightmare
    run $ Destroy GameSource (EnemyTarget goblin)
    getEngagementCost prowler `shouldReturn` 15

  it "discards a character chosen by the player Goblin Prowler engages" . nightmare $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    prowler <- stageEnemy Enemies.goblinProwler
    run $ EngageEnemy pid prowler
    chooseTarget pid ally
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    (.placement) <$> getEnemy prowler `shouldReturn` EngagedWith pid

  it
    "has the Nightmare Great Goblin discard the player count after attacking, staging only Goblin enemies" . twoPlayerTestIn nightmareScenario testDeck $ \one _ -> do
    goblin <- stageEnemy Enemies.theGreatGoblinNightmare
    _ <- setEncounterDeck [Enemies.goblinProwler, Locations.darkPassages, Enemies.goblinProwler]
    run $ CheckWindows [mkAfter $ EnemyAttacking goblin one]
    getsGame (codes . toList . (.entities.enemies))
      `shouldReturn` codes [Enemies.theGreatGoblinNightmare, Enemies.goblinProwler]
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.darkPassages]
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.goblinProwler]

  it "makes a different titled staged location active, preserving the existing active location" . nightmare $ \_ -> do
    active <- stageLocation Locations.theHighPass
    run $ PlaceLocation active ActiveLocation
    staged <- stageLocation Locations.greatCavernRoom
    _ <- stageLocation Locations.darkPassages
    reveal [Locations.darkPassages]
    (.placement) <$> getLocation active `shouldReturn` ActiveLocation
    (.placement) <$> getLocation staged `shouldReturn` ActiveLocation
    getsGame (length . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
      `shouldReturn` 2

  it "charges each player 1 threat per staged Cave when traveling to Dark Passages" . nightmare $ \pid -> do
    dark <- stageLocation Locations.darkPassages
    _ <- stageLocation Locations.greatCavernRoom
    _ <- stageLocation Locations.theHighPass
    before <- (.threat) <$> getPlayer pid
    run $ TravelTo dark
    (.threat) <$> getPlayer pid `shouldReturn` (before + 2)

  it "has Lightning Splinters discard exactly the chosen cards and assign their combined printed cost" . nightmare $ \pid -> do
    two <- putCardIntoHand pid Allies.guardOfTheCitadel
    zero <- putCardIntoHand pid Events.daeronsRunes
    one <- putCardIntoHand pid Events.feint
    kept <- putCardIntoHand pid Attachments.citadelPlate
    defender <- getHeroWithCode "01012"
    reveal [Treacheries.lightningSplinters]
    chooseTarget pid (CardIdTarget two.id)
    chooseTarget pid (CardIdTarget zero.id)
    chooseTarget pid (CardIdTarget one.id)
    (.hand) <$> getPlayer pid `shouldReturn` [kept]
    getDamage (toTarget defender) `shouldReturn` 0
    replicateM_ 3 $ chooseTarget pid defender
    getDamage (toTarget defender) `shouldReturn` 3

  it "discards as many as possible for Lightning Splinters when the hand has fewer than 3 cards" . nightmare $ \pid -> do
    zero <- putCardIntoHand pid Events.daeronsRunes
    _ <- putCardIntoHand pid Events.feint
    defender <- getHeroWithCode "01012"
    reveal [Treacheries.lightningSplinters]
    chooseTarget pid (CardIdTarget zero.id)
    (.hand) <$> getPlayer pid `shouldReturn` []
    chooseTarget pid defender
    getDamage (toTarget defender) `shouldReturn` 1

  it "finishes both players' Lightning Splinters discards before either assigns damage" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    _ <- putCardIntoHand one Events.feint
    _ <- putCardIntoHand two Allies.guardOfTheCitadel
    firstHero <-
      getsGame
        ( fromJustNote "no first hero"
            . find (\h -> h.owner == one && toCardCode h == "01012")
            . toList
            . (.entities.heroes)
        )
    secondHero <-
      getsGame
        ( fromJustNote "no second hero"
            . find (\h -> h.owner == two && toCardCode h == "01012")
            . toList
            . (.entities.heroes)
        )
    reveal [Treacheries.lightningSplinters]
    (.hand) <$> getPlayer one `shouldReturn` []
    (.hand) <$> getPlayer two `shouldReturn` []
    getDamage (toTarget firstHero) `shouldReturn` 0
    getDamage (toTarget secondHero) `shouldReturn` 0
    chooseTarget one firstHero
    replicateM_ 2 $ chooseTarget two secondHero
    getDamage (toTarget firstHero) `shouldReturn` 1
    getDamage (toTarget secondHero) `shouldReturn` 2

  it "does not assign damage for Lightning Splinters with an empty hand" . nightmare $ \pid -> do
    reveal [Treacheries.lightningSplinters]
    getQuestion pid `shouldReturn` Nothing

  it "counts only staged Goblins and a player's own engaged Goblins for Swish, smack! Whip crack!" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    _ <- stageEnemy Enemies.goblinBentSwords
    run . PutEnemyIntoPlayEngaged two =<< genEncounterCard Enemies.goblinMiners
    run . PutEnemyIntoPlayEngaged two =<< genEncounterCard Enemies.goblinAxeman
    run . PutEnemyIntoPlayEngaged one =<< genEncounterCard Enemies.forestSpider
    firstHero <-
      getsGame
        ( fromJustNote "no first hero"
            . find (\h -> h.owner == one && toCardCode h == "01012")
            . toList
            . (.entities.heroes)
        )
    secondHero <-
      getsGame
        ( fromJustNote "no second hero"
            . find (\h -> h.owner == two && toCardCode h == "01012")
            . toList
            . (.entities.heroes)
        )
    reveal [Treacheries.swishSmackWhipCrack]
    chooseTarget one firstHero
    replicateM_ 3 $ chooseTarget two secondHero
    getDamage (toTarget firstHero) `shouldReturn` 1
    getDamage (toTarget secondHero) `shouldReturn` 3

  it "prevents damage to the attacking enemy for the round with Swish, smack! Whip crack!'s shadow" . nightmare $ \pid -> do
    enemy <- stageEnemy Enemies.goblinProwler
    shadow <- genEncounterCard Treacheries.swishSmackWhipCrack
    run $ AttachShadowCard enemy shadow
    run $ ResolveShadow enemy pid []
    run $ DealDamage GameSource (EnemyTarget enemy) 1
    getDamage (EnemyTarget enemy) `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ DealDamage GameSource (EnemyTarget enemy) 1
    getDamage (EnemyTarget enemy) `shouldReturn` 1
