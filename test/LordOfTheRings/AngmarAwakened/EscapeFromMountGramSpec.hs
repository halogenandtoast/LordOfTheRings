module LordOfTheRings.AngmarAwakened.EscapeFromMountGramSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdCardType, cdCost), CardType (AllyType, HeroType), costAsResources, toCard, toCardDef)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyAttack, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getTimeCounters)
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (escapeFromMountGram, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedDeck, capturedUnder)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

gramTest :: (PlayerId -> GameT ()) -> IO ()
gramTest = gameTestIn escapeFromMountGram testDeck

-- | Setup done with Éowyn as the starting hero, and on to Gornákh's Dungeons, its When
-- Revealed resolved.
setup :: PlayerId -> GameT ()
setup pid = do
  flipFirstQuest pid
  chooseTarget pid =<< getHeroWithCode "01007"
  resolveWhenRevealed pid

cardOf :: CardDef -> GameT CardId
cardOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ fromJustNote "no such card"
    $ headMay
    $ [(toCard l).id | l <- toList g.entities.locations, toCardCode l == code]
    <> [(toCard e).id | e <- toList g.entities.enemies, toCardCode e == code]
    <> [(toCard q).id | q <- toList g.entities.quests, toCardCode q == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

questOf :: CardDef -> GameT Target
questOf def = getsGame (fromJustNote "no such quest" . fmap toTarget . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

heroCodes :: GameT [CardCode]
heroCodes = getsGame (map toCardCode . toList . (.entities.heroes))

underCount :: CardDef -> GameT Int
underCount def = length <$> (capturedUnder =<< cardOf def)

-- | An ally of the player's captured underneath the card.
captureAlly :: PlayerId -> CardDef -> Target -> GameT Card
captureAlly pid def host = do
  ally <- putAllyIntoPlay pid def
  card <- getsGame (toCard . fromJustNote "no ally" . find ((== ally) . toTarget) . toList . (.entities.allies))
  run $ PlaceUnderneath host [card]
  pure card

spec :: Spec
spec = describe "Escape from Mount Gram" do
  it "is a scenario the game can be dealt" . gramTest $ \_ ->
    isJust (lookupScenario "escape-from-mount-gram") `shouldBe` True

  it "starts each player with one hero, 2 resources on it, its threat and 3 cards, the rest captured" . gramTest $ \pid -> do
    setup pid
    heroCodes `shouldReturn` ["01007"]
    eowyn <- getHeroWithCode "01007"
    getHeroResources (toId eowyn) `shouldReturn` 2
    threatOf pid `shouldReturn` 9
    length . (.hand) <$> getPlayer pid `shouldReturn` 3
    -- Capture 7 on the stage, Capture 1 and 2 more on the Prison Cell it adds
    underCount Quests.gornakhsDungeons `shouldReturn` 7
    underCount Locations.prisonCell `shouldReturn` 3
    -- a hero not chosen was set on top of the captured deck, so the stage captured it
    dungeon <- capturedUnder =<< cardOf Quests.gornakhsDungeons
    any ((== HeroType) . cdCardType . toCardDef) dungeon `shouldBe` True
    -- no ally or Item is left in the deck or hand
    player <- getPlayer pid
    [c | c <- player.deck <> player.hand, c.cardType `elem` [HeroType]] `shouldBe` []
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Locations.southernGate)
    deck `shouldSatisfy` notElem (toCardCode Enemies.jailorGornakh)

  it "rescues captured cards at random from Gornákh's Dungeons in place of its progress" . gramTest $ \pid -> do
    setup pid
    dungeon <- questOf Quests.gornakhsDungeons
    hand <- length . (.hand) <$> getPlayer pid
    heroes <- length <$> heroCodes
    run $ PlaceTokens GameSource dungeon Progress 7
    underCount Quests.gornakhsDungeons `shouldReturn` 0
    (.progress) <$> getActiveQuest `shouldReturn` 0
    hand' <- length . (.hand) <$> getPlayer pid
    heroes' <- length <$> heroCodes
    (hand' - hand) + (heroes' - heroes) `shouldBe` 7
    heroes' `shouldSatisfy` (> heroes)

  it "advances to Flight from Mount Gram once no captured cards are left under the stage" . gramTest $ \pid -> do
    setup pid
    dungeon <- questOf Quests.gornakhsDungeons
    run $ PlaceTokens GameSource dungeon Progress 7
    _ <- setEncounterDeck [Locations.patrolRoom]
    run $ EndPhase QuestPhase
    resolveWhenRevealed pid
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.flightFromMountGram
    _ <- locationOf Locations.southernGate
    _ <- enemyOf Enemies.jailorGornakh
    _ <- locationOf Locations.patrolRoom
    pure ()

  it "lets no one travel to Southern Gate below 14 progress, nor defeat the stage while it is in play" . gramTest $ \pid -> do
    flight <- putQuestStage Quests.flightFromMountGram
    staged Locations.southernGate
    gate <- locationOf Locations.southernGate
    run $ PlaceTokens GameSource flight Progress 14
    getsGame (.outcome) `shouldReturn` Nothing
    threat <- threatOf pid
    run $ TravelTo gate
    threatOf pid `shouldReturn` max threat 35
    run $ AddToVictoryDisplay (toTarget gate)
    getsGame (.outcome) `shouldReturn` Just Won

  it "gives each Orc enemy +2 attack while Southern Gate is the active location" . gramTest $ \_ -> do
    staged Enemies.dungeonGuard
    guard' <- enemyOf Enemies.dungeonGuard
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.southernGate
    getEnemyAttack guard' `shouldReturn` 4

  it "has Cruel Torturer capture the character its attack would damage, rescued to hand once it is destroyed" . gramTest $ \pid -> do
    staged Enemies.cruelTorturer
    torturer <- enemyOf Enemies.cruelTorturer
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ AttackDamage torturer ally
    getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` []
    underCount Enemies.cruelTorturer `shouldReturn` 1
    run $ Destroy GameSource (toTarget torturer)
    map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.guardOfTheCitadel))

  it "has Jailor Gornákh engage the player with the highest threat and attack as captured cards are rescued" . gramTest $ \pid -> do
    staged Enemies.jailorGornakh
    staged Enemies.dungeonGuard
    jailor <- enemyOf Enemies.jailorGornakh
    guard' <- enemyOf Enemies.dungeonGuard
    _ <- captureAlly pid Allies.silverlodeArcher (toTarget guard')
    run $ Destroy GameSource (toTarget guard')
    (.placement) <$> getEnemy jailor `shouldReturn` EngagedWith pid

  it "has Raid Golfimbul's Vault discard each card rescued from it but Items, a hero too" . gramTest $ \pid -> do
    vault <- EncounterCard <$> genEncounterCard Quests.raidGolfimbulsVault
    aragorn <- genPlayerCard Heroes.aragorn (Just pid)
    run $ ScenarioSpecific "mountGram.rescue" (toJSON (vault, [aragorn]))
    heroCodes >>= (`shouldSatisfy` notElem (toCardCode Heroes.aragorn))
    map toCardCode . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Heroes.aragorn))

  it "lets the players put a card rescued from Dungeon Guard into play" . gramTest $ \pid -> do
    staged Enemies.dungeonGuard
    guard' <- enemyOf Enemies.dungeonGuard
    card <- captureAlly pid Allies.silverlodeArcher (toTarget guard')
    run $ Destroy GameSource (toTarget guard')
    chooseTarget pid (CardIdTarget card.id)
    getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.silverlodeArcher]
    map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` notElem (toCardCode Allies.silverlodeArcher))

  it "puts a captured card from under Prison Cell into play as the players travel there" . gramTest $ \pid -> do
    staged Locations.prisonCell
    cell <- locationOf Locations.prisonCell
    card <- captureAlly pid Allies.silverlodeArcher (toTarget cell)
    run $ TravelTo cell
    chooseAbilityOf pid cell
    chooseTarget pid (CardIdTarget card.id)
    getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.silverlodeArcher]
    underCount Locations.prisonCell `shouldReturn` 0

  it "gives Tunnels of Mount Gram a threat of the cards captured under it, and Goblin Tormentor an attack of the cards holding them" . gramTest $ \pid -> do
    staged Locations.tunnelsOfMountGram
    tunnels <- locationOf Locations.tunnelsOfMountGram
    getLocationThreat tunnels `shouldReturn` 0
    _ <- captureAlly pid Allies.silverlodeArcher (toTarget tunnels)
    _ <- captureAlly pid Allies.guardOfTheCitadel (toTarget tunnels)
    underCount Locations.tunnelsOfMountGram `shouldReturn` 2
    getLocationThreat tunnels `shouldReturn` 2
    staged Enemies.goblinTormentor
    tormentor <- enemyOf Enemies.goblinTormentor
    getEnemyAttack tormentor `shouldReturn` 1

  it "has Interrogation discard the top of each captured deck, raising threat by its cost" . gramTest $ \pid -> do
    setup pid
    top <- fromJustNote "no captured card" . headMay <$> capturedDeck pid
    threat <- threatOf pid
    reveal [Treacheries.interrogation]
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem top.id)
    threatOf pid `shouldReturn` threat + maybe 0 costAsResources (cdCost $ toCardDef top)

  it "gains doomed 2 when a captured deck has nothing to discard" . gramTest $ \pid -> do
    threat <- threatOf pid
    reveal [Treacheries.interrogation]
    threatOf pid `shouldReturn` threat + 2

  it "has Stop the Executioners! capture 5, then discard them and raise threat once its time runs out" . gramTest $ \pid -> do
    setup pid
    _ <- setEncounterDeck [Quests.stopTheExecutioners]
    run RevealEncounterCard
    executioners <- questOf Quests.stopTheExecutioners
    getTimeCounters executioners `shouldReturn` 4
    held <- capturedUnder =<< cardOf Quests.stopTheExecutioners
    length held `shouldBe` 5
    threat <- threatOf pid
    run $ RemoveTokens GameSource executioners TimeCounter 4
    getsGame (map toCardCode . toList . (.entities.quests)) >>= (`shouldSatisfy` notElem (toCardCode Quests.stopTheExecutioners))
    let characters = length [c | c <- held, cdCardType (toCardDef c) `elem` [HeroType, AllyType]]
    threatOf pid `shouldReturn` threat + 3 * characters
    map (.id) . (.discard) <$> getPlayer pid >>= \d -> all ((`elem` d) . (.id)) held `shouldBe` True

  it "has Captives of Gornákh capture an ally underneath the encounter card with the highest threat" . gramTest $ \pid -> do
    staged Enemies.jailorGornakh
    _ <- putAllyIntoPlay pid Allies.silverlodeArcher
    reveal [Treacheries.captivesOfGornakh]
    underCount Enemies.jailorGornakh `shouldReturn` 1

  it "has Sound the Alarm! add an enemy when there is none, and give each enemy -10 engagement cost and +1 attack" . gramTest $ \pid -> do
    discard <- genEncounterCard Enemies.dungeonGuard
    run $ AddToEncounterDiscard [discard]
    reveal [Treacheries.soundTheAlarm]
    chooseTarget pid discard
    guard' <- enemyOf Enemies.dungeonGuard
    getEngagementCost guard' `shouldReturn` 6
    getEnemyAttack guard' `shouldReturn` 3

  it "gives each player his own staging area and Gornákh's Dungeons" $ do
    seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", testDeck)])
    env <- newGameEnv (newTableIn escapeFromMountGram seats) (const $ pure ()) 1
    runGameT env do
      order <- getsGame (.playerOrder)
      run Setup
      -- keep both hands, flip the stage, choose each starting hero, resolve each When Revealed
      let answerAll :: Int -> GameT ()
          answerAll n = when (n > 0) do
            open <- filterM (fmap isJust . getQuestion) order
            for_ (take 1 open) \p -> answerQuestion p 0 >> answerAll (n - 1)
      answerAll 20
      separated <- getsGame (isJust . gameSeparation)
      separated `shouldBe` True
      for_ order \p -> length <$> capturedDeck p >>= (`shouldSatisfy` (> 0))
