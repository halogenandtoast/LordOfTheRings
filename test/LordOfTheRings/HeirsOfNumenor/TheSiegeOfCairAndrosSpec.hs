module LordOfTheRings.HeirsOfNumenor.TheSiegeOfCairAndrosSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getStagingThreat)
import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (currentQuestHas, getArcheryTotal)
import LordOfTheRings.Modifier (ModifierType (CannotBeDefendedByAllies, CannotSpendResources))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theSiegeOfCairAndros)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

siegeTest :: (PlayerId -> GameT ()) -> IO ()
siegeTest = gameTestIn theSiegeOfCairAndros testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
      locations = [l.placement | l <- toList g.entities.locations, toCardCode l == code]
      enemies = [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
  pure $ headMay (locations <> enemies)

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

damageOn :: CardDef -> GameT Int
damageOn def = getDamage . toTarget =<< locationOf def

questDeck :: GameT [CardCode]
questDeck = getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The enemy engaged with the player, the encounter deck then holding the cards given.
engaged :: PlayerId -> CardDef -> [CardDef] -> GameT EnemyId
engaged pid def deck = do
  run . AddToStaging =<< genEncounterCard def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  _ <- setEncounterDeck deck
  pure eid

-- | Put the location into play in the staging area.
stage :: CardDef -> GameT LocationId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

spec :: Spec
spec = describe "The Siege of Cair Andros" do
  it "is a scenario the game can be dealt" . siegeTest $ \_ ->
    isJust (lookupScenario "the-siege-of-cair-andros") `shouldBe` True

  it "adds the three Battleground locations to the staging area at setup" . siegeTest $ \pid -> do
    flipFirstQuest pid
    for_ [Locations.theApproach, Locations.theCitadel, Locations.theBanks] \l ->
      placementOf l `shouldReturn` Just StagingArea
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Locations.theCitadel)
    questDeck
      `shouldReturn` map toCardCode [Quests.reinforcingTheBanks, Quests.breakthroughAtTheApproach, Quests.breakthroughAtTheCitadel, Quests.theLastBattle]
    currentQuestHas Siege `shouldReturn` True

  it "removes a Battleground from the game once its damage reaches its quest points" . siegeTest $ \_ -> do
    banks <- stage Locations.theBanks
    run $ DealDamage GameSource (toTarget banks) 2
    damageOn Locations.theBanks `shouldReturn` 2
    run $ DealDamage GameSource (toTarget banks) 1
    placementOf Locations.theBanks `shouldReturn` Nothing
    getsGame ((.victoryPoints) . toAttrs . (.scenario)) `shouldReturn` 0

  it "removes stage 2 from the quest deck once The Banks is explored" . siegeTest $ \pid -> do
    flipFirstQuest pid
    banks <- locationOf Locations.theBanks
    run $ PlaceTokens GameSource (toTarget banks) Progress 3
    placementOf Locations.theBanks `shouldReturn` Nothing
    questDeck >>= (`shouldSatisfy` notElem (toCardCode Quests.reinforcingTheBanks))
    getsGame ((.victoryPoints) . toAttrs . (.scenario)) `shouldReturn` 1

  it "deals undefended damage to the lowest threat Battleground" . siegeTest $ \pid -> do
    flipFirstQuest pid
    eid <- engaged pid Enemies.orcScramblers []
    run $ UndefendedDamage pid eid
    damageOn Locations.theBanks `shouldReturn` 2

  it "advances once no Battleground is left in play" . siegeTest $ \pid -> do
    flipFirstQuest pid
    for_ [Locations.theApproach, Locations.theCitadel, Locations.theBanks] \l -> do
      lid <- locationOf l
      run $ DealDamage GameSource (toTarget lid) 11
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.reinforcingTheBanks

  it "damages each Battleground as Orc Scramblers is revealed" . siegeTest $ \_ -> do
    _ <- stage Locations.theCitadel
    _ <- stage Locations.theApproach
    reveal [Enemies.orcScramblers, Locations.theBanks]
    damageOn Locations.theCitadel `shouldReturn` 1
    damageOn Locations.theApproach `shouldReturn` 1

  it "damages an active Battleground as Battering Ram is revealed" . siegeTest $ \_ -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.theCitadel
    reveal [Enemies.batteringRam]
    damageOn Locations.theCitadel `shouldReturn` 3

  it "damages the lowest threat Battleground as Siege Raft is revealed" . siegeTest $ \_ -> do
    _ <- stage Locations.theCitadel
    _ <- stage Locations.theApproach
    reveal [Enemies.siegeRaft]
    damageOn Locations.theApproach `shouldReturn` 2
    damageOn Locations.theCitadel `shouldReturn` 0

  it "damages The Citadel with Orc Scramblers' shadow, or else raises the attack" . siegeTest $ \pid -> do
    raft <- engaged pid Enemies.siegeRaft [Enemies.orcScramblers]
    run $ DealShadowCard raft
    run $ ResolveShadow raft pid []
    getEnemyAttack raft `shouldReturn` 6
    _ <- stage Locations.theCitadel
    ram <- engaged pid Enemies.batteringRam [Enemies.orcScramblers]
    run $ DealShadowCard ram
    run $ ResolveShadow ram pid []
    damageOn Locations.theCitadel `shouldReturn` 2
    getEnemyAttack ram `shouldReturn` 7

  it "lowers each character's attack and defense with Orc Assault" . siegeTest $ \_ -> do
    reveal [Treacheries.orcAssault]
    beravor <- toTarget <$> getHeroWithCode "01012"
    getAttack beravor `shouldReturn` 0
    getDefense beravor `shouldReturn` 0

  it "has each player discard a character in place of raising threat at Breakthrough at the Approach" . siegeTest $ \pid -> do
    _ <- putQuestStage Quests.breakthroughAtTheApproach
    currentQuestHas Battle `shouldReturn` True
    threat <- (.threat) <$> getPlayer pid
    _ <- stage Locations.theCitadel
    run QuestResolution
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    getsGame (any ((== "01007") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` False
    (.threat) <$> getPlayer pid `shouldReturn` threat

  it "adds 5 threat to the staging area at Breakthrough at the Citadel" . siegeTest $ \_ -> do
    _ <- putQuestStage Quests.breakthroughAtTheCitadel
    getStagingThreat `shouldReturn` 5

  it "gains battle and loses siege at The Last Battle with 4 victory points, and is won" . siegeTest $ \pid -> do
    quest <- putQuestStage Quests.theLastBattle
    _ <- setEncounterDeck [Locations.theBanks]
    run $ Flip quest
    resolveWhenRevealed pid
    placementOf Locations.theBanks `shouldReturn` Just StagingArea
    currentQuestHas Siege `shouldReturn` True
    run . AddToVictoryDisplay . toTarget =<< stage Locations.theCitadel
    run . AddToVictoryDisplay . toTarget =<< locationOf Locations.theBanks
    currentQuestHas Siege `shouldReturn` False
    currentQuestHas Battle `shouldReturn` True
    run $ PlaceTokens GameSource quest Progress 15
    getsGame (.outcome) `shouldReturn` Just Won

  -- \* Mordor Elite

  it "keeps allies from defending Lieutenant of Mordor, which reveals the top discarded treachery again" . siegeTest $ \_ -> do
    run . AddToEncounterDiscard . pure =<< genEncounterCard Treacheries.orcAssault
    reveal [Enemies.lieutenantOfMordor]
    lieutenant <- enemyOf Enemies.lieutenantOfMordor
    getModifiers (toTarget lieutenant) >>= (`shouldSatisfy` elem CannotBeDefendedByAllies)
    beravor <- toTarget <$> getHeroWithCode "01012"
    getAttack beravor `shouldReturn` 0

  it "gives Orc Arbalesters archery for each resource icon among the heroes" . siegeTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Enemies.orcArbalesters
    -- Beravor's lore, Éowyn's spirit and Théodred's leadership
    getArcheryTotal `shouldReturn` 3

  it "keeps leadership, spirit and tactics heroes from spending while Orc Vanguard is in the staging area" . siegeTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Enemies.orcVanguard
    theodred <- toTarget <$> getHeroWithCode "01002"
    beravor <- toTarget <$> getHeroWithCode "01012"
    getModifiers theodred >>= (`shouldSatisfy` elem CannotSpendResources)
    getModifiers beravor >>= (`shouldSatisfy` notElem CannotSpendResources)

  it "takes no progress on Orc War Camp in the staging area while an Orc enemy is in play" . siegeTest $ \_ -> do
    camp <- stage Locations.orcWarCamp
    run . AddToStaging =<< genEncounterCard Enemies.orcArbalesters
    run $ PlaceTokens GameSource (toTarget camp) Progress 1
    getsGame (fmap (.progress) . lookup camp . (.entities.locations)) `shouldReturn` Just 0

  it "deals 3 damage to each character outside the sphere chosen for The Master's Malice" . siegeTest $ \pid -> do
    reveal [Treacheries.theMastersMalice]
    chooseLabel "$label.theMastersMalice.lore" pid
    (getDamage . toTarget =<< getHeroWithCode "01002") `shouldReturn` 3
    (getDamage . toTarget =<< getHeroWithCode "01012") `shouldReturn` 0

  it "shuffles the staging area away and reveals as many with The Power of Mordor" . siegeTest $ \pid -> do
    _ <- stage Locations.orcWarCamp
    run . AddToStaging =<< genEncounterCard Enemies.orcArbalesters
    threat <- (.threat) <$> getPlayer pid
    _ <- setEncounterDeck [Treacheries.thePowerOfMordor, Locations.theBanks, Locations.theApproach]
    run RevealEncounterCard
    resolveRevealed
    length <$> getEncounterDeck `shouldReturn` 2
    staged <- getsGame \g -> length [l | l <- toList g.entities.locations, l.placement == StagingArea] + length [e | e <- toList g.entities.enemies, e.placement == StagingArea]
    staged `shouldBe` 2
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3
