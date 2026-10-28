module LordOfTheRings.AgainstTheShadow.TheDruadanForestSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getStagingThreat)
import LordOfTheRings.Keyword (Keyword (Siege))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (AnyCard), currentQuestHas, getArcheryTotal)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theDruadanForest)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

forestTest :: (PlayerId -> GameT ()) -> IO ()
forestTest = gameTestIn theDruadanForest testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
      locations = [l.placement | l <- toList g.entities.locations, toCardCode l == code]
      enemies = [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
  pure $ headMay (locations <> enemies)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))

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

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

heroWithCode :: CardCode -> GameT HeroId
heroWithCode code = toId <$> getHeroWithCode code

giveResources :: CardCode -> Int -> GameT HeroId
giveResources code n = do
  hid <- heroWithCode code
  run $ PlaceTokens GameSource (toTarget hid) Resource n
  pure hid

-- | The Passage Out in play, Drû-buri-Drû set aside for it.
passageOut :: PlayerId -> GameT Target
passageOut pid = do
  run . SetAside =<< genEncounterCard Enemies.druBuriDru
  quest <- putQuestStage Quests.thePassageOut
  run $ Flip quest
  resolveWhenRevealed pid
  pure quest

-- | The enemy attacks the player undefended, Éowyn taking the damage.
undefendedAttack :: PlayerId -> EnemyId -> GameT ()
undefendedAttack pid eid = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid
  chooseTarget pid =<< heroWithCode "01007"

spec :: Spec
spec = describe "The Drúadan Forest" do
  it "is a scenario the game can be dealt" . forestTest $ \_ ->
    isJust (lookupScenario "the-druadan-forest") `shouldBe` True

  it "sets Drû-buri-Drû aside and reveals a card per player at setup" . forestTest $ \pid -> do
    flipFirstQuest pid
    resolveRevealed
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Enemies.druBuriDru)
    getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.druBuriDru]
    revealed <- getsGame \g -> length g.entities.enemies + length g.entities.locations
    discarded <- length <$> getEncounterDiscard
    revealed + discarded `shouldSatisfy` (>= 1)

  it "has the players discard resources for prowl as the card is revealed" . forestTest $ \pid -> do
    beravor <- giveResources "01012" 2
    reveal [Enemies.druadanThief, Locations.gardenOfPoisons]
    chooseTarget pid beravor
    getHeroResources beravor `shouldReturn` 1
    -- Drúadan Thief's surge
    placementOf Locations.gardenOfPoisons `shouldReturn` Just StagingArea

  it "prowls for each player with Drúadan Elite" . forestTest $ \pid -> do
    beravor <- giveResources "01012" 2
    reveal [Enemies.druadanElite]
    chooseTarget pid beravor
    getHeroResources beravor `shouldReturn` 1

  it "gives Drúadan Hunter archery for each hero with no resources" . forestTest $ \_ -> do
    stage Enemies.druadanHunter
    getArcheryTotal `shouldReturn` 3
    _ <- giveResources "01012" 1
    getArcheryTotal `shouldReturn` 2

  it "gives each Wose archery 1 at Glade of Cleansing, its threat the highest Wose archery" . forestTest $ \_ -> do
    stage Locations.gladeOfCleansing
    glade <- locationOf Locations.gladeOfCleansing
    getLocationThreat glade `shouldReturn` 0
    stage Enemies.druadanElite
    getArcheryTotal `shouldReturn` 3
    getLocationThreat glade `shouldReturn` 3

  it "raises each Wose's threat in the staging area with Drúadan Drummer" . forestTest $ \_ -> do
    stage Enemies.druadanDrummer
    stage Enemies.druadanThief
    getStagingThreat `shouldReturn` 6

  it "keeps the heroes from gaining resources from card effects at Garden of Poisons" . forestTest $ \pid -> do
    stage Locations.gardenOfPoisons
    beravor <- heroWithCode "01012"
    run $ PlaceTokens (PlayerSource pid) (toTarget beravor) Resource 1
    getHeroResources beravor `shouldReturn` 0
    run $ BeginPhase ResourcePhase
    getHeroResources beravor `shouldReturn` 1

  it "raises the cost of each player card at Ancestral Clearing" . forestTest $ \_ -> do
    stage Locations.ancestralClearing
    getModifiers GameTarget >>= (`shouldSatisfy` elem (IncreaseCost AnyCard 1))

  it "damages each hero that does not pay for Men in the Dark, and does not surge" . forestTest $ \pid -> do
    beravor <- giveResources "01012" 1
    reveal [Treacheries.menInTheDark, Locations.gardenOfPoisons]
    chooseLabel "$label.payResource" pid
    getHeroResources beravor `shouldReturn` 0
    getDamage (toTarget beravor) `shouldReturn` 0
    (getDamage . toTarget =<< heroWithCode "01007") `shouldReturn` 1
    placementOf Locations.gardenOfPoisons `shouldReturn` Nothing

  it "surges Men in the Dark once every hero pays" . forestTest $ \pid -> do
    for_ ["01012", "01007", "01002"] \c -> giveResources c 1
    reveal [Treacheries.menInTheDark, Locations.gardenOfPoisons]
    replicateM_ 3 $ chooseLabel "$label.payResource" pid
    placementOf Locations.gardenOfPoisons `shouldReturn` Just StagingArea

  it "removes each questing hero that does not pay from the quest with Stars in Sky" . forestTest $ \_ -> do
    eowyn <- heroWithCode "01007"
    run $ CommitToQuest (toTarget eowyn)
    reveal [Treacheries.starsInSky]
    heroCommitted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False

  it "discards the attachments of a player who cannot pay for them with Leaves on Tree" . forestTest $ \pid -> do
    beravor <- heroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate (toTarget beravor)
    reveal [Treacheries.leavesOnTree, Locations.gardenOfPoisons]
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    placementOf Locations.gardenOfPoisons `shouldReturn` Just StagingArea

  it "returns the attacking enemy to the staging area unless a player pays, with Drúadan Thief's shadow" . forestTest $ \pid -> do
    eid <- engaged pid Enemies.druadanThief [Enemies.druadanThief]
    run $ DealShadowCard eid
    undefendedAttack pid eid
    placementOf Enemies.druadanThief `shouldReturn` Just StagingArea

  it "keeps the attacking enemy engaged once a player pays for Drúadan Elite's shadow" . forestTest $ \pid -> do
    beravor <- giveResources "01012" 1
    eid <- engaged pid Enemies.druadanThief [Enemies.druadanElite]
    run $ DealShadowCard eid
    undefendedAttack pid eid
    chooseLabel "$label.payResource" pid
    getHeroResources beravor `shouldReturn` 0
    placementOf Enemies.druadanThief `shouldReturn` Just (EngagedWith pid)

  it "has archery damage assigned to allies at An Untimely End" . forestTest $ \_ -> do
    _ <- putQuestStage Quests.anUntimelyEnd
    run . Flip . toTarget =<< getActiveQuest
    getModifiers GameTarget >>= (`shouldSatisfy` elem ArcheryDamageToAllies)

  it "adds Drû-buri-Drû to the staging area at The Passage Out, a siege stage" . forestTest $ \pid -> do
    _ <- passageOut pid
    placementOf Enemies.druBuriDru `shouldReturn` Just StagingArea
    currentQuestHas Siege `shouldReturn` True
    dru <- enemyOf Enemies.druBuriDru
    getModifiers (toTarget dru) >>= (`shouldSatisfy` elem CannotBeDefendedByAllies)

  it "places progress for a willpower attack, an enemy with enough going to the victory display" . forestTest $ \pid -> do
    _ <- passageOut pid
    eid <- engaged pid Enemies.druadanThief []
    eowyn <- heroWithCode "01007"
    run $ AttackWith (toTarget eowyn)
    run $ ResolveAttack pid eid
    -- Éowyn's 4 willpower against 1 defense: 3 progress, past the Thief's 2 hit points
    victoryDisplay `shouldReturn` [toCardCode Enemies.druadanThief]
    getsGame (fmap (.progress) . headMay . toList . (.entities.quests)) `shouldReturn` Just 3

  it "is not won until Drû-buri-Drû is in the victory display" . forestTest $ \pid -> do
    quest <- passageOut pid
    run $ PlaceTokens GameSource quest Progress 14
    getsGame (.outcome) `shouldReturn` Nothing
    dru <- enemyOf Enemies.druBuriDru
    run $ PlaceTokens GameSource (toTarget dru) Progress 6
    victoryDisplay `shouldReturn` [toCardCode Enemies.druBuriDru]
    getsGame (.outcome) `shouldReturn` Just Won

  it "raises each character's willpower and defense while Drû-buri-Drû is in the victory display" . forestTest $ \pid -> do
    _ <- passageOut pid
    eowyn <- toTarget <$> heroWithCode "01007"
    getWillpower eowyn `shouldReturn` 4
    dru <- enemyOf Enemies.druBuriDru
    run $ PlaceTokens GameSource (toTarget dru) Progress 6
    getWillpower eowyn `shouldReturn` 5
    getsGame (fmap (.progress) . headMay . toList . (.entities.quests)) `shouldReturn` Just 6
