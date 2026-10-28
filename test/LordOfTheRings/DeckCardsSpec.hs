-- | The cards of The One Deck and Seastan's Boromir decklists.
module LordOfTheRings.DeckCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Matcher (getCurrentQuest, getEnemy)
import LordOfTheRings.Modifier (ModifierType (CurrentQuest))
import LordOfTheRings.Phase (Phase (QuestPhase, ResourcePhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioVictoryDisplay))
import LordOfTheRings.Window
import LordOfTheRings.Decklist (deckProblem, loadPlayer)
import TestImport

-- Beravor (Lore, Dúnedain), Éowyn (Spirit) and Théodred (Leadership): the test deck's heroes.
beravor, eowyn, theodred :: CardCode
beravor = "01012"
eowyn = "01007"
theodred = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

putHero :: PlayerId -> CardDef -> GameT Target
putHero pid def = do
  run . PutHeroIntoPlay pid =<< genPlayerCard def (Just pid)
  heroOf pid (toCardCode def)

stat :: (Target -> GameT Int) -> Target -> GameT Int
stat f t = run (Run []) >> f t

staged :: CardDef -> GameT EnemyId
staged def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))
    <&> toId

giveResources :: CardCode -> Int -> GameT ()
giveResources code n = do
  h <- hero code
  run $ PlaceTokens GameSource h Resource n

resourcesOf :: Target -> GameT Int
resourcesOf = \case
  HeroTarget hid -> getHeroResources hid
  _ -> pure 0

handOf :: PlayerId -> GameT [Card]
handOf pid = (.hand) <$> getPlayer pid

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

onTopOfDeck :: PlayerId -> CardDef -> GameT Card
onTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (card :)) =<< getGame
  pure card

onBottomOfDeck :: PlayerId -> CardDef -> GameT Card
onBottomOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (<> [card])) =<< getGame
  pure card

inDiscard :: PlayerId -> CardDef -> GameT Card
inDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . discardL %~ (card :)) =<< getGame
  pure card

ally :: Target -> GameT AllyAttrs
ally t = getsGame (toAttrs . fromJustNote "no ally" . find ((== t) . toTarget) . toList . (.entities.allies))

allyResources :: Target -> GameT Int
allyResources t = countTokens Resource . allyTokens <$> ally t

-- | Whether the open question offers the card to be clicked.
offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices
    <$> getQuestion pid

-- | Gather Information, played from hand to the staging area.
playGatherInformation :: PlayerId -> GameT Target
playGatherInformation pid = do
  card <- playWith pid theodred 0 Quests.gatherInformation
  passActions pid
  getsGame (toTarget . fromJustNote "no side quest" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.quests))

-- | The decklists, from RingsDB: The One Deck (8682) and Seastan's Boromir (42).
theOneDeck, seastansBoromir :: DeckList
theOneDeck = DeckList ["04128", "10140", "145002"] [("01014", 1), ("01026", 2), ("01031", 1), ("01045", 2), ("01046", 1), ("01050", 2), ("01057", 2), ("01073", 3), ("02079", 1), ("02100", 2), ("02119", 3), ("04083", 3), ("04106", 3), ("04134", 3), ("04137", 3), ("08146", 2), ("09014", 1), ("10145", 2), ("12006", 1), ("12009", 3), ("12085", 2), ("17004", 1), ("17006", 3), ("17008", 3)]
seastansBoromir = DeckList ["02095", "04101", "08112"] [("01026", 3), ("01048", 2), ("01050", 2), ("01053", 2), ("01057", 1), ("01073", 3), ("02056", 3), ("04006", 1), ("04058", 1), ("04059", 3), ("04083", 1), ("04107", 3), ("04108", 2), ("04110", 2), ("05013", 3), ("06005", 3), ("06010", 3), ("06085", 3), ("08117", 3), ("08118", 3), ("08121", 3)]

spec :: Spec
spec = describe "The One Deck and Seastan's Boromir" do
  it "both decklists load, every card known and within the deckbuilding rules" . (id :: IO () -> IO ()) $
    for_ [theOneDeck, seastansBoromir] \dl -> do
      deckProblem dl `shouldBe` Nothing
      loaded <- loadPlayer "tester" dl
      either (Left . show) (const $ Right ()) loaded `shouldBe` Right ()

  it "Treebeard enters play exhausted, collects a resource each resource phase, and pays 2 to ready an Ent" . gameTest $ \pid -> do
    tb <- putAllyIntoPlay pid Allies.treebeard
    allyExhausted <$> ally tb `shouldReturn` True
    run $ BeginPhase ResourcePhase
    run $ BeginPhase ResourcePhase
    allyResources tb `shouldReturn` 2
    run $ ActionWindow pid
    chooseAbilityOf pid tb
    allyExhausted <$> ally tb `shouldReturn` False
    allyResources tb `shouldReturn` 0

  it "Glorfindel is played from the discard pile, and discards a card to ready" . gameTest $ \pid -> do
    card <- inDiscard pid Allies.glorfindel
    giveResources eowyn 5
    run $ PlayCard pid card.id
    glorfindel <- getsGame (toTarget . fromJustNote "not in play" . find ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies))
    run $ Exhaust glorfindel
    other <- putCardIntoHand pid Events.aTestOfWill
    run $ ActionWindow pid
    chooseAbilityOf pid glorfindel
    chooseTarget pid (CardIdTarget other.id)
    allyExhausted <$> ally glorfindel `shouldReturn` False

  it "Elven-light is played only from the discard pile, returning to hand and drawing a card" . gameTest $ \pid -> do
    inHand <- putCardIntoHand pid Events.elvenLight
    card <- inDiscard pid Events.elvenLight
    giveResources eowyn 1
    run $ ActionWindow pid
    offersCard pid inHand `shouldReturn` False
    chooseTarget pid (CardIdTarget card.id)
    hand <- map (.id) <$> handOf pid
    hand `shouldSatisfy` elem card.id
    length hand `shouldBe` 3

  it "Marksman of Lórien gives an enemy -2 defense as it enters play" . gameTest $ \pid -> do
    eid <- staged Enemies.hillTroll
    base <- getEnemyDefense eid
    marksman <- putAllyIntoPlay pid Allies.marksmanOfLorien
    chooseAbilityOf pid marksman
    chooseTarget pid eid
    getEnemyDefense eid `shouldReturn` base - 2

  it "Yazan deals 1 damage to a non-unique enemy as he is declared an attacker" . gameTest $ \pid -> do
    eid <- staged Enemies.hillTroll
    yazan <- putAllyIntoPlay pid Allies.yazan
    run $ CheckWindows [mkAfter $ DeclaredAsAttacker yazan eid]
    chooseAbilityOf pid yazan
    chooseTarget pid eid
    getDamage (toTarget eid) `shouldReturn` 1

  it "Jubayr discards a facedown shadow card after he exhausts to defend" . gameTest $ \pid -> do
    eid <- staged Enemies.wolfRider
    shadow <- genEncounterCard Locations.mountainsOfMirkwood
    run $ AttachShadowCard eid shadow
    jubayr <- putAllyIntoPlay pid Allies.jubayr
    run $ Exhaust jubayr
    run $ CheckWindows [mkAfter $ DeclaredAsDefender jubayr eid]
    chooseAbilityOf pid jubayr
    chooseTarget pid shadow
    (.shadowCards) <$> getEnemy eid `shouldReturn` []
    map (.id) <$> getEncounterDiscard `shouldReturn` [shadow.id]

  it "Firyal looks at the top encounter card as she commits, and may discard it" . gameTest $ \pid -> do
    [top] <- setEncounterDeck [Locations.mountainsOfMirkwood]
    firyal <- putAllyIntoPlay pid Allies.firyal
    run $ CheckWindows [mkAfter $ CommittedToQuest firyal]
    chooseAbilityOf pid firyal
    chooseLabel "$label.firyal.discard" pid
    getEncounterDeck `shouldReturn` []
    map (.id) <$> getEncounterDiscard `shouldReturn` [top.id]

  it "Arwen Undómiel discards a card to add a resource to a Noldor" . gameTest $ \pid -> do
    arwen <- putHero pid Heroes.arwenUndomiel
    card <- putCardIntoHand pid Events.aTestOfWill
    run $ ActionWindow pid
    chooseAbilityOf pid arwen
    chooseTarget pid (CardIdTarget card.id)
    resourcesOf arwen `shouldReturn` 1
    handOf pid `shouldReturn` []

  it "Éowyn lowers your threat by 3 at setup, and raises it by 3 to ready with +9 attack" . gameTest $ \pid -> do
    e <- putHero pid Heroes.eowynTactics
    setThreat pid 30
    run $ DoStep 3 Setup
    threat pid `shouldReturn` 27
    base <- stat getAttack e
    run $ Exhaust e
    run $ ActionWindow pid
    chooseAbilityOf pid e
    threat pid `shouldReturn` 30
    heroExhausted . toAttrs <$> getHeroWithCode (toCardCode Heroes.eowynTactics) `shouldReturn` False
    getAttack e `shouldReturn` base + 9

  it "Blood of Númenor spends a resource for +1 defense for each one left" . gameTest $ \pid -> do
    b <- hero beravor
    blood <- putAttachmentOn pid Attachments.bloodOfNumenor b
    giveResources beravor 3
    base <- stat getDefense b
    run $ ActionWindow pid
    chooseAbilityOf pid blood
    resourcesOf b `shouldReturn` 2
    getDefense b `shouldReturn` base + 2

  it "Gondorian Fire spends a resource for +1 attack for each one left" . gameTest $ \pid -> do
    b <- hero beravor
    fire <- putAttachmentOn pid Attachments.gondorianFire b
    giveResources beravor 2
    base <- stat getAttack b
    run $ ActionWindow pid
    chooseAbilityOf pid fire
    getAttack b `shouldReturn` base + 1

  it "Mirror of Galadriel takes a card from the top 10, then discards one at random" . gameTest $ \pid -> do
    galadriel <- putHero pid Heroes.galadriel
    mirror <- putAttachmentOn pid Attachments.mirrorOfGaladriel galadriel
    wanted <- onTopOfDeck pid Events.aTestOfWill
    _ <- putCardIntoHand pid Events.hastyStroke
    run $ ActionWindow pid
    chooseAbilityOf pid mirror
    chooseTarget pid (CardIdTarget wanted.id)
    length <$> handOf pid `shouldReturn` 1
    length . (.discard) <$> getPlayer pid `shouldReturn` 1
    getsGame (any (\a -> toTarget a == mirror && attachmentExhausted (toAttrs a)) . toList . (.entities.attachments))
      `shouldReturn` True

  it "A Good Harvest lets any sphere's resources pay for the named sphere's cards" . gameTest $ \pid -> do
    setThreat pid 30
    giveResources theodred 2
    card <- putCardIntoHand pid Events.needfulToKnow
    player <- getPlayer pid
    canAfford player card `shouldReturn` False
    _ <- playWith pid theodred 0 Events.aGoodHarvest
    chooseLabel "$label.aGoodHarvest.lore" pid
    player' <- getPlayer pid
    canAfford player' card `shouldReturn` True

  it "Heed the Dream searches the top 5, then the deck again for 3 leadership" . gameTest $ \pid -> do
    top <- onTopOfDeck pid Events.aTestOfWill
    bottom <- onBottomOfDeck pid Events.hastyStroke
    giveResources theodred 3
    _ <- playWith pid beravor 1 Events.heedTheDream
    chooseTarget pid (CardIdTarget top.id)
    chooseLabel "$label.heedTheDream.searchAgain" pid
    t <- hero theodred
    replicateM_ 3 (chooseTarget pid t)
    chooseTarget pid (CardIdTarget bottom.id)
    hand <- map (.id) <$> handOf pid
    sort hand `shouldBe` sort [top.id, bottom.id]
    resourcesOf t `shouldReturn` 0

  it "Gather Information goes to the staging area, and the first player may quest against it" . gameTest $ \pid -> do
    main <- putQuestStage Quests.fliesAndSpiders
    side <- playGatherInformation pid
    getCurrentQuest `shouldReturn` Just main
    run $ BeginPhase QuestPhase
    chooseTarget pid side
    getCurrentQuest `shouldReturn` Just side

  it "Gather Information, quested against, takes the progress and lets each player search their deck once defeated" . gameTest $ \pid -> do
    main <- putQuestStage Quests.fliesAndSpiders
    side <- playGatherInformation pid
    wanted <- onBottomOfDeck pid Events.aTestOfWill
    run $ CreateEffect $ Effect GameSource side [CurrentQuest] UntilEndOfPhase
    run $ PlaceProgressOnQuest 6
    chooseTarget pid (CardIdTarget wanted.id)
    map (.id) <$> handOf pid `shouldReturn` [wanted.id]
    getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` [toCardCode Quests.gatherInformation]
    getCurrentQuest `shouldReturn` Just main
    getsGame (fmap (.progress) . find ((== main) . toTarget) . toList . (.entities.quests)) `shouldReturn` Just 0
