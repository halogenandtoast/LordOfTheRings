module LordOfTheRings.Starter.DefendersOfGondorSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (PlayerCard (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotAttack))
import LordOfTheRings.Phase
import TestImport

-- Boromir, Prince Imrahil and Mablung (32 starting threat).
boromir, imrahil, mablung :: CardCode
boromir = "05002"
imrahil = "02050"
mablung = "08084"

dogTest :: (PlayerId -> GameT ()) -> IO ()
dogTest = gameTestWith (coreDeck "defenders-of-gondor")

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

resources :: CardCode -> GameT Int
resources code = (.resources) <$> getHeroWithCode code

exhausted :: Target -> GameT Bool
exhausted = \case
  HeroTarget hid -> getsGame (maybe False (heroExhausted . toAttrs) . lookup hid . (.entities.heroes))
  AllyTarget aid -> getsGame (maybe False (allyExhausted . toAttrs) . lookup aid . (.entities.allies))
  AttachmentTarget aid -> getsGame (maybe False (attachmentExhausted . toAttrs) . lookup aid . (.entities.attachments))
  _ -> pure False

give :: CardCode -> Int -> GameT ()
give code n = do
  h <- hero code
  run $ PlaceTokens GameSource h Resource n

-- | A Dol Guldur Orcs added to the staging area, with what it brings (engagement 10, 2
-- threat, 2 attack, 0 defense, 3 hit points).
stagedOrcs :: GameT EnemyId
stagedOrcs = do
  card <- genEncounterCard Enemies.dolGuldurOrcs
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== toCardCode card) . toCardCode) . toList . (.entities.enemies)) <&> toId

placementOf :: Target -> GameT Placement
placementOf = \case
  AttachmentTarget aid -> getsGame (maybe Unplaced (attachmentPlacement . toAttrs) . lookup aid . (.entities.attachments))
  _ -> pure Unplaced

-- | Play a card from hand in the planning phase.
playInPlanning :: PlayerId -> CardDef -> GameT Card
playInPlanning pid def = do
  card <- putCardIntoHand pid def
  run $ BeginPhase PlanningPhase
  chooseTarget pid (CardIdTarget card.id)
  pure card

putHero :: PlayerId -> CardDef -> GameT Target
putHero pid def = do
  run . PutHeroIntoPlay pid =<< genPlayerCard def (Just pid)
  hero (toCardCode def)

trapped :: PlayerId -> CardDef -> GameT (Target, EnemyId)
trapped pid def = do
  run . PlayAttachment pid =<< genPlayerCard def (Just pid)
  trap <- getsGame (toTarget . fromJustNote "no trap" . headMay . toList . (.entities.attachments))
  (trap,) <$> stagedOrcs

-- | The tests start without setup, so the threat starts at 0.
threatTo40 :: PlayerId -> GameT ()
threatTo40 pid = do
  t <- (.threat) <$> getPlayer pid
  run $ RaiseThreat pid (40 - t)

spec :: Spec
spec = describe "Defenders of Gondor" do
  it "Prince Imrahil readies after a character leaves play" . dogTest $ \pid -> do
    i <- hero imrahil
    run $ Exhaust i
    ally' <- putAllyIntoPlay pid Allies.defenderOfRammas
    run $ Discard ally'
    chooseAbilityOf pid i
    exhausted i `shouldReturn` False

  it "Boromir gives Gondor allies +1 attack while he has a resource" . dogTest $ \pid -> do
    ally' <- putAllyIntoPlay pid Allies.defenderOfRammas
    getAttack ally' `shouldReturn` 1
    give boromir 1
    getAttack ally' `shouldReturn` 2

  it "Mablung gains a resource after you engage an enemy" . dogTest $ \pid -> do
    eid <- stagedOrcs
    run $ EngageEnemy pid eid
    chooseAbilityOf pid =<< hero mablung
    resources mablung `shouldReturn` 1

  it "Beregond lowers the cost of an Armor played on him by 2" . dogTest $ \pid -> do
    b <- putHero pid Heroes.beregond
    _ <- playInPlanning pid Attachments.gondorianShield
    chooseTarget pid b
    getDefense b `shouldReturn` 6

  it "Faramir gets +1 attack for each enemy in the staging area" . dogTest $ \pid -> do
    f <- putHero pid Heroes.faramir
    _ <- stagedOrcs
    getAttack f `shouldReturn` 3

  it "Errand-rider moves a resource to another hero" . dogTest $ \pid -> do
    rider <- putAllyIntoPlay pid Allies.errandRider
    give boromir 1
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid rider
    chooseTarget pid =<< hero imrahil
    resources boromir `shouldReturn` 0
    resources imrahil `shouldReturn` 1

  it "Citadel Custodian costs 1 less for each Gondor ally in play" . dogTest $ \pid -> do
    replicateM_ 2 $ putAllyIntoPlay pid Allies.defenderOfRammas
    _ <- playWith pid boromir 4 Allies.citadelCustodian
    resources boromir `shouldReturn` 1
    getsGame (length . (.entities.allies)) `shouldReturn` 3

  it "Envoy of Pelargir adds a resource to a Gondor or Noble hero" . dogTest $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.envoyOfPelargir
    chooseAbility pid
    chooseTarget pid =<< hero imrahil
    resources imrahil `shouldReturn` 1

  it "Squire of the Citadel adds a resource to a Gondor hero as it leaves play" . dogTest $ \pid -> do
    squire <- putAllyIntoPlay pid Allies.squireOfTheCitadel
    run $ Discard squire
    chooseTarget pid =<< hero boromir
    resources boromir `shouldReturn` 1

  it "Pelargir Ship Captain moves a resource after entering play" . dogTest $ \pid -> do
    give boromir 1
    _ <- putAllyIntoPlay pid Allies.pelargirShipCaptain
    chooseAbility pid
    chooseTarget pid =<< hero mablung
    resources mablung `shouldReturn` 1

  it "Emyn Arnen Ranger gets the printed threat of a trapped enemy" . dogTest $ \pid -> do
    eid <- stagedOrcs
    _ <- putAttachmentOn pid Attachments.rangerSpikes (toTarget eid)
    ranger <- putAllyIntoPlay pid Allies.emynArnenRanger
    run $ BeginPhase QuestPhase
    chooseTarget pid ranger
    chooseAbilityOf pid ranger
    getWillpower ranger `shouldReturn` 2

  it "Soldier of Gondor takes a Gondor ally from the top 5 cards" . dogTest $ \pid -> do
    defender <- genPlayerCard Allies.defenderOfRammas (Just pid)
    g <- getGame
    putGame $ g & playersL . ix pid . deckL %~ (PlayerCard defender :)
    _ <- putAllyIntoPlay pid Allies.soldierOfGondor
    chooseAbility pid
    chooseTarget pid (CardIdTarget defender.pcId)
    map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem defender.pcId)

  it "Angbor the Fearless gets +2 attack and quests without exhausting at 40 threat" . dogTest $ \pid -> do
    angbor <- putAllyIntoPlay pid Allies.angborTheFearless
    getAttack angbor `shouldReturn` 1
    threatTo40 pid
    getAttack angbor `shouldReturn` 3
    run $ BeginPhase QuestPhase
    chooseTarget pid angbor
    exhausted angbor `shouldReturn` False

  it "Path of Need lets heroes quest without exhausting at its location" . dogTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
    lid <- getsGame (toId . fromJustNote "no location" . headMay . toList . (.entities.locations))
    run $ PlaceLocation lid ActiveLocation
    _ <- putAttachmentOn pid Attachments.pathOfNeed (toTarget lid)
    b <- hero boromir
    run $ BeginPhase QuestPhase
    chooseTarget pid b
    exhausted b `shouldReturn` False

  it "Behind Strong Walls readies a defending Gondor character with +1 defense" . dogTest $ \pid -> do
    eid <- stagedOrcs
    run $ PlaceEnemy eid (EngagedWith pid)
    walls <- putCardIntoHand pid Events.behindStrongWalls
    give mablung 1
    b <- hero boromir
    run $ EnemyAttack pid eid
    chooseTarget pid b
    chooseTarget pid (CardIdTarget walls.id)
    exhausted b `shouldReturn` False
    getDefense b `shouldReturn` 3

  it "Ranger Spikes waits in staging and traps the next enemy" . dogTest $ \pid -> do
    (spikes, eid) <- trapped pid Attachments.rangerSpikes
    placementOf spikes `shouldReturn` AttachedTo (toTarget eid)
    getEnemyThreat eid `shouldReturn` 0
    run $ EngagementCheck pid 0
    getsGame (fmap (.placement) . lookup eid . (.entities.enemies)) `shouldReturn` Just StagingArea

  it "Ithilien Pit lets characters attack its enemy in the staging area" . dogTest $ \pid -> do
    (_, eid) <- trapped pid Attachments.ithilienPit
    run $ PlayerAttacks pid
    chooseTarget pid =<< hero boromir
    chooseLabel "$label.doneAttacking" pid
    getsGame (fmap (.damage) . lookup eid . (.entities.enemies)) `shouldReturn` Nothing

  it "Gondorian Shield gives a Gondor hero +2 defense" . dogTest $ \pid -> do
    b <- hero boromir
    _ <- putAttachmentOn pid Attachments.gondorianShield b
    getDefense b `shouldReturn` 4

  it "Valiant Sword gives +2 attack at 40 threat" . dogTest $ \pid -> do
    b <- hero boromir
    _ <- putAttachmentOn pid Attachments.valiantSword b
    getAttack b `shouldReturn` 4
    threatTo40 pid
    getAttack b `shouldReturn` 5

  it "Visionary Leadership gives Gondor characters +1 willpower" . dogTest $ \pid -> do
    b <- hero boromir
    _ <- putAttachmentOn pid Attachments.visionaryLeadership b
    i <- hero imrahil
    getWillpower i `shouldReturn` 2
    give boromir 1
    getWillpower i `shouldReturn` 3

  it "Heir of Mardil readies its hero after a card gives it resources" . dogTest $ \pid -> do
    i <- hero imrahil
    heir <- putAttachmentOn pid Attachments.heirOfMardil i
    run $ Exhaust i
    b <- getHeroWithCode boromir
    run $ PlaceTokens (toSource b) i Resource 1
    chooseAbilityOf pid heir
    exhausted i `shouldReturn` False
    exhausted heir `shouldReturn` True

  it "Captain of Gondor gives +1 attack and defense after optionally engaging" . dogTest $ \pid -> do
    b <- hero boromir
    captain <- putAttachmentOn pid Attachments.captainOfGondor b
    eid <- stagedOrcs
    run $ OptionalEngagement pid
    chooseTarget pid eid
    chooseLabel "$label.pass" pid
    chooseAbilityOf pid captain
    getAttack b `shouldReturn` 4
    getDefense b `shouldReturn` 3

  it "Foe-hammer exhausts a Weapon to draw 3 after its hero destroys an enemy" . dogTest $ \pid -> do
    b <- hero boromir
    sword <- putAttachmentOn pid Attachments.valiantSword b
    eid <- stagedOrcs
    run $ PlaceEnemy eid (EngagedWith pid)
    hammer <- putCardIntoHand pid Events.foeHammer
    handBefore <- length . (.hand) <$> getPlayer pid
    run $ PlayerAttacks pid
    chooseTarget pid b
    chooseLabel "$label.doneAttacking" pid
    chooseTarget pid (CardIdTarget hammer.id)
    exhausted sword `shouldReturn` True
    length . (.hand) <$> getPlayer pid `shouldReturn` (handBefore - 1 + 3)

  it "Coney in a Trap stops an engaged enemy attacking this round" . dogTest $ \pid -> do
    _ <- putHero pid Heroes.faramir
    give "06081" 1
    coney <- putCardIntoHand pid Events.coneyInATrap
    eid <- stagedOrcs
    run $ EngageEnemy pid eid
    chooseTarget pid (CardIdTarget coney.id)
    getModifiers eid >>= (`shouldSatisfy` elem CannotAttack)

  it "Pillars of the Kings sets threat to 40 for free and draws a card" . dogTest $ \pid -> do
    run $ RaiseThreat pid 32
    handBefore <- length . (.hand) <$> getPlayer pid
    _ <- playInPlanning pid Events.pillarsOfTheKings
    (.threat) <$> getPlayer pid `shouldReturn` 40
    length . (.hand) <$> getPlayer pid `shouldReturn` (handBefore + 1)

  it "Need Drives Them readies characters at 40 threat" . dogTest $ \pid -> do
    threatTo40 pid
    b <- hero boromir
    i <- hero imrahil
    run $ Exhaust i
    _ <- playWith pid boromir 3 Events.needDrivesThem
    exhausted i `shouldReturn` False
    exhausted b `shouldReturn` False

  it "Helm of Secrecy swaps a hero for one from the collection with its attachments" . dogTest $ \pid -> do
    m <- hero mablung
    shield <- putAttachmentOn pid Attachments.gondorianShield m
    _ <- playWith pid boromir 4 Events.helmOfSecrecy
    chooseTarget pid m
    chooseOptionMatching "swap in Beregond" pid \case
      TargetLabel _ [SwapHero _ c] -> toCardCode c == "05001"
      _ -> False
    getsGame (sort . map toCardCode . toList . (.entities.heroes)) `shouldReturn` sort [boromir, imrahil, "05001"]
    b <- hero "05001"
    placementOf shield `shouldReturn` AttachedTo b
