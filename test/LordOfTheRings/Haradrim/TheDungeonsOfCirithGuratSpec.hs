module LordOfTheRings.Haradrim.TheDungeonsOfCirithGuratSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardType (AllyType), IsCard (toCard))
import LordOfTheRings.Matcher (CardMatcher (CardWithType))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyDefense, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theDungeonsOfCirithGurat)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins, RoundEnds), mkAfter, mkWhen)
import TestImport

dungeonTest :: (PlayerId -> GameT ()) -> IO ()
dungeonTest = gameTestIn theDungeonsOfCirithGurat testDeck

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT (Maybe Placement)
placementOfEnemy eid = getsGame (fmap (.placement) . lookup eid . (.entities.enemies))

cardIdOfEnemy :: EnemyId -> GameT CardId
cardIdOfEnemy eid = getsGame (fromJustNote "no such enemy" . fmap ((.id) . toCard) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

cardIdOfLocation :: LocationId -> GameT CardId
cardIdOfLocation lid = getsGame (fromJustNote "no such location" . fmap ((.id) . toCard) . lookup lid . (.entities.locations))

allyOf :: CardDef -> GameT (Maybe AllyAttrs)
allyOf def = getsGame (fmap toAttrs . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

placementOfAlly :: Target -> GameT (Maybe Placement)
placementOfAlly = \case
  AllyTarget aid -> getsGame (fmap (allyPlacement . toAttrs) . lookup aid . (.entities.allies))
  _ -> pure Nothing

questOf :: CardDef -> GameT Target
questOf def = getsGame (toTarget . fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questsInPlay :: GameT [CardCode]
questsInPlay = getsGame (map toCardCode . toList . (.entities.quests))

setAside :: GameT [CardCode]
setAside = getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | The cards on top of the player's deck, the first his next card.
topDeck :: PlayerId -> [CardDef] -> GameT [Card]
topDeck pid defs = do
  cards <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) defs
  putGame . (playersL . ix pid . deckL %~ (cards <>)) =<< getGame
  pure cards

discardPile :: PlayerId -> GameT [CardCode]
discardPile pid = map toCardCode . (.discard) <$> getPlayer pid

-- | An ally of the player's in play, made the captive of the encounter card.
captiveOf :: PlayerId -> CardId -> GameT Target
captiveOf pid captor = do
  guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
  run $ GuardCharacter guardian captor
  pure guardian

withCaptives :: GameT ()
withCaptives = run . AddToStaging =<< genEncounterCard Objectives.theCaptivesOfCirithGurat

chooseAnyTarget :: HasCallStack => PlayerId -> GameT ()
chooseAnyTarget pid = chooseOptionMatching "choose a target" pid \case
  TargetLabel {} -> True
  _ -> False

spec :: Spec
spec = describe "The Dungeons of Cirith Gurat" do
  it "is a scenario the game can be dealt" . dungeonTest $ \_ ->
    isJust (lookupScenario "the-dungeons-of-cirith-gurat") `shouldBe` True

  it "sets Uruk Chieftain aside and stages the Captives, a Dungeon Cell guarding Abaan, an Orc, and the first ally of the player's deck guarded by it" . dungeonTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    _ <- topDeck pid [Events.theGaladhrimsGreeting, Allies.guardOfTheCitadel]
    chooseOptionMatching "flip the quest" pid (const True)
    -- the player's search for an Orc enemy
    chooseAnyTarget pid
    setAside `shouldReturn` [toCardCode Enemies.urukChieftain]
    getsGame (map toCardCode . toList . (.entities.objectives)) `shouldReturn` [toCardCode Objectives.theCaptivesOfCirithGurat]
    cell <- locationOf Locations.dungeonCell
    cellId <- cardIdOfLocation cell
    fmap allyPlacement <$> allyOf Objectives.abaan `shouldReturn` Just (AttachedTo (CardIdTarget cellId))
    -- Abaan's printed willpower 1
    getLocationThreat cell `shouldReturn` 2
    orcs <- getsGame (toList . (.entities.enemies))
    map (.placement) orcs `shouldBe` [StagingArea]
    fmap allyPlacement <$> allyOf Allies.guardOfTheCitadel `shouldReturn` Just (AttachedTo (CardIdTarget (fromJustNote "no orc" $ headMay [(toCard o).id | o <- orcs])))
    discardPile pid `shouldReturn` [toCardCode Events.theGaladhrimsGreeting]
    deck <- map toCardCode <$> getEncounterDeck
    length (filter (== toCardCode Locations.dungeonCell) deck) `shouldBe` 2
    deck `shouldSatisfy` all (`notElem` map toCardCode [Objectives.abaan, Objectives.theCaptivesOfCirithGurat, Enemies.urukChieftain])
    (.flipped) <$> getActiveQuest `shouldReturn` True

  describe "captives" do
    it "discards a captive's tokens, keeps it from every ally select, and rescues it exhausted as its guard leaves play" . dungeonTest $ \pid -> do
      withCaptives
      uruk <- stage Enemies.urukOfMordor
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ DealDamage GameSource guardian 1
      run . GuardCharacter guardian =<< cardIdOfEnemy uruk
      getDamage guardian `shouldReturn` 0
      getsGame (length . (.entities.allies)) `shouldReturn` 1
      run $ DealDamage GameSource (toTarget uruk) 4
      placementOfAlly guardian `shouldReturn` Just (InPlayArea pid)
      fmap allyExhausted <$> allyOf Allies.guardOfTheCitadel `shouldReturn` Just True

    it "bars Rescue the Prisoners from being defeated while a captive is in play, and reveals an encounter card as captives are rescued" . dungeonTest $ \pid -> do
      q <- putQuestStage Quests.rescueThePrisoners
      run $ Flip q
      uruk <- stage Enemies.urukOfMordor
      _ <- captiveOf pid =<< cardIdOfEnemy uruk
      run $ PlaceTokens GameSource q Progress 10
      questsInPlay `shouldReturn` [toCardCode Quests.rescueThePrisoners]
      _ <- setEncounterDeck [Locations.gateOfIron]
      run $ DealDamage GameSource (toTarget uruk) 4
      _ <- locationOf Locations.gateOfIron
      -- with no captive left, the stage is defeated: nothing follows it in this quest deck
      getsGame (.outcome) `shouldReturn` Just Won

    it "puts Abaan under the first player as he is rescued, and the players lose if he leaves play" . dungeonTest $ \pid -> do
      cell <- stageLocation Locations.dungeonCell
      cellCard <- getsGame (fromJustNote "no cell" . fmap toCard . lookup cell . (.entities.locations))
      abaan <- genEncounterCard Objectives.abaan
      run $ GuardCard (EncounterCard abaan) cellCard.id
      run $ Discard (toTarget cell)
      fmap allyPlacement <$> allyOf Objectives.abaan `shouldReturn` Just (InPlayArea pid)
      a <- fromJustNote "no Abaan" <$> allyOf Objectives.abaan
      run $ Discard (AllyTarget a.allyId)
      getsGame (.outcome) `shouldReturn` Just Lost

    it "lets Abaan, exhausted, give a Harad character +1 willpower, attack and defense this round" . dungeonTest $ \pid -> do
      cell <- stageLocation Locations.dungeonCell
      cellId <- cardIdOfLocation cell
      run . (`GuardCard` cellId) . EncounterCard =<< genEncounterCard Objectives.abaan
      run $ Discard (toTarget cell)
      a <- fromJustNote "no Abaan" <$> allyOf Objectives.abaan
      run $ Ready (AllyTarget a.allyId)
      run $ Exhaust (AllyTarget a.allyId)
      chooseAbilityOf pid (AllyTarget a.allyId)
      getWillpower (AllyTarget a.allyId) `shouldReturn` 2

  describe "enemies" do
    it "has Jailer of Cirith Gurat take an ally of the player it engages" . dungeonTest $ \pid -> do
      withCaptives
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      jailer <- engaged pid Enemies.jailerOfCirithGurat
      cid <- cardIdOfEnemy jailer
      placementOfAlly guardian `shouldReturn` Just (AttachedTo (CardIdTarget cid))

    it "has Jailer of Cirith Gurat's shadow take the defending ally of less willpower than the attacker's threat, canceling the attack" . dungeonTest $ \pid -> do
      withCaptives
      guard' <- engaged pid Enemies.prisonGuardCirithGurat
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Enemies.jailerOfCirithGurat]
      run $ DealShadowCard guard'
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ EnemyAttacks pid
      passActions pid
      chooseTarget pid guardian
      passActions pid
      cid <- cardIdOfEnemy guard'
      placementOfAlly guardian `shouldReturn` Just (AttachedTo (CardIdTarget cid))
      getDamage guardian `shouldReturn` 0

    it "gives Prison Guard +2 defense while it guards, and has it take an ally its attack damages while it guards none" . dungeonTest $ \pid -> do
      withCaptives
      guard' <- engaged pid Enemies.prisonGuardCirithGurat
      getEnemyDefense guard' `shouldReturn` 1
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      -- its 4 attack deals the ally 1 damage, once the attack has resolved it takes it
      run $ CreateEffect $ Effect GameSource guardian [DefenseModifier 3] UntilEndOfRound
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ EnemyAttacks pid
      passActions pid
      chooseTarget pid guardian
      passActions pid
      cid <- cardIdOfEnemy guard'
      placementOfAlly guardian `shouldReturn` Just (AttachedTo (CardIdTarget cid))
      getEnemyDefense guard' `shouldReturn` 3

    it "has Dungeon Warg attack the player with the most allies as it is revealed" . dungeonTest $ \pid -> do
      reveal [Enemies.dungeonWarg, Locations.gateOfIron]
      -- its attack: the player declares a defender or not
      getQuestion pid >>= (`shouldSatisfy` isJust)

  describe "locations" do
    it "lets no progress be placed on locations in the staging area while Gate of Iron is there" . dungeonTest $ \_ -> do
      gate <- stageLocation Locations.gateOfIron
      cell <- stageLocation Locations.dungeonCell
      for_ [gate, cell] \l -> getModifiers (LocationTarget l) >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "has the first player discard a costlier card or each player raise his threat by 3 to travel to Gate of Iron" . dungeonTest $ \pid -> do
      gate <- stageLocation Locations.gateOfIron
      -- Mines of Cirith Gurat's threat is 2: the event costs 3
      _ <- setEncounterDeck [Locations.minesOfCirithGurat]
      _ <- putCardIntoHand pid Events.theGaladhrimsGreeting
      was <- threatOf pid
      run $ TravelTo gate
      chooseLabel "$label.gateOfIron.raiseThreat" pid
      threatOf pid `shouldReturn` (was + 3)

    it "attaches an ally destroyed to Dungeon Cell in the staging area, its threat raised by the ally's willpower" . dungeonTest $ \pid -> do
      withCaptives
      cell <- stageLocation Locations.dungeonCell
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ DealDamage GameSource guardian 2
      cid <- cardIdOfLocation cell
      fmap allyPlacement <$> allyOf Allies.guardOfTheCitadel `shouldReturn` Just (AttachedTo (CardIdTarget cid))
      discardPile pid `shouldReturn` []
      getLocationThreat cell `shouldReturn` 2

    it "raises the cost of allies while Uruk Guardroom is in the staging area, and adds an Orc to the staging area to travel there" . dungeonTest $ \_ -> do
      room <- stageLocation Locations.urukGuardroom
      getModifiers GameTarget >>= (`shouldSatisfy` elem (IncreaseCost (CardWithType AllyType) 1))
      _ <- setEncounterDeck [Locations.gateOfIron, Enemies.urukOfMordor]
      run $ TravelTo room
      uruk <- enemyOf Enemies.urukOfMordor
      placementOfEnemy uruk `shouldReturn` Just StagingArea

    it "reduces a player card's threat reduction to 1 while Mines of Cirith Gurat is in the staging area" . dungeonTest $ \pid -> do
      _ <- stageLocation Locations.minesOfCirithGurat
      run $ RaiseThreat pid 20
      was <- threatOf pid
      _ <- playWith pid "01007" 3 Events.theGaladhrimsGreeting
      chooseLabel "$label.theGaladhrimsGreeting.one" pid
      threatOf pid `shouldReturn` (was - 1)

    it "has each player raise his threat by 1 for each ally of the top 3 of his deck to travel to Mines of Cirith Gurat" . dungeonTest $ \pid -> do
      mines <- stageLocation Locations.minesOfCirithGurat
      _ <- topDeck pid [Allies.guardOfTheCitadel, Events.theGaladhrimsGreeting, Allies.guardOfTheCitadel]
      was <- threatOf pid
      run $ TravelTo mines
      threatOf pid `shouldReturn` (was + 2)

    it "attaches the first ally of the first player's deck to Torture Room, and raises each threat at the end of the round while it guards" . dungeonTest $ \pid -> do
      withCaptives
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- topDeck pid [Events.theGaladhrimsGreeting, Allies.guardOfTheCitadel]
      reveal [Locations.tortureRoom]
      room <- locationOf Locations.tortureRoom
      cid <- cardIdOfLocation room
      allies <- getsGame (map toAttrs . toList . (.entities.allies))
      map allyPlacement allies `shouldBe` [AttachedTo (CardIdTarget cid)]
      -- the copy in play is discarded
      discardPile pid >>= (`shouldSatisfy` elem (toCardCode Allies.guardOfTheCitadel))
      was <- threatOf pid
      run $ CheckWindows [mkWhen RoundEnds]
      threatOf pid `shouldReturn` (was + 1)

  describe "treacheries" do
    it "has Taken Prisoner attach the first player's ally to an unattached card in the staging area" . dungeonTest $ \pid -> do
      withCaptives
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      gate <- stageLocation Locations.gateOfIron
      reveal [Treacheries.takenPrisoner]
      cid <- cardIdOfLocation gate
      placementOfAlly guardian `shouldReturn` Just (AttachedTo (CardIdTarget cid))

    it "gives Taken Prisoner doomed 2 when the first player has no ally" . dungeonTest $ \pid -> do
      _ <- stageLocation Locations.gateOfIron
      was <- threatOf pid
      reveal [Treacheries.takenPrisoner]
      threatOf pid `shouldReturn` (was + 2)

    it "raises each player's threat by the guarded objectives he owns for Tormented by Orcs" . dungeonTest $ \pid -> do
      withCaptives
      uruk <- stage Enemies.urukOfMordor
      _ <- captiveOf pid =<< cardIdOfEnemy uruk
      was <- threatOf pid
      reveal [Treacheries.tormentedByOrcs]
      threatOf pid `shouldReturn` (was + 1)

    it "surges Tormented by Orcs when a player owns no guarded objective" . dungeonTest $ \_ -> do
      reveal [Treacheries.tormentedByOrcs, Locations.gateOfIron]
      _ <- locationOf Locations.gateOfIron
      pure ()

  describe "Captured!" do
    it "takes an ally of each player, and returns it as the stage is defeated, the player's threat reduced by its cost" . dungeonTest $ \pid -> do
      withCaptives
      _ <- putQuestStage Quests.fightYourWayOut
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      reveal [Quests.capturedCirithGurat]
      side <- questOf Quests.capturedCirithGurat
      sideId <- getsGame (fromJustNote "no such quest" . fmap ((.id) . toCard) . find ((== toCardCode Quests.capturedCirithGurat) . toCardCode) . toList . (.entities.quests))
      placementOfAlly guardian `shouldReturn` Just (AttachedTo (CardIdTarget sideId))
      run $ RaiseThreat pid 10
      was <- threatOf pid
      run $ PlaceTokens GameSource side Progress 5
      threatOf pid `shouldReturn` (was - 2)
      placementOfAlly guardian `shouldReturn` Just (InPlayArea pid)

    it "is discarded, and reveals another card, when no ally is attached to it" . dungeonTest $ \_ -> do
      _ <- putQuestStage Quests.fightYourWayOut
      reveal [Quests.capturedCirithGurat, Locations.gateOfIron]
      questsInPlay `shouldReturn` [toCardCode Quests.fightYourWayOut]
      _ <- locationOf Locations.gateOfIron
      pure ()

  describe "stages" do
    it "adds an enemy to the staging area for each player as Sneak Past the Guards is revealed" . dungeonTest $ \pid -> do
      q <- putQuestStage Quests.sneakPastTheGuards
      _ <- setEncounterDeck [Locations.gateOfIron, Enemies.urukOfMordor]
      run $ Flip q
      resolveWhenRevealed pid
      uruk <- enemyOf Enemies.urukOfMordor
      placementOfEnemy uruk `shouldReturn` Just StagingArea

    it "shields enemies in the staging area while Sneak Past the Guards is in play, raising their engagement cost by its progress" . dungeonTest $ \pid -> do
      q <- putQuestStage Quests.sneakPastTheGuards
      _ <- setEncounterDeck [Enemies.urukOfMordor]
      run $ Flip q
      resolveWhenRevealed pid
      uruk <- enemyOf Enemies.urukOfMordor
      run $ PlaceTokens GameSource q Progress 3
      getEngagementCost uruk `shouldReturn` 38
      run $ DealDamage GameSource (toTarget uruk) 2
      getDamage (toTarget uruk) `shouldReturn` 0

    it "reveals an encounter card at the end of the round, placing a resource on Sneak Past the Guards if no enemy engaged" . dungeonTest $ \pid -> do
      q <- putQuestStage Quests.sneakPastTheGuards
      _ <- setEncounterDeck []
      run $ Flip q
      resolveWhenRevealed pid
      _ <- setEncounterDeck [Locations.gateOfIron, Locations.gateOfIron]
      runOnly EndRound
      void mainLoop
      _ <- locationOf Locations.gateOfIron
      getsGame (fmap (countTokens Resource . questTokens . toAttrs) . find ((== toCardCode Quests.sneakPastTheGuards) . toCardCode) . toList . (.entities.quests))
        `shouldReturn` Just 1

    it "cannot defeat Sneak Past the Guards until it has 2 resources" . dungeonTest $ \pid -> do
      q <- putQuestStage Quests.sneakPastTheGuards
      _ <- setEncounterDeck []
      run $ Flip q
      resolveWhenRevealed pid
      run $ PlaceTokens GameSource q Resource 1
      run $ PlaceTokens GameSource q Progress 15
      questsInPlay `shouldReturn` [toCardCode Quests.sneakPastTheGuards]
      run $ PlaceTokens GameSource q Resource 1
      -- defeated, with nothing after it in this quest deck
      getsGame (.outcome) `shouldReturn` Just Won

    it "adds Uruk Chieftain, takes a hero of each player captive and engages the unguarded enemies as Fight Your Way Out is revealed" . dungeonTest $ \pid -> do
      withCaptives
      run . SetAside =<< genEncounterCard Enemies.urukChieftain
      uruk <- stage Enemies.urukOfMordor
      q <- putQuestStage Quests.fightYourWayOut
      run $ Flip q
      resolveWhenRevealed pid
      beravor <- hero "01012"
      chooseTarget pid beravor
      chieftain <- enemyOf Enemies.urukChieftain
      chooseTarget pid (toTarget chieftain)
      h <- getHeroWithCode "01012"
      cid <- cardIdOfEnemy chieftain
      heroGuardedBy (toAttrs h) `shouldBe` Just cid
      placementOfEnemy uruk `shouldReturn` Just (EngagedWith pid)
      placementOfEnemy chieftain `shouldReturn` Just StagingArea

    it "adds an enemy at the beginning of the quest phase, and wins once no captive is left" . dungeonTest $ \pid -> do
      q <- putQuestStage Quests.fightYourWayOut
      run $ Flip q
      resolveWhenRevealed pid
      _ <- setEncounterDeck [Locations.gateOfIron, Enemies.urukOfMordor]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      uruk <- enemyOf Enemies.urukOfMordor
      placementOfEnemy uruk `shouldReturn` Just StagingArea
      run $ PlaceTokens GameSource q Progress 10
      getsGame (.outcome) `shouldReturn` Just Won
