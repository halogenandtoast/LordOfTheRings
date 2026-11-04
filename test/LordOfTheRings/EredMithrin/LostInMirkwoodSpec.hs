module LordOfTheRings.EredMithrin.LostInMirkwoodSpec (spec) where

import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (DolGuldurOrcs, SpidersOfMirkwood))
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, lostInMirkwood)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), encounterDiscardL, questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds, RoundEnds), mkAfter, mkWhen)
import TestImport

mirkwoodTest :: (PlayerId -> GameT ()) -> IO ()
mirkwoodTest = gameTestIn lostInMirkwood testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  enemyOf card

enemyOf :: EncounterCard -> GameT EnemyId
enemyOf card = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOf :: LocationId -> GameT Placement
placementOf lid = (.placement) <$> getLocation lid

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

questDeckCodes :: GameT [CardCode]
questDeckCodes = map toCardCode . scenarioQuestDeck <$> scenarioAttrs

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

setSetAside :: [CardDef] -> GameT ()
setSetAside defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

clearDiscard :: GameT ()
clearDiscard = do
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [])

-- | The stage put into play flipped, the given stages behind it in the quest deck.
atStage :: CardDef -> [CardDef] -> GameT Target
atStage def rest = do
  q <- putQuestStage def
  g <- getGame
  putGame =<< preloadModifiers (g & entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True}))
  setQuestDeck rest
  pure q

laterStages :: [CardDef]
laterStages =
  [ Quests.spidersOfMirkwoodStage
  , Quests.dolGuldurOrcsStage
  , Quests.carriedAway
  , Quests.trulyLost
  , Quests.theForestOfGreatFear
  , Quests.escapeFromTaurNuFuin
  ]

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "Lost in Mirkwood" do
  it "is a scenario the game can be dealt" . mirkwoodTest $ \_ ->
    isJust (lookupScenario "lost-in-mirkwood") `shouldBe` True

  describe "setup" do
    it "gives the first player Haldan, adds Searching for a Way Out and a Forest Spider, and sets two sets aside" . mirkwoodTest $ \pid -> do
      flipFirstQuest pid
      activeQuestCode `shouldReturn` toCardCode Quests.ambushedAtNight
      allies <- getsGame (map toCardCode . toList . (.entities.allies))
      allies `shouldBe` [toCardCode Objectives.haldan]
      objectives <- getsGame (map toCardCode . toList . (.entities.objectives))
      objectives `shouldBe` [toCardCode Objectives.searchingForAWayOut]
      enemies <- getsGame (map toCardCode . toList . (.entities.enemies))
      enemies `shouldBe` [toCardCode Enemies.forestSpider]
      attrs <- scenarioAttrs
      all ((`elem` [Just SpidersOfMirkwood, Just DolGuldurOrcs]) . cdEncounterSet . toCardDef) attrs.scenarioSetAside `shouldBe` True
      notNull attrs.scenarioSetAside `shouldBe` True
      any ((`elem` [Just SpidersOfMirkwood, Just DolGuldurOrcs]) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` False
      questDeckCodes `shouldReturn` map toCardCode laterStages

  describe "Ambushed at Night" do
    it "raises the threat of a player an enemy engages, and stops player cards reducing threat" . mirkwoodTest $ \pid -> do
      _ <- atStage Quests.ambushedAtNight []
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (PlayerCardThreatReductionLimit 0))
      threat0 <- threatOf pid
      eid <- stage Enemies.kingSpider
      run $ EngageEnemy pid eid
      threatOf pid `shouldReturn` (threat0 + 1)

  describe "Searching for a Way Out" do
    it "discards encounter cards for progress while no objective-location is in play, then puts the topmost one discarded into play guarded" . mirkwoodTest $ \_ -> do
      q <- atStage Quests.ambushedAtNight []
      run . AddToStaging =<< genEncounterCard Objectives.searchingForAWayOut
      clearDiscard
      _ <- setEncounterDeck [Locations.twilightHall, Locations.overgrownPath, Locations.accursedForest]
      run $ PlaceTokens GameSource q Progress 2
      (.progress) <$> getActiveQuest `shouldReturn` 0
      path <- locationOf Locations.overgrownPath
      forest <- locationOf Locations.accursedForest
      forestCard <- (.cardId) . toAttrs <$> getLocation forest
      placementOf path `shouldReturn` AttachedTo (CardIdTarget forestCard)
      placementOf forest `shouldReturn` StagingArea
      getModifiers path >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getModifiers forest >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      -- with one in play, progress is placed on the stage
      run $ PlaceTokens GameSource q Progress 2
      (.progress) <$> getActiveQuest `shouldReturn` 2

  describe "objective-locations" do
    it "are guarded by the first enemy discarded as they are revealed, and go free as it leaves play" . mirkwoodTest $ \_ -> do
      _ <- setEncounterDeck [Locations.bareHilltop, Locations.twilightHall, Enemies.forestSpider]
      run RevealEncounterCard
      resolveRevealed
      hilltop <- locationOf Locations.bareHilltop
      spider <- getsGame (fromJustNote "no spider" . headMay . toList . (.entities.enemies))
      placementOf hilltop `shouldReturn` AttachedTo (CardIdTarget (toAttrs spider).cardId)
      run $ DealDamage GameSource (toTarget spider) 10
      placementOf hilltop `shouldReturn` StagingArea

    it "advance to the next stage A as the players travel to Overgrown Path, the other version leaving the quest deck" . mirkwoodTest $ \_ -> do
      _ <- atStage Quests.ambushedAtNight laterStages
      _ <- setEncounterDeck []
      clearDiscard
      path <- stageLocation Locations.overgrownPath
      _ <- setEncounterDeck [Treacheries.vastnessOfMirkwood]
      run $ TravelTo path
      activeQuestCode `shouldReturn` toCardCode Quests.spidersOfMirkwoodStage
      questDeckCodes `shouldReturn` map toCardCode [Quests.carriedAway, Quests.trulyLost, Quests.theForestOfGreatFear, Quests.escapeFromTaurNuFuin]

    it "advance to the stage numbered 1 higher, of the side named: from 2A, Abandoned Village goes to 3C" . mirkwoodTest $ \_ -> do
      _ <- atStage Quests.spidersOfMirkwoodStage (drop 2 laterStages)
      _ <- setEncounterDeck []
      clearDiscard
      village <- stageLocation Locations.abandonedVillage
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ TravelTo village
      activeQuestCode `shouldReturn` toCardCode Quests.trulyLost
      questDeckCodes `shouldReturn` map toCardCode [Quests.theForestOfGreatFear, Quests.escapeFromTaurNuFuin]
      -- the enemy discarded by its Travel cost went to the staging area
      getsGame (length . (.entities.enemies)) `shouldReturn` 1

    it "deals Overgrown Path's discarded card's threat as damage as the players travel there" . mirkwoodTest $ \pid -> do
      _ <- atStage Quests.ambushedAtNight []
      _ <- setEncounterDeck []
      clearDiscard
      path <- stageLocation Locations.overgrownPath
      _ <- setEncounterDeck [Locations.twilightHall]
      run $ TravelTo path
      h <- heroOf pid "01002"
      chooseTarget pid h
      chooseTarget pid h
      getCharacterDamage h `shouldReturn` 2

    it "raises each player's threat by Forest Clearing's discarded card's threat" . mirkwoodTest $ \pid -> do
      _ <- atStage Quests.ambushedAtNight []
      _ <- setEncounterDeck []
      clearDiscard
      clearing <- stageLocation Locations.forestClearingLostInMirkwood
      _ <- setEncounterDeck [Locations.accursedForest]
      threat0 <- threatOf pid
      run $ TravelTo clearing
      threatOf pid `shouldReturn` (threat0 + 4)

  describe "Twilight Hall" do
    it "takes a resource as a Forest location is explored, gets +2 threat for each, and cannot be traveled to without one" . mirkwoodTest $ \pid -> do
      hall <- stageLocation Locations.twilightHall
      getModifiers hall >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      getLocationThreat hall `shouldReturn` 2
      woods <- stageLocation Locations.darkBlackWoods
      run $ PlaceLocation woods ActiveLocation
      run $ PlaceTokens GameSource (toTarget woods) Progress 3
      getLocationThreat hall `shouldReturn` 4
      getModifiers hall >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      threat0 <- threatOf pid
      run $ TravelTo hall
      threatOf pid `shouldReturn` (threat0 + 1)

  describe "Mirkwood Patrol" do
    it "guards an unguarded objective-location in the staging area at the end of the encounter phase" . mirkwoodTest $ \_ -> do
      _ <- setEncounterDeck []
      clearDiscard
      village <- stageLocation Locations.abandonedVillage
      patrol <- genEncounterCard Enemies.mirkwoodPatrol
      run $ AddToStaging patrol
      run $ CheckWindows [mkWhen $ PhaseEnds EncounterPhase]
      placementOf village `shouldReturn` AttachedTo (CardIdTarget patrol.id)

  describe "Ravenous Spider" do
    it "makes an immediate attack after it engages you" . mirkwoodTest $ \pid -> do
      eid <- stage Enemies.ravenousSpider
      _ <- setEncounterDeck [Treacheries.vastnessOfMirkwood]
      run $ EngageEnemy pid eid
      q <- getQuestion pid
      isJust q `shouldBe` True

  describe "Unseen Danger" do
    it "discards a committed character whose willpower the discarded card's threat beats" . mirkwoodTest $ \pid -> do
      h <- heroOf pid "01002"
      run $ CommitToQuest h
      _ <- setEncounterDeck [Treacheries.unseenDanger, Locations.accursedForest]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid h
      getsGame (any ((== "01002") . toCardCode) . (.entities.heroes)) `shouldReturn` False

  describe "Vastness of Mirkwood" do
    it "raises non-objective locations' threat for the phase and surges with too few of them" . mirkwoodTest $ \_ -> do
      woods <- stageLocation Locations.darkBlackWoods
      run $ PlaceTokens GameSource (toTarget woods) Progress 1
      _ <- setEncounterDeck [Treacheries.vastnessOfMirkwood, Locations.twilightHall]
      run RevealEncounterCard
      resolveRevealed
      getLocationThreat woods `shouldReturn` 5
      -- 1 location for 1 player: no surge
      getsGame (any ((== toCardCode Locations.twilightHall) . toCardCode) . (.entities.locations)) `shouldReturn` False

  describe "Spiders of Mirkwood" do
    it "shuffles the Spiders of Mirkwood set in and reveals a card per player" . mirkwoodTest $ \pid -> do
      setSetAside [Enemies.kingSpider, Enemies.chieftanUfthak]
      _ <- setEncounterDeck []
      q <- putQuestStage Quests.spidersOfMirkwoodStage
      run $ Flip q
      resolveWhenRevealed pid
      resolveRevealed
      enemies <- getsGame (map toCardCode . toList . (.entities.enemies))
      enemies `shouldBe` [toCardCode Enemies.kingSpider]
      map toCardCode . scenarioSetAside <$> scenarioAttrs `shouldReturn` [toCardCode Enemies.chieftanUfthak]

  describe "Carried Away" do
    it "puts a hero in the staging area guarded by an enemy, returns it exhausted as the enemy is defeated" . mirkwoodTest $ \pid -> do
      h <- heroOf pid "01002"
      run $ PlaceTokens GameSource h Resource 2
      q <- putQuestStage Quests.carriedAway
      _ <- setEncounterDeck [Locations.twilightHall, Enemies.forestSpider]
      run $ Flip q
      resolveWhenRevealed pid
      chooseTarget pid h
      hero <- getHeroWithCode "01002"
      isJust (heroGuardedBy $ toAttrs hero) `shouldBe` True
      hero.resources `shouldBe` 0
      spider <- getsGame (fromJustNote "no spider" . headMay . toList . (.entities.enemies))
      getModifiers (toTarget spider) >>= (`shouldSatisfy` elem CannotTakeNonCombatDamage)
      run CheckEliminations
      (.eliminated) <$> getPlayer pid `shouldReturn` False
      run $ Destroy GameSource (toTarget spider)
      hero' <- toAttrs <$> getHeroWithCode "01002"
      (isJust hero'.heroGuardedBy, hero'.heroExhausted) `shouldBe` (False, True)

    it "frees a hero, ready, whose guard leaves play without being defeated" . mirkwoodTest $ \pid -> do
      q <- putQuestStage Quests.carriedAway
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ Flip q
      resolveWhenRevealed pid
      h <- heroOf pid "01002"
      chooseTarget pid h
      spider <- getsGame (fromJustNote "no spider" . headMay . toList . (.entities.enemies))
      run $ ShuffleIntoEncounterDeck (toTarget spider)
      hero <- toAttrs <$> getHeroWithCode "01002"
      (isJust hero.heroGuardedBy, hero.heroExhausted) `shouldBe` (False, False)

    it "discards each guarded hero as the players advance from it" . mirkwoodTest $ \pid -> do
      q <- putQuestStage Quests.carriedAway
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ Flip q
      resolveWhenRevealed pid
      h <- heroOf pid "01002"
      chooseTarget pid h
      setQuestDeck [Quests.theForestOfGreatFear, Quests.escapeFromTaurNuFuin]
      run $ ScenarioSpecific "lostInMirkwood.advance" (toJSON False)
      activeQuestCode `shouldReturn` toCardCode Quests.escapeFromTaurNuFuin
      getsGame (any ((== "01002") . toCardCode) . (.entities.heroes)) `shouldReturn` False

  describe "Truly Lost" do
    it "adds a non-objective location, puts the hand under it, and returns the hand as it is explored" . mirkwoodTest $ \pid -> do
      _ <- putCardIntoHand pid Events.quickStrike
      _ <- putCardIntoHand pid Events.quickStrike
      hand <- (.hand) <$> getPlayer pid
      q <- putQuestStage Quests.trulyLost
      _ <- setEncounterDeck [Locations.overgrownPath, Enemies.forestSpider, Locations.darkBlackWoods]
      run $ Flip q
      resolveWhenRevealed pid
      woods <- locationOf Locations.darkBlackWoods
      (.hand) <$> getPlayer pid `shouldReturn` []
      length . (.underneath) . toAttrs <$> getLocation woods `shouldReturn` length hand
      getModifiers woods >>= (`shouldSatisfy` elem (QuestPointModifier (length hand)))
      run $ PlaceLocation woods ActiveLocation
      run $ PlaceTokens GameSource (toTarget woods) Progress (3 + length hand)
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` map (.id) hand

    it "discards the cards under locations as the players advance from it" . mirkwoodTest $ \pid -> do
      _ <- putCardIntoHand pid Events.quickStrike
      q <- putQuestStage Quests.trulyLost
      _ <- setEncounterDeck [Locations.darkBlackWoods]
      run $ Flip q
      resolveWhenRevealed pid
      handBefore <- length . (.discard) <$> getPlayer pid
      woods <- locationOf Locations.darkBlackWoods
      under <- length . (.underneath) . toAttrs <$> getLocation woods
      setQuestDeck [Quests.theForestOfGreatFear, Quests.escapeFromTaurNuFuin]
      run $ ScenarioSpecific "lostInMirkwood.advance" (toJSON True)
      length . (.discard) <$> getPlayer pid `shouldReturn` (handBefore + under)
      length . (.underneath) . toAttrs <$> getLocation woods `shouldReturn` 0

  describe "The Forest of Great Fear" do
    it "makes the set-aside enemy with the most hit points the nemesis, and the players win as it is defeated" . mirkwoodTest $ \pid -> do
      setSetAside [Enemies.dolGuldurOrcs, Enemies.chieftanUfthak, Enemies.dolGuldurBeastmaster]
      run . AddToStaging =<< genEncounterCard Objectives.searchingForAWayOut
      q <- putQuestStage Quests.theForestOfGreatFear
      run $ Flip q
      resolveWhenRevealed pid
      getsGame (length . (.entities.objectives)) `shouldReturn` 0
      ufthak <- getsGame (fromJustNote "no nemesis" . headMay . toList . (.entities.enemies))
      toCardCode ufthak `shouldBe` toCardCode Enemies.chieftanUfthak
      mods <- getModifiers (toTarget ufthak)
      (ImmuneToPlayerCardEffects `elem` mods, CannotTakeDamage `elem` mods) `shouldBe` (True, True)
      getEnemyAttack (toId ufthak) `shouldReturn` 5
      getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
      run $ PlaceTokens GameSource q Progress 8
      getModifiers (toTarget ufthak) >>= (`shouldSatisfy` notElem CannotTakeDamage)
      run $ Destroy GameSource (toTarget ufthak)
      getsGame (.outcome) `shouldReturn` Just Won

    it "returns the nemesis to the staging area at the end of the round to attack the first player" . mirkwoodTest $ \pid -> do
      setSetAside [Enemies.ungoliantsSpawn]
      q <- putQuestStage Quests.theForestOfGreatFear
      run $ Flip q
      resolveWhenRevealed pid
      spawn <- getsGame (fromJustNote "no nemesis" . headMay . toList . (.entities.enemies))
      run $ EngageEnemy pid (toId spawn)
      _ <- setEncounterDeck [Treacheries.vastnessOfMirkwood]
      run $ CheckWindows [mkAfter RoundEnds]
      (.placement) <$> getEnemy (toId spawn) `shouldReturn` StagingArea

  describe "Escape from Taur-nu-Fuin" do
    it "engages each player with an enemy, cannot be defeated while one is engaged, and raises threat for each at the end of the round" . mirkwoodTest $ \pid -> do
      q <- putQuestStage Quests.escapeFromTaurNuFuin
      _ <- setEncounterDeck [Locations.twilightHall, Enemies.forestSpider]
      run $ Flip q
      resolveWhenRevealed pid
      spider <- getsGame (fromJustNote "no spider" . headMay . toList . (.entities.enemies))
      (.placement) <$> getEnemy (toId spider) `shouldReturn` EngagedWith pid
      getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
      threat0 <- threatOf pid
      run $ CheckWindows [mkAfter RoundEnds]
      threatOf pid `shouldReturn` (threat0 + 1)
