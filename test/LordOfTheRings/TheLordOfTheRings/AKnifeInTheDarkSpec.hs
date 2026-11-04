module LordOfTheRings.TheLordOfTheRings.AKnifeInTheDarkSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (..), PoolCard (..), newCampaignLog)
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing)
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (aKnifeInTheDark, lookupScenario)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

knifeTest :: (PlayerId -> GameT ()) -> IO ()
knifeTest = gameTestIn aKnifeInTheDark testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

campaignTest :: (PlayerId -> GameT ()) -> IO ()
campaignTest = campaignTestIn aKnifeInTheDark lotr testDeck

-- | Setup, The Black Riders' Frodo Baggins taken should the game offer several
-- Ring-bearers, the starting hand kept and stage 1A clicked.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  getQuestion pid >>= \case
    Just (QuestionLabel l _ _ _) | "$label.saga.chooseFellowshipHero" `isPrefixOf` l -> void $ answerQuestion pid 0
    _ -> pure ()
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

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

enemiesOf :: CardDef -> GameT [EnemyId]
enemiesOf def = getsGame (map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

enemiesEngagedWith :: PlayerId -> GameT Int
enemiesEngagedWith pid = getsGame (length . filter ((== EngagedWith pid) . (.placement)) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationAttrs :: LocationId -> GameT LocationAttrs
locationAttrs lid = getsGame (toAttrs . fromJustNote "no such location" . lookup lid . (.entities.locations))

treacheriesOn :: Target -> GameT [CardCode]
treacheriesOn host = getsGame (map toCardCode . filter ((== AttachedTo host) . (.placement)) . toList . (.entities.treacheries))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

-- | The Ringwraiths stage 1A set aside, as a game started past setup has them.
setRingwraithsAside :: Int -> GameT ()
setRingwraithsAside n = do
  cards <- replicateM n (genEncounterCard Enemies.ringwraith)
  run $ ShuffleSetAsideIntoEncounterDeck []
  putGame . (scenarioL %~ overAttrs (\a -> a {scenarioSetAside = cards})) =<< getGame

setAsideRingwraiths :: GameT Int
setAsideRingwraiths = copies Enemies.ringwraith . (.scenarioSetAside) <$> scenarioAttrs

-- | Reveal the first of the cards, the rest beneath it in the encounter deck.
reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

exhaustTheOneRing :: GameT ()
exhaustTheOneRing = getSagaOneRing >>= traverse_ (run . Exhaust . ObjectiveTarget . (.id))

spec :: Spec
spec = describe "A Knife in the Dark" do
  it "is a scenario the game can be dealt" . knifeTest $ \_ ->
    isJust (lookupScenario "a-knife-in-the-dark") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "stages The Prancing Pony and Bill Ferny and sets the Ringwraiths, The Witch-king, Midgewater and Weathertop aside" . knifeTest $ \pid -> do
      setUp pid
      pony <- locationAttrs =<< locationOf Locations.thePrancingPony
      pony.placement `shouldBe` StagingArea
      (placementOfEnemy =<< enemyOf Enemies.billFerny) `shouldReturn` StagingArea
      attrs <- scenarioAttrs
      sort (map toCardCode attrs.scenarioSetAside)
        `shouldBe` sort (map toCardCode $ replicate 4 Enemies.ringwraith <> [Enemies.theWitchKing, Locations.midgewater, Locations.weathertop])
      all ((`elem` map Just [AKnifeInTheDark, Hunted, TheRing, TheNazgul]) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.intoTheWild, Quests.theRingwraithsAttack]
      activeQuestCode `shouldReturn` toCardCode Quests.troubleInBree

    it "builds the encounter deck, fewer copies in easy mode" . knifeTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.riderOfMordor, Enemies.shadyBreeLander, Enemies.squintEyedSoutherner, Locations.chetwood, Locations.weatherHills, Treacheries.unwantedAttention, Treacheries.blackBreath]
        `shouldBe` [2, 4, 3, 3, 3, 3, 2]

    it "keeps 2 Squint-eyed Southerners and 2 Ringwraiths in easy mode" . gameTestIn (overAttrs (modeL .~ EasyMode) aKnifeInTheDark) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      copies Enemies.squintEyedSoutherner <$> getEncounterDeck `shouldReturn` 2
      setAsideRingwraiths `shouldReturn` 2

  -- \* The stages

  describe "Trouble in Bree" do
    it "cannot be defeated while The Prancing Pony is in play" . knifeTest $ \pid -> do
      setUp pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 8
      activeQuestCode `shouldReturn` toCardCode Quests.troubleInBree
      pony <- locationOf Locations.thePrancingPony
      _ <- setEncounterDeck [Treacheries.lureOfTheRing]
      run $ AddToVictoryDisplay (LocationTarget pony)
      useForcedAbilities pid
      resolveWhenRevealed pid
      activeQuestCode `shouldReturn` toCardCode Quests.intoTheWild

  describe "Into the Wild" do
    it "adds Midgewater, has each player place 1 progress, and cannot be defeated while Midgewater is in play" . knifeTest $ \pid -> do
      midgewater <- genEncounterCard Locations.midgewater
      putGame . (scenarioL %~ overAttrs (\a -> a {scenarioSetAside = [midgewater]})) =<< getGame
      q <- putQuestStage Quests.intoTheWild
      run $ Flip q
      resolveWhenRevealed pid
      _ <- locationOf Locations.midgewater
      (.progress) <$> getActiveQuest `shouldReturn` 1
      run $ PlaceTokens GameSource q Progress 5
      activeQuestCode `shouldReturn` toCardCode Quests.intoTheWild

    it "at the end of the round removes X progress or shuffles an out of play Ringwraith in" . knifeTest $ \pid -> do
      setRingwraithsAside 4
      q <- putQuestStage Quests.intoTheWild
      run $ Flip q
      resolveWhenRevealed pid
      run $ CheckWindows [mkAfter RoundEnds]
      useForcedAbilities pid
      -- 1 progress for 1 player: both options can be done in full
      chooseLabel "$label.intoTheWild.removeProgress" pid
      (.progress) <$> getActiveQuest `shouldReturn` 0
      run $ CheckWindows [mkAfter RoundEnds]
      useForcedAbilities pid
      -- no progress left: the Ringwraith is a must
      setAsideRingwraiths `shouldReturn` 3
      copies Enemies.ringwraith <$> getEncounterDeck `shouldReturn` 1

  describe "The Ringwraiths Attack" do
    it "adds Weathertop, The Witch-king and each Nazgûl in the encounter deck and discard pile, and wins once none is in play" . knifeTest $ \pid -> do
      weathertop <- genEncounterCard Locations.weathertop
      witchKing <- genEncounterCard Enemies.theWitchKing
      putGame . (scenarioL %~ overAttrs (\a -> a {scenarioSetAside = [weathertop, witchKing]})) =<< getGame
      _ <- setEncounterDeck [Enemies.riderOfMordor, Treacheries.lureOfTheRing]
      run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.ringwraith
      q <- putQuestStage Quests.theRingwraithsAttack
      run $ Flip q
      _ <- locationOf Locations.weathertop
      nazgul <- concat <$> traverse enemiesOf [Enemies.theWitchKing, Enemies.riderOfMordor, Enemies.ringwraith]
      length nazgul `shouldBe` 3
      traverse placementOfEnemy nazgul `shouldReturn` replicate 3 StagingArea
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Treacheries.lureOfTheRing]
      -- not while one Nazgûl is still in play
      for_ (take 2 nazgul) \e -> run $ Discard (EnemyTarget e)
      getsGame (.outcome) `shouldReturn` Nothing
      for_ (drop 2 nazgul) \e -> run $ Discard (EnemyTarget e)
      getsGame (.outcome) `shouldReturn` Just Won
      void $ pure pid

    it "in campaign mode has each player choose a boon for a hero he controls as the players win, recorded on that hero" . campaignTest $ \pid -> do
      setUp pid
      -- the players reach stage 3, Trouble in Bree still on the table not mattering here
      _ <- putQuestStage Quests.theRingwraithsAttack
      q <- getsGame (fromJustNote "no stage 3" . fmap toTarget . find ((== toCardCode Quests.theRingwraithsAttack) . toCardCode) . toList . (.entities.quests))
      run $ Flip q
      nazgul <- concat <$> traverse enemiesOf [Enemies.theWitchKing, Enemies.riderOfMordor, Enemies.ringwraith]
      for_ nazgul \e -> run $ Discard (EnemyTarget e)
      -- pass, then Tireless Ranger, Skilled Healer...
      void $ answerQuestion pid 3
      eowyn <- getHeroWithCode "01007"
      chooseTarget pid eowyn.id
      getsGame (.outcome) `shouldReturn` Just Won
      (.pool) <$> getCampaignLog `shouldReturn` [PoolCard (toCardCode Attachments.valiantWarrior) (Just 0) (Just "01007")]

  -- \* Locations

  describe "The Prancing Pony" do
    it "has the first player put an ally into play from his hand after traveling there" . knifeTest $ \pid -> do
      pony <- stageLocation Locations.thePrancingPony
      _ <- putCardIntoHand pid Allies.gondorianSpearman
      run $ TravelTo pony
      chooseAbilityOf pid (LocationTarget pony)
      getsGame (any ((== toCardCode Allies.gondorianSpearman) . toCardCode) . toList . (.entities.allies)) `shouldReturn` True

    it "discards until X enemies are discarded as it leaves play, each engaging the first player" . knifeTest $ \pid -> do
      pony <- stageLocation Locations.thePrancingPony
      _ <- setEncounterDeck [Treacheries.lureOfTheRing, Enemies.shadyBreeLander, Enemies.shadyBreeLander]
      run $ Discard (LocationTarget pony)
      useForcedAbilities pid
      enemiesEngagedWith pid `shouldReturn` 1
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.shadyBreeLander]
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Treacheries.lureOfTheRing))

  describe "Weathertop" do
    it "is immune, needs The One Ring exhausted to travel to, and lowers each enemy's threat from the victory display" . knifeTest $ \pid -> do
      setUp pid
      top <- stageLocation Locations.weathertop
      getModifiers (LocationTarget top) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getModifiers (LocationTarget top) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo top
      getModifiers (LocationTarget top) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      bill <- enemyOf Enemies.billFerny
      run $ AddToVictoryDisplay (LocationTarget top)
      getModifiers (EnemyTarget bill) >>= (`shouldSatisfy` elem (ThreatModifier (-1)))

  describe "Chetwood" do
    it "gives Spy enemies +1 threat and +1 attack while in the staging area" . knifeTest $ \_ -> do
      _ <- stageLocation Locations.chetwood
      bree <- stage Enemies.shadyBreeLander
      getModifiers (EnemyTarget bree) >>= (`shouldSatisfy` \ms -> ThreatModifier 1 `elem` ms && AttackModifier 1 `elem` ms)

  describe "Midgewater" do
    it "returns engaged enemies to the staging area as it becomes active, and stops enemies attacking, taking damage or engaging" . knifeTest $ \pid -> do
      bree <- engaged pid Enemies.shadyBreeLander
      water <- stageLocation Locations.midgewater
      run $ TravelTo water
      useForcedAbilities pid
      placementOfEnemy bree `shouldReturn` StagingArea
      getModifiers (EnemyTarget bree) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [CannotAttack, CannotTakeDamage, CannotEngagePlayer pid])

  describe "Weather Hills" do
    it "puts a non-unique enemy under it, out of play, and returns it to the staging area as it leaves play" . knifeTest $ \pid -> do
      bree <- engaged pid Enemies.shadyBreeLander
      hills <- stageLocation Locations.weatherHills
      run $ TravelTo hills
      chooseAbilityOf pid (LocationTarget hills)
      getsGame (null . (.entities.enemies)) `shouldReturn` True
      length . (.underneath) <$> locationAttrs hills `shouldReturn` 1
      run $ Discard (LocationTarget hills)
      useForcedAbilities pid
      (placementOfEnemy =<< enemyOf Enemies.shadyBreeLander) `shouldReturn` StagingArea
      void $ pure bree

  -- \* Enemies

  describe "Rider of Mordor" do
    it "gives its engaged player's characters -1 willpower and gets -10 engagement cost while The One Ring is exhausted" . knifeTest $ \pid -> do
      setUp pid
      eowyn <- hero "01007"
      w <- getWillpower eowyn
      rider <- engaged pid Enemies.riderOfMordor
      getWillpower eowyn `shouldReturn` (w - 1)
      getEngagementCost rider `shouldReturn` 35
      exhaustTheOneRing
      getEngagementCost rider `shouldReturn` 25

  describe "Bill Ferny" do
    it "cannot be optionally engaged, and at the staging step shuffles in a Ringwraith or raises threat for each committed non-Fellowship hero" . knifeTest $ \pid -> do
      setUp pid
      bill <- enemyOf Enemies.billFerny
      getModifiers (EnemyTarget bill) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))
      eowyn <- hero "01007"
      run $ CommitToQuest eowyn
      inPhase QuestPhase
      p <- threatOf pid
      run $ CheckWindows [mkAfter StagingStepBegins]
      useForcedAbilities pid
      chooseLabel "$label.billFerny.raiseThreat" pid
      threatOf pid `shouldReturn` (p + 1)
      run $ CheckWindows [mkAfter StagingStepBegins]
      useForcedAbilities pid
      chooseLabel "$label.aKnifeInTheDark.shuffleRingwraith" pid
      setAsideRingwraiths `shouldReturn` 3

  describe "Shady Bree-lander" do
    it "cannot be optionally engaged while The One Ring is exhausted" . knifeTest $ \pid -> do
      setUp pid
      bree <- stage Enemies.shadyBreeLander
      getModifiers (EnemyTarget bree) >>= (`shouldSatisfy` notElem (CannotBeOptionallyEngagedBy AnyPlayer))
      exhaustTheOneRing
      getModifiers (EnemyTarget bree) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))

  describe "Squint-eyed Southerner" do
    it "has the revealer shuffle in a Ringwraith or reveal another card, only the reveal with none out of play" . knifeTest $ \pid -> do
      setRingwraithsAside 1
      reveal [Enemies.squintEyedSoutherner, Treacheries.lureOfTheRing]
      chooseLabel "$label.aKnifeInTheDark.shuffleRingwraith" pid
      setAsideRingwraiths `shouldReturn` 0
      copies Enemies.ringwraith <$> getEncounterDeck `shouldReturn` 1
      -- none left out of play: the additional card is revealed without asking
      reveal [Enemies.squintEyedSoutherner, Enemies.shadyBreeLander]
      length <$> enemiesOf Enemies.shadyBreeLander `shouldReturn` 1

  -- \* Treacheries

  describe "Unwanted Attention" do
    it "removes 2 of the revealer's heroes from the quest, or shuffles in a Ringwraith" . knifeTest $ \pid -> do
      setRingwraithsAside 1
      eowyn <- hero "01007"
      theodred <- hero "01002"
      run $ CommitToQuest eowyn
      run $ CommitToQuest theodred
      inPhase QuestPhase
      reveal [Treacheries.unwantedAttention]
      chooseLabel "$label.unwantedAttention.remove" pid
      let committed code = getsGame (heroCommitted . toAttrs . fromJustNote "no hero" . find ((== code) . toCardCode) . toList . (.entities.heroes))
      committed "01007" `shouldReturn` False
      committed "01002" `shouldReturn` False
      -- no heroes committed: the Ringwraith is a must
      reveal [Treacheries.unwantedAttention]
      setAsideRingwraiths `shouldReturn` 0

  describe "Black Breath" do
    it "attaches to a hero of the revealer's, reducing its willpower, attack and defense to 0" . knifeTest $ \pid -> do
      eowyn <- hero "01007"
      reveal [Treacheries.blackBreath]
      chooseTarget pid eowyn
      treacheriesOn eowyn `shouldReturn` [toCardCode Treacheries.blackBreath]
      getWillpower eowyn `shouldReturn` 0
      getAttack eowyn `shouldReturn` 0
      getDefense eowyn `shouldReturn` 0
