module LordOfTheRings.Hobbit.DungeonsDeepAndCavernsDimSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithTrait), getBilbo, getCurrentQuest, getMainQuests)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (dungeonsDeepAndCavernsDim, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL)
import LordOfTheRings.Trait (Trait (Creature))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

ddTest :: (PlayerId -> GameT ()) -> IO ()
ddTest = gameTestIn dungeonsDeepAndCavernsDim testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

questOf :: CardDef -> GameT (Maybe Quest)
questOf def = getsGame (find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questTarget :: CardDef -> GameT Target
questTarget def = toTarget . fromJustNote "no such stage" <$> questOf def

riddleProgress :: GameT Int
riddleProgress = maybe 0 (.progress) <$> questOf Quests.riddlesInTheDark

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

objectiveOf :: CardDef -> GameT ObjectiveAttrs
objectiveOf def = getsGame (toAttrs . fromJustNote "no such objective" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

locationOf :: CardDef -> GameT (Maybe LocationId)
locationOf def = getsGame (fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

bilboTarget :: GameT Target
bilboTarget = toTarget . fromJustNote "no Bilbo" <$> getBilbo

-- | The player's deck, top first.
setDeck :: PlayerId -> [CardDef] -> GameT ()
setDeck pid defs = do
  cards <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) defs
  putGame . (playersL . ix pid . deckL .~ cards) =<< getGame

{- | Bilbo Baggins in play, then stage 1's setup, as the game performs it: Lake in the
Cavern to the staging area, and the riddle area with stage 2, Gollum and Bilbo's Magic Ring.
-}
atRiddles :: PlayerId -> GameT ()
atRiddles _ = do
  run $ DoStep 3 Setup
  _ <- setEncounterDeck [Locations.lakeInTheCavern, Enemies.gollumDungeonsDeep, Objectives.bilbosMagicRing, Locations.theWargsGlade]
  laterStages <- traverse genEncounterCard [Quests.riddlesInTheDark, Quests.intoTheFire]
  putGame . (scenarioL %~ overAttrs (questDeckL .~ laterStages)) =<< getGame
  card <- genEncounterCard Quests.outOfTheFryingPan
  run $ PutQuestIntoPlay card
  run $ PerformSetup (CardIdTarget card.id)

-- | Stage 1 defeated, the players advance to Into the Fire, revealing The Wargs' Glade.
atIntoTheFire :: PlayerId -> GameT ()
atIntoTheFire pid = do
  _ <- setEncounterDeck [Locations.theWargsGlade]
  frying <- questOf Quests.outOfTheFryingPan
  for_ frying \q -> run $ LeaveQuestStage (toId q)
  card <- genEncounterCard Quests.intoTheFire
  putGame . (scenarioL %~ overAttrs (questDeckL .~ [])) =<< getGame
  run $ AdvanceToQuest card
  resolveWhenRevealed pid
  passAll pid

-- | Decline every action window and take each attack undefended, until neither is asked.
passAll :: PlayerId -> GameT ()
passAll pid = do
  passActions pid
  getQuestion pid >>= \case
    Just q | any (\case Label l _ -> l == "$label.takeAttackUndefended"; _ -> False) (questionChoices q) -> do
      chooseLabel "$label.takeAttackUndefended" pid
      passAll pid
    _ -> pure ()

spec :: Spec
spec = describe "Dungeons Deep and Caverns Dim" do
  it "is a scenario the game can be dealt" . ddTest $ \_ ->
    isJust (lookupScenario "dungeons-deep-and-caverns-dim") `shouldBe` True

  describe "setup" do
    it "adds Lake in the Cavern to the staging area and creates the riddle area with stage 2, Gollum and Bilbo wearing his Magic Ring" . ddTest $ \pid -> do
      flipFirstQuest pid
      attrs <- scenarioAttrs
      let inDeck = attrs.encounterDeck <> attrs.encounterDiscard
      all ((`elem` map Just [DungeonsDeepAndCavernsDim, MistyMountainGoblins]) . cdEncounterSet . toCardDef) inDeck `shouldBe` True
      any ((`elem` map toCardCode [Enemies.gollumDungeonsDeep, Objectives.bilbosMagicRing, Locations.lakeInTheCavern]) . toCardCode) inDeck `shouldBe` False
      map toCardCode attrs.questDeck `shouldBe` [toCardCode Quests.intoTheFire]
      isJust <$> locationOf Locations.lakeInTheCavern `shouldReturn` True
      gollum <- enemyOf Enemies.gollumDungeonsDeep
      placementOfEnemy gollum `shouldReturn` RiddleArea
      bilbo <- bilboTarget
      (.objectivePlacement) <$> objectiveOf Objectives.bilbosMagicRing `shouldReturn` AttachedTo bilbo
      (.flipped) . toAttrs . fromJustNote "stage 1" <$> questOf Quests.outOfTheFryingPan `shouldReturn` True
      (.flipped) . toAttrs . fromJustNote "stage 2" <$> questOf Quests.riddlesInTheDark `shouldReturn` True
      hero <- getHeroWithCode "131001"
      hero.owner `shouldBe` pid

  describe "the riddle area" do
    it "keeps stage 2 out of the current quest, and Bilbo from questing, attacking, attachments and player card effects" . ddTest $ \pid -> do
      atRiddles pid
      frying <- questTarget Quests.outOfTheFryingPan
      getCurrentQuest `shouldReturn` Just frying
      getMainQuests `shouldReturn` [frying]
      bilbo <- bilboTarget
      getModifiers bilbo >>= (`shouldSatisfy` \ms -> all (`elem` ms) [CannotCommitToQuest, CannotAttack, CannotHavePlayerAttachments, ImmuneToPlayerCardEffects])
      gollum <- enemyOf Enemies.gollumDungeonsDeep
      getModifiers (EnemyTarget gollum) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

    it "takes progress on stage 2 only from riddles" . ddTest $ \pid -> do
      atRiddles pid
      riddles <- questTarget Quests.riddlesInTheDark
      run $ PlaceTokens GameSource riddles Progress 2
      riddleProgress `shouldReturn` 0

    it "lets Bilbo take damage only from Gollum while he is in the riddle area" . ddTest $ \pid -> do
      atRiddles pid
      bilbo <- bilboTarget
      run $ DealDamage GameSource bilbo 1
      getDamage bilbo `shouldReturn` 0
      gollum <- enemyOf Enemies.gollumDungeonsDeep
      run $ DealDamage (EnemyAttackSource gollum) bilbo 1
      getDamage bilbo `shouldReturn` 1

    it "gives Lake in the Cavern threat twice the number of players" . ddTest $ \pid -> do
      atRiddles pid
      lake <- fromJustNote "lake" <$> locationOf Locations.lakeInTheCavern
      getLocationThreat lake `shouldReturn` 2
      getModifiers (LocationTarget lake) >>= (`shouldSatisfy` \ms -> CannotBeTraveledTo `elem` ms && ImmuneToPlayerCardEffects `elem` ms)

    it "reveals 1 more encounter card per player during the staging step" . ddTest $ \pid -> do
      atRiddles pid
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))

  describe "riddles" do
    it "lets the first player answer a revealed card's riddle, placing 1 progress on stage 2 for each match, the card discarded" . ddTest $ \pid -> do
      atRiddles pid
      setDeck pid [Allies.snowbournScout, Allies.snowbournScout]
      _ <- setEncounterDeck [Enemies.wildWargs]
      run RevealEncounterCard
      chooseLabel "$label.riddle.answer" pid
      chooseLabel "$label.riddle.ally" pid
      passAll pid
      riddleProgress `shouldReturn` 2
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.wildWargs]
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.gollumDungeonsDeep]

    it "resolves the card normally if the first player chooses to" . ddTest $ \pid -> do
      atRiddles pid
      _ <- setEncounterDeck [Enemies.wildWargs]
      run RevealEncounterCard
      chooseLabel "$label.riddle.resolve" pid
      wargs <- enemyOf Enemies.wildWargs
      placementOfEnemy wargs `shouldReturn` StagingArea
      riddleProgress `shouldReturn` 0

    it "has Gollum attack Bilbo Baggins when the riddle finds no match" . ddTest $ \pid -> do
      atRiddles pid
      setDeck pid [Events.sneakAttack, Events.everVigilant]
      _ <- setEncounterDeck [Enemies.wildWargs]
      run RevealEncounterCard
      chooseLabel "$label.riddle.answer" pid
      chooseLabel "$label.riddle.ally" pid
      passAll pid
      riddleProgress `shouldReturn` 0
      bilbo <- bilboTarget
      getDamage bilbo `shouldReturn` 2

    it "matches only the cards that have every item named" . ddTest $ \pid -> do
      atRiddles pid
      -- Sneak Attack is a Leadership event costing 1, Grim Resolve one costing 5
      setDeck pid [Events.sneakAttack, Events.grimResolve]
      _ <- setEncounterDeck [Treacheries.whatsInMyPocket]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.riddle.leadership" pid
      chooseLabel "$label.amount n=i:1" pid
      passAll pid
      riddleProgress `shouldReturn` 1

    it "lets Bilbo's Magic Ring spend 1 Baggins resource to discard 1 more card as the riddle is answered" . ddTest $ \pid -> do
      atRiddles pid
      bilbo <- bilboTarget
      run $ PlaceTokens GameSource bilbo Resource 1
      setDeck pid [Allies.snowbournScout, Allies.snowbournScout, Allies.snowbournScout]
      _ <- setEncounterDeck [Enemies.wildWargs]
      run RevealEncounterCard
      chooseLabel "$label.riddle.answer" pid
      chooseLabel "$label.riddle.ally" pid
      ring <- objectiveOf Objectives.bilbosMagicRing
      chooseAbilityOf pid (ObjectiveTarget ring.objectiveId)
      getQuestion pid >>= \case
        Just q | any (\case TargetLabel t _ -> t == bilbo; _ -> False) (questionChoices q) -> chooseTarget pid bilbo
        _ -> pure ()
      passAll pid
      riddleProgress `shouldReturn` 3
      getHeroResources (case bilbo of HeroTarget h -> h; _ -> error "hero") `shouldReturn` 0

    it "lets Bilbo's Magic Ring exhaust and raise each player's threat by 2 to give Bilbo 1 resource" . ddTest $ \pid -> do
      atRiddles pid
      threat <- (.threat) <$> getPlayer pid
      ring <- objectiveOf Objectives.bilbosMagicRing
      run $ ActionWindow pid
      chooseAbilityOf pid (ObjectiveTarget ring.objectiveId)
      (.threat) <$> getPlayer pid `shouldReturn` (threat + 2)
      bilbo <- bilboTarget
      getHeroResources (case bilbo of HeroTarget h -> h; _ -> error "hero") `shouldReturn` 1
      (.objectiveExhausted) <$> objectiveOf Objectives.bilbosMagicRing `shouldReturn` True

    it "has It Likes Riddles? answer the next riddle discarded from the encounter deck when its own finds a match" . ddTest $ \pid -> do
      atRiddles pid
      setDeck pid [Allies.snowbournScout, Allies.snowbournScout, Allies.snowbournScout]
      _ <- setEncounterDeck [Treacheries.itLikesRiddles, Enemies.goblinAxeman, Locations.theWargsGlade, Enemies.goblinMiners]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.amount n=i:1" pid
      passAll pid
      -- The Wargs' Glade's riddle, a sphere, 1 card
      chooseLabel "$label.riddle.leadership" pid
      passAll pid
      riddleProgress `shouldReturn` 3
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.goblinMiners]

    it "has Not fair! Not fair! remove 1 progress from stage 2 for each card discarded not of the type named" . ddTest $ \pid -> do
      atRiddles pid
      riddles <- questTarget Quests.riddlesInTheDark
      run $ PlaceTokens ScenarioSource riddles Progress 3
      _ <- setEncounterDeck [Treacheries.notFairNotFair, Enemies.wildWargs, Locations.theWargsGlade, Treacheries.hidingInTheTrees]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.notFairNotFair.enemy" pid
      riddleProgress `shouldReturn` 1

  describe "the encounter cards" do
    it "gives Wild Wargs +2 attack for a shadow card with a riddle" . ddTest $ \pid -> do
      atRiddles pid
      _ <- setEncounterDeck [Enemies.wildWargs]
      run RevealEncounterCard
      chooseLabel "$label.riddle.resolve" pid
      wargs <- enemyOf Enemies.wildWargs
      shadow <- genEncounterCard Treacheries.hidingInTheTrees
      run $ AttachShadowCard wargs shadow
      run $ ResolveShadow wargs pid []
      getEnemyAttack wargs `shouldReturn` 5

    it "gives Wild Wargs +2 attack once for each riddle shadow card, as each turns faceup" . ddTest $ \pid -> do
      atRiddles pid
      _ <- setEncounterDeck [Enemies.wildWargs]
      run RevealEncounterCard
      chooseLabel "$label.riddle.resolve" pid
      wargs <- enemyOf Enemies.wildWargs
      run . AttachShadowCard wargs =<< genEncounterCard Treacheries.hidingInTheTrees
      run . AttachShadowCard wargs =<< genEncounterCard Treacheries.hidingInTheTrees
      run $ ResolveShadow wargs pid []
      resolveShadowsInOrder pid
      getEnemyAttack wargs `shouldReturn` 7

    it "has Great Gray Wolf, dealt a riddle, return the Creature enemies to the staging area at the end of the combat phase" . ddTest $ \pid -> do
      atRiddles pid
      wolf <- do
        run . AddToStaging =<< genEncounterCard Enemies.greatGrayWolf
        enemyOf Enemies.greatGrayWolf
      run $ PlaceEnemy wolf (EngagedWith pid)
      shadow <- genEncounterCard Enemies.wildWargs
      run $ AttachShadowCard wolf shadow
      run $ ResolveShadow wolf pid []
      getModifiers GameTarget >>= (`shouldSatisfy` elem (ReturnToStagingAtEndOfCombat $ EnemyWithTrait Creature))
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ EndPhase CombatPhase
      placementOfEnemy wolf `shouldReturn` StagingArea

    it "has The Wargs' Glade remove 1 progress from the current quest as a Creature's attack damages a character" . ddTest $ \pid -> do
      atRiddles pid
      _ <- setEncounterDeck [Locations.theWargsGlade]
      run RevealEncounterCard
      chooseLabel "$label.riddle.resolve" pid
      frying <- questTarget Quests.outOfTheFryingPan
      run $ PlaceTokens GameSource frying Progress 2
      run . AddToStaging =<< genEncounterCard Enemies.wildWargs
      wargs <- enemyOf Enemies.wildWargs
      beravor <- toTarget <$> getHeroWithCode "01012"
      run $ CheckWindows [mkAfter (DamagedByEnemyAttack beravor wargs)]
      maybe 0 (.progress) <$> questOf Quests.outOfTheFryingPan `shouldReturn` 1

    it "has Hiding in the Trees give each character -1 willpower and no attacks this round" . ddTest $ \pid -> do
      atRiddles pid
      _ <- setEncounterDeck [Treacheries.hidingInTheTrees]
      run RevealEncounterCard
      chooseLabel "$label.riddle.resolve" pid
      resolveRevealed
      eowyn <- toTarget <$> getHeroWithCode "01007"
      getModifiers eowyn >>= (`shouldSatisfy` \ms -> WillpowerModifier (-1) `elem` ms && CannotAttack `elem` ms)

  describe "the stages" do
    it "advances to Into the Fire only once both stage 1 and stage 2 are complete" . ddTest $ \pid -> do
      atRiddles pid
      frying <- questTarget Quests.outOfTheFryingPan
      run $ PlaceTokens GameSource frying Progress 14
      isJust <$> questOf Quests.outOfTheFryingPan `shouldReturn` True
      _ <- setEncounterDeck [Enemies.wildWargs, Locations.theWargsGlade]
      riddles <- questTarget Quests.riddlesInTheDark
      run $ PlaceTokens ScenarioSource riddles Progress 9
      resolveWhenRevealed pid
      passAll pid
      map toCardCode . toList <$> getsGame (.entities.quests) `shouldReturn` [toCardCode Quests.intoTheFire]
      -- Into the Fire reveals 1 card per player, its riddle ignored
      wargs <- enemyOf Enemies.wildWargs
      placementOfEnemy wargs `shouldReturn` StagingArea
      locationOf Locations.lakeInTheCavern `shouldReturn` Nothing
      gollum <- enemyOf Enemies.gollumDungeonsDeep
      placementOfEnemy gollum `shouldReturn` EngagedWith pid
      bilbo <- bilboTarget
      getModifiers (EnemyTarget gollum) >>= (`shouldSatisfy` elem (UndefendedDamageTo bilbo))
      getModifiers bilbo >>= (`shouldSatisfy` notElem CannotCommitToQuest)

    it "gives treachery cards surge on Into the Fire" . ddTest $ \pid -> do
      atRiddles pid
      atIntoTheFire pid
      _ <- setEncounterDeck [Treacheries.hidingInTheTrees, Locations.theWargsGlade]
      run RevealEncounterCard
      resolveRevealed
      length . filter ((== toCardCode Locations.theWargsGlade) . toCardCode) . toList <$> getsGame (.entities.locations) `shouldReturn` 2

    it "wins the game as Into the Fire is defeated" . ddTest $ \pid -> do
      atRiddles pid
      atIntoTheFire pid
      fire <- questTarget Quests.intoTheFire
      run $ PlaceTokens GameSource fire Progress 16
      getsGame (.outcome) `shouldReturn` Just Won
