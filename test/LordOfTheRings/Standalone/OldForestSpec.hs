module LordOfTheRings.Standalone.OldForestSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Ally (Ally)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Card (IsCard (toCard), CardType (EventType))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (getActionAbilities)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getHero, CardMatcher (AnyCard))
import LordOfTheRings.Matcher (selectCharacters, HeroMatcher (CommittedHero), AllyMatcher (CommittedAlly))
import LordOfTheRings.Modifier (ModifierType (AttackModifier, AdditionalStagingReveals, PlayersCannotWin, MustBeTraveledTo, CannotPlay, PlayedCardThisRound, Blank, CannotHaveAttachments, CannotHaveAttachmentsWithoutTrait, CannotLeavePlay, CannotDrawCardsFromCardEffects, CannotReady))
import LordOfTheRings.Trait (Trait (Condition, Weapon))
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase, CombatPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (victoryLocations, locationVictoryPoints)
import LordOfTheRings.Window (WindowType (EnemyAttacking, ActiveLocationExplored, OptionallyEngaged), mkWhen, mkAfter)
import TestImport

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "enemy missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

victoryLocation :: CardDef -> GameT ()
victoryLocation def = do
  lid <- stageLocation def
  run $ AddToVictoryDisplay (LocationTarget lid)

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "location missing" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations)

heroExhaustedTarget :: Target -> GameT Bool
heroExhaustedTarget (HeroTarget hid) = heroExhausted . toAttrs <$> getHero hid
heroExhaustedTarget _ = error "expected a hero"

tomAlly :: GameT Ally
tomAlly = getsGame $ fromJustNote "Tom missing" . find ((== toCardCode Objectives.tomBombadilOldForest) . toCardCode) . toList . (.entities.allies)

bogeyAvailable :: PlayerId -> Target -> GameT Bool
bogeyAvailable pid target = any (\ability -> sourceTarget ability.abilitySource == Just target) <$> getActionAbilities pid

spec :: Spec
spec = describe "Old Forest" do
  describe "Tom Bombadil" do
    it "lets the revealer choose who controls him, ready and committed during questing" . twoPlayerTest testDeck $ \one two -> do
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ SetRevealer (Just two)
      _ <- setEncounterDeck [Objectives.tomBombadilOldForest]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget two (PlayerTarget one)
      tom <- tomAlly
      tom.controller `shouldBe` one
      allyCommitted (toAttrs tom) `shouldBe` True
      allyExhausted (toAttrs tom) `shouldBe` False

    it "cannot have its When Revealed canceled" . gameTest $ \pid -> do
      hero <- heroOf pid "01007"
      _ <- putCardIntoHand pid Events.aTestOfWill
      run $ PlaceTokens GameSource hero Resource 1
      _ <- setEncounterDeck [Objectives.tomBombadilOldForest]
      run RevealEncounterCard
      resolveRevealed
      tom <- tomAlly
      allyExhausted (toAttrs tom) `shouldBe` False
      getQuestion pid `shouldReturn` Nothing

    it "enters ready outside the quest phase without committing to a quest" . gameTest $ \_ -> do
      putGame . (phaseL .~ CombatPhase) =<< getGame
      _ <- setEncounterDeck [Objectives.tomBombadilOldForest]
      run RevealEncounterCard
      resolveRevealed
      tom <- tomAlly
      allyCommitted (toAttrs tom) `shouldBe` False
      allyExhausted (toAttrs tom) `shouldBe` False

    it "enters ready and committed even while characters cannot ready" . gameTest $ \_ -> do
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ CreateEffect $ Effect GameSource GameTarget [CannotReady] UntilEndOfRound
      _ <- setEncounterDeck [Objectives.tomBombadilOldForest]
      run RevealEncounterCard
      resolveRevealed
      tom <- tomAlly
      allyCommitted (toAttrs tom) `shouldBe` True
      allyExhausted (toAttrs tom) `shouldBe` False

    it "discards him and shuffles the entire encounter discard into the deck even if he is blanked" . gameTest $ \_ -> do
      discarded <- stageEnemy Enemies.armyOfFlies
      run $ Discard GameSource (EnemyTarget discarded)
      _ <- setEncounterDeck [Objectives.tomBombadilOldForest]
      run RevealEncounterCard
      resolveRevealed
      tom <- tomAlly
      run $ CreateEffect $ Effect GameSource (toTarget tom) [Blank] UntilEndOfRound
      run EndRound
      getsGame (any ((== tom.id) . toId) . toList . (.entities.allies)) `shouldReturn` False
      getEncounterDiscard `shouldReturn` []
      cards <- getEncounterDeck
      sort (map toCardCode cards) `shouldBe` sort (map toCardCode [Objectives.tomBombadilOldForest, Enemies.armyOfFlies])

    it "does not shuffle the discard if he was destroyed before the round ended" . gameTest $ \_ -> do
      _ <- setEncounterDeck [Objectives.tomBombadilOldForest]
      run RevealEncounterCard
      resolveRevealed
      tom <- tomAlly
      run $ DealDamage GameSource (toTarget tom) 6
      run EndRound
      getEncounterDeck `shouldReturn` []
      getEncounterDiscard >>= (\cards -> map toCardCode cards `shouldBe` [toCardCode Objectives.tomBombadilOldForest])

  describe "Old Bogey-stories" do
    it "has the first player choose its setup host even when it belongs to another player's deck" . twoPlayerTest testDeck $ \one two -> do
      card <- genPlayerCard Attachments.oldBogeyStories (Just two)
      g <- getGame
      putGame $ g & playersL . ix two . deckL %~ (PlayerCard card :)
      run $ DoStep 4 Setup
      getQuestion two `shouldReturn` Nothing
      hero <- heroOf one "01012"
      chooseTarget one hero
      attachments <- getsGame (toList . (.entities.attachments))
      let boon = fromJustNote "boon missing" $ find ((== card.id) . (.cardId) . toAttrs) attachments
      toCard boon `shouldBe` PlayerCard card
      (toAttrs boon).controller `shouldBe` one

    for_ [5, 6, 8] \n ->
      it ("requires at least six cards and replaces the controller's entire hand: " <> show n) . gameTest $ \pid -> do
        hero <- heroOf pid "01012"
        boon <- putAttachmentOn pid Attachments.oldBogeyStories hero
        original <- replicateM n $ putCardIntoHand pid Allies.guardOfTheCitadel
        oldDeck <- (.deck) <$> getPlayer pid
        bogeyAvailable pid boon `shouldReturn` (n >= 6)
        case boon of
          AttachmentTarget aid -> run $ UseCardAbility pid (AttachmentSource aid) 1 []
          _ -> error "attachment missing"
        player <- getPlayer pid
        if n < 6 then player.hand `shouldBe` reverse original else do
          length player.hand `shouldBe` 6
          length player.deck `shouldBe` length oldDeck + n - 6
          sort (map (.id) $ player.hand <> player.deck) `shouldBe` sort (map (.id) $ original <> oldDeck)
          display <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
          map toCardCode display `shouldBe` [toCardCode Attachments.oldBogeyStories]

    it "uses the attached hero's controller's hand while preserving the boon's original owner" . twoPlayerTest testDeck $ \one two -> do
      hero <- heroOf two "01012"
      boon <- putAttachmentOn one Attachments.oldBogeyStories hero
      ownHand <- replicateM 5 $ putCardIntoHand one Allies.guardOfTheCitadel
      replicateM_ 8 $ putCardIntoHand two Allies.guardOfTheCitadel
      case boon of
        AttachmentTarget aid -> do
          run $ AttachCard aid hero
          bogeyAvailable one boon `shouldReturn` False
          bogeyAvailable two boon `shouldReturn` True
          run $ UseCardAbility two (AttachmentSource aid) 1 []
        _ -> error "attachment missing"
      (.hand) <$> getPlayer one `shouldReturn` reverse ownHand
      length . (.hand) <$> getPlayer two `shouldReturn` 6

    it "cannot be used if it cannot leave play" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      boon <- putAttachmentOn pid Attachments.oldBogeyStories hero
      original <- replicateM 6 $ putCardIntoHand pid Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource boon [CannotLeavePlay] UntilEndOfRound
      bogeyAvailable pid boon `shouldReturn` False
      case boon of
        AttachmentTarget aid -> run $ UseCardAbility pid (AttachmentSource aid) 1 []
        _ -> error "attachment missing"
      (.hand) <$> getPlayer pid `shouldReturn` reverse original

    it "shuffles before attempting to draw when card effects cannot draw cards" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      boon <- putAttachmentOn pid Attachments.oldBogeyStories hero
      replicateM_ 6 $ putCardIntoHand pid Allies.guardOfTheCitadel
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCardsFromCardEffects] UntilEndOfRound
      case boon of
        AttachmentTarget aid -> run $ UseCardAbility pid (AttachmentSource aid) 1 []
        _ -> error "attachment missing"
      (.hand) <$> getPlayer pid `shouldReturn` []

  describe "Swaying Without Wind" do
    it "surges when no victory location can be returned" . gameTest $ \_ -> do
      troll <- stageEnemy Enemies.hillTroll
      run $ AddToVictoryDisplay (EnemyTarget troll)
      _ <- setEncounterDeck [Treacheries.swayingWithoutWind, Enemies.longTrailer]
      run RevealEncounterCard
      resolveRevealed
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.longTrailer]

    it "lets the revealer choose a victory location and resets its progress without surging" . gameTest $ \pid -> do
      wall <- stageLocation Locations.wallOfTrees
      run $ PlaceTokens GameSource (LocationTarget wall) Progress 3
      run $ AddToVictoryDisplay (LocationTarget wall)
      victoryLocation Locations.deepGulley
      cards <- victoryLocations
      let selected = fromJustNote "wall missing" $ find ((== toCardCode Locations.wallOfTrees) . toCardCode) cards
      _ <- setEncounterDeck [Treacheries.swayingWithoutWind, Enemies.longTrailer]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid (CardIdTarget selected.id)
      locations <- getsGame (toList . (.entities.locations))
      map toCardCode locations `shouldBe` [toCardCode Locations.wallOfTrees]
      map ((.progress) . toAttrs) locations `shouldBe` [0]
      getEncounterDeck >>= (\xs -> map toCardCode xs `shouldBe` [toCardCode Enemies.longTrailer])

  describe "Falling Branches" do
    it "removes each player's own quester and deals two damage to a hero or three to an ally" . twoPlayerTest testDeck $ \one two -> do
      hero <- heroOf one "01012"
      ally <- putAllyIntoPlay two Allies.beorn
      run $ CommitToQuest hero
      run $ CommitToQuest ally
      _ <- setEncounterDeck [Treacheries.fallingBranches]
      run RevealEncounterCard
      resolveRevealed
      getDamage hero `shouldReturn` 2
      getDamage ally `shouldReturn` 3
      selectCharacters CommittedHero CommittedAlly `shouldReturn` []

    it "does nothing to characters that are not questing" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      _ <- setEncounterDeck [Treacheries.fallingBranches]
      run RevealEncounterCard
      resolveRevealed
      getDamage hero `shouldReturn` 0
      getQuestion pid `shouldReturn` Nothing

    for_ [False, True] \allyDefends ->
      it ("increases shadow attack according to the defender's type: " <> show allyDefends) . gameTest $ \pid -> do
        enemy <- stageEnemy Enemies.longTrailer
        card <- genEncounterCard Treacheries.fallingBranches
        run $ AttachShadowCard enemy card
        defender <- if allyDefends then putAllyIntoPlay pid Allies.beorn else heroOf pid "01012"
        run $ EnemyAttack pid enemy
        passActions pid
        chooseTarget pid defender
        passActions pid
        passActions pid
        getDamage defender `shouldReturn` 3

  describe "Song of Sleep" do
    it "removes a questing hero, blanks its ability and prevents it from readying until discarded" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      run $ CommitToQuest hero
      _ <- setEncounterDeck [Treacheries.songOfSleep]
      run RevealEncounterCard
      resolveRevealed
      selectCharacters CommittedHero CommittedAlly `shouldReturn` []
      getModifiers hero >>= (`shouldSatisfy` elem Blank)
      run $ Ready hero
      heroExhaustedTarget hero `shouldReturn` True
      treacheries <- getsGame (toList . (.entities.treacheries))
      length treacheries `shouldBe` 1
      for_ treacheries \t -> run $ Discard GameSource (toTarget t)
      run $ Ready hero
      heroExhaustedTarget hero `shouldReturn` False

    it "makes the player who revealed it choose only among their own questing heroes" . twoPlayerTest testDeck $ \one two -> do
      other <- heroOf one "01012"
      selected <- heroOf two "01012"
      remaining <- heroOf two "01007"
      traverse_ (run . CommitToQuest) [other, selected, remaining]
      run $ SetRevealer (Just two)
      _ <- setEncounterDeck [Treacheries.songOfSleep]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget two selected
      committed <- selectCharacters CommittedHero CommittedAlly
      committed `shouldSatisfy` elem other
      committed `shouldSatisfy` elem remaining
      committed `shouldSatisfy` notElem selected

    for_ [(CannotHaveAttachments, False), (CannotHaveAttachmentsWithoutTrait Weapon, False), (CannotHaveAttachmentsWithoutTrait Condition, True)] \(restriction, attached) ->
      it ("respects the hero's encounter-attachment restrictions: " <> show restriction) . gameTest $ \pid -> do
        hero <- heroOf pid "01012"
        run $ CommitToQuest hero
        run $ CreateEffect $ Effect GameSource hero [restriction] UntilEndOfRound
        _ <- setEncounterDeck [Treacheries.songOfSleep]
        run RevealEncounterCard
        resolveRevealed
        selectCharacters CommittedHero CommittedAlly `shouldReturn` []
        getsGame (length . (.entities.treacheries)) `shouldReturn` if attached then 1 else 0

    it "does not attach a second copy to the same hero" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      run $ CommitToQuest hero
      _ <- setEncounterDeck [Treacheries.songOfSleep, Treacheries.songOfSleep]
      run RevealEncounterCard
      resolveRevealed
      run $ CommitToQuest hero
      run RevealEncounterCard
      resolveRevealed
      getsGame (length . (.entities.treacheries)) `shouldReturn` 1

  describe "Lost and Witless" do
    it "removes staging progress before swapping a random location, preserving the old active location's progress" . gameTest $ \_ -> do
      active <- stageLocation Locations.oldForestRoad
      staged <- stageLocation Locations.deepGulley
      run $ PlaceLocation active ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget active) Progress 1
      run $ PlaceTokens GameSource (LocationTarget staged) Progress 2
      _ <- setEncounterDeck [Treacheries.lostAndWitless, Enemies.longTrailer]
      run RevealEncounterCard
      resolveRevealed
      (.placement) . toAttrs <$> getLocation active `shouldReturn` StagingArea
      (.progress) . toAttrs <$> getLocation active `shouldReturn` 1
      (.placement) . toAttrs <$> getLocation staged `shouldReturn` ActiveLocation
      (.progress) . toAttrs <$> getLocation staged `shouldReturn` 0

    it "leaves a unique active location in place" . gameTest $ \_ -> do
      active <- stageLocation Locations.withywindle
      staged <- stageLocation Locations.deepGulley
      run $ PlaceLocation active ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget staged) Progress 2
      _ <- setEncounterDeck [Treacheries.lostAndWitless, Enemies.longTrailer]
      run RevealEncounterCard
      resolveRevealed
      (.placement) . toAttrs <$> getLocation active `shouldReturn` ActiveLocation
      (.placement) . toAttrs <$> getLocation staged `shouldReturn` StagingArea
      (.progress) . toAttrs <$> getLocation staged `shouldReturn` 0

  describe "Ending and Failing" do
    it "counts enemies at phase end rather than at revelation and raises every player's threat" . twoPlayerTest testDeck $ \one two -> do
      enemy <- stageEnemy Enemies.longTrailer
      _ <- setEncounterDeck [Treacheries.endingAndFailing]
      threats <- traverse (fmap (.threat) . getPlayer) [one, two]
      run RevealEncounterCard
      resolveRevealed
      traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` map (+ 2) threats
      run $ EngageEnemy one enemy
      _ <- stageEnemy Enemies.graspingRoot
      _ <- stageEnemy Enemies.armyOfFlies
      run $ EndPhase PlanningPhase
      traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` map (+ 4) threats

    it "resolves multiple copies separately" . gameTest $ \pid -> do
      _ <- stageEnemy Enemies.longTrailer
      _ <- setEncounterDeck [Treacheries.endingAndFailing, Treacheries.endingAndFailing]
      threat <- (.threat) <$> getPlayer pid
      run RevealEncounterCard
      resolveRevealed
      run RevealEncounterCard
      resolveRevealed
      run $ EndPhase PlanningPhase
      (.threat) <$> getPlayer pid `shouldReturn` threat + 6

    it "returns an attacking enemy to staging only after its shadow attack finishes" . gameTest $ \pid -> do
      enemy <- stageEnemy Enemies.longTrailer
      run $ EngageEnemy pid enemy
      card <- genEncounterCard Treacheries.endingAndFailing
      run $ AttachShadowCard enemy card
      defender <- heroOf pid "01012"
      _ <- putCardIntoHand pid Events.aTestOfWill
      run $ EnemyAttack pid enemy
      passActions pid
      chooseTarget pid defender
      chooseLabel "$label.pass" pid
      (.placement) <$> getEnemy enemy `shouldReturn` EngagedWith pid
      passActions pid
      getDamage defender `shouldReturn` 2
      (.placement) <$> getEnemy enemy `shouldReturn` StagingArea

  describe "Withywindle" do
    it "prevents victory in either area and reveals an extra card only while active" . gameTest $ \_ -> do
      river <- stageLocation Locations.withywindle
      getModifiers GameTarget >>= (`shouldSatisfy` elem PlayersCannotWin)
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))
      run $ PlaceLocation river ActiveLocation
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))
      run $ AddToVictoryDisplay (LocationTarget river)
      getModifiers GameTarget >>= (`shouldSatisfy` notElem PlayersCannotWin)
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))

  describe "Misleading Path" do
    it "requires travel only when no active location exists" . gameTest $ \_ -> do
      path <- stageLocation Locations.misleadingPath
      road <- stageLocation Locations.oldForestRoad
      getModifiers path >>= (`shouldSatisfy` elem MustBeTraveledTo)
      run $ PlaceLocation road ActiveLocation
      getModifiers path >>= (`shouldSatisfy` notElem MustBeTraveledTo)

    it "discards every ally in only the defending player's hand" . twoPlayerTest testDeck $ \one two -> do
      enemy <- stageEnemy Enemies.forestSpider
      path <- genEncounterCard Locations.misleadingPath
      run $ AttachShadowCard enemy path
      allyOne <- putCardIntoHand one Allies.guardOfTheCitadel
      allyTwo <- putCardIntoHand one Allies.henamarthRiversong
      event <- putCardIntoHand one Events.aTestOfWill
      other <- putCardIntoHand two Allies.guardOfTheCitadel
      run $ ResolveShadowCard path.id enemy one []
      (.hand) <$> getPlayer one `shouldReturn` [event]
      (.discard) <$> getPlayer one >>= (`shouldSatisfy` (\xs -> all (`elem` xs) [allyOne, allyTwo]))
      (.hand) <$> getPlayer two `shouldReturn` [other]

  describe "Deep Gulley" do
    it "uses the modified attack of an optionally engaged enemy" . twoPlayerTest testDeck $ \one two -> do
      _ <- stageLocation Locations.deepGulley
      enemy <- stageEnemy Enemies.longTrailer
      run $ CreateEffect $ Effect GameSource (EnemyTarget enemy) [AttackModifier 3] UntilEndOfRound
      threat <- (.threat) <$> getPlayer two
      otherThreat <- (.threat) <$> getPlayer one
      run $ EngageEnemy two enemy
      (.threat) <$> getPlayer two `shouldReturn` threat
      run $ CheckWindows [mkAfter $ OptionallyEngaged enemy two]
      (.threat) <$> getPlayer two `shouldReturn` threat + 7
      (.threat) <$> getPlayer one `shouldReturn` otherThreat

    it "does not raise threat once it is active" . gameTest $ \pid -> do
      gulley <- stageLocation Locations.deepGulley
      run $ PlaceLocation gulley ActiveLocation
      enemy <- stageEnemy Enemies.forestSpider
      threat <- (.threat) <$> getPlayer pid
      run $ CheckWindows [mkAfter $ OptionallyEngaged enemy pid]
      (.threat) <$> getPlayer pid `shouldReturn` threat

  describe "Strange Woods" do
    it "prevents healing every player's characters only while staged" . twoPlayerTest testDeck $ \one two -> do
      woods <- stageLocation Locations.strangeWoods
      heroes <- traverse (\pid -> heroOf pid "01012") [one, two]
      for_ heroes \hero -> do
        run $ DealDamage GameSource hero 2
        run $ HealDamage GameSource hero 2
        getDamage hero `shouldReturn` 2
      run $ PlaceLocation woods ActiveLocation
      for_ heroes \hero -> do
        run $ HealDamage GameSource hero 2
        getDamage hero `shouldReturn` 0

    for_ [False, True] \destroyed ->
      it ("raises everyone's threat from its shadow only if the attack destroys a character: " <> show destroyed) . twoPlayerTest testDeck $ \one two -> do
        enemy <- stageEnemy Enemies.longTrailer
        card <- genEncounterCard Locations.strangeWoods
        run $ AttachShadowCard enemy card
        defender <- if destroyed then putAllyIntoPlay one Allies.guardOfTheCitadel else heroOf one "01012"
        threats <- traverse (fmap (.threat) . getPlayer) [one, two]
        run $ EnemyAttack one enemy
        passActions one
        passActions two
        chooseTarget one defender
        passActions one
        passActions two
        passActions one
        passActions two
        traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` map (+ if destroyed then 2 else 0) threats

  describe "Wall of Trees" do
    it "leaves active-location progress alone and takes progress before the current quest" . twoPlayerTest testDeck $ \_ _ -> do
      quest <- putQuestStage Quests.fliesAndSpiders
      wall <- stageLocation Locations.wallOfTrees
      road <- stageLocation Locations.oldForestRoad
      run $ PlaceLocation road ActiveLocation
      getLocationThreat wall `shouldReturn` 2
      run $ PlaceTokens GameSource (LocationTarget road) Progress 1
      (.progress) . toAttrs <$> getLocation wall `shouldReturn` 0
      run $ PlaceTokens GameSource quest Progress 3
      (.progress) . toAttrs <$> getLocation wall `shouldReturn` 3
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0
      run $ PlaceTokens GameSource quest Progress 3
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 2
      cards <- victoryLocations
      map toCardCode cards `shouldBe` [toCardCode Locations.wallOfTrees]
      traverse locationVictoryPoints cards `shouldReturn` [2]

    it "lets the first player order multiple walls and spills excess progress onto the quest" . gameTest $ \pid -> do
      quest <- putQuestStage Quests.fliesAndSpiders
      wallOne <- stageLocation Locations.wallOfTrees
      wallTwo <- stageLocation Locations.wallOfTrees
      run $ PlaceTokens GameSource quest Progress 9
      chooseTarget pid (LocationTarget wallTwo)
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 1
      getsGame (map toId . toList . (.entities.locations)) >>= (`shouldSatisfy` (\xs -> all (`notElem` xs) [wallOne, wallTwo]))
      cards <- victoryLocations
      length cards `shouldBe` 2
      traverse locationVictoryPoints cards `shouldReturn` [1, 1]

  describe "Oppressive Forest" do
    it "counts cards played before it arrived and limits only the player who has played" . twoPlayerTest testDeck $ \one two -> do
      run $ CreateEffect $ Effect GameSource (PlayerTarget one) [PlayedCardThisRound (toCardCode Events.aTestOfWill) EventType] UntilEndOfRound
      forest <- stageLocation Locations.oppressiveForest
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` elem (CannotPlay AnyCard))
      getModifiers (PlayerTarget two) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))
      run $ EndEffects UntilEndOfRound
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))
      run $ CreateEffect $ Effect GameSource (PlayerTarget one) [PlayedCardThisRound (toCardCode Events.aTestOfWill) EventType] UntilEndOfRound
      run $ PlaceLocation forest ActiveLocation
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))

    for_ [False, True] \staged ->
      it ("adds its shadow to staging only when no locations are staged: " <> show staged) . gameTest $ \pid -> do
        when staged $ void $ stageLocation Locations.oldForestRoad
        enemy <- stageEnemy Enemies.forestSpider
        card <- genEncounterCard Locations.oppressiveForest
        run $ AttachShadowCard enemy card
        run $ ResolveShadowCard card.id enemy pid []
        getsGame (any ((== toCardCode Locations.oppressiveForest) . toCardCode) . toList . (.entities.locations)) `shouldReturn` not staged

  describe "Old Man Willow" do
    it "discards a location from victory and raises only the defending player's threat by its points" . twoPlayerTest testDeck $ \one two -> do
      willow <- stageEnemy Enemies.oldManWillow
      troll <- stageEnemy Enemies.hillTroll
      run $ AddToVictoryDisplay (EnemyTarget troll)
      victoryLocation Locations.theStoneOfErechStandalone
      threatOne <- (.threat) <$> getPlayer one
      threatTwo <- (.threat) <$> getPlayer two
      run $ UseCardAbility two (EnemySource willow) 1 [mkWhen $ EnemyAttacking willow two]
      (.threat) <$> getPlayer one `shouldReturn` threatOne
      (.threat) <$> getPlayer two `shouldReturn` threatTwo + 5
      cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      map toCardCode cards `shouldBe` [toCardCode Enemies.hillTroll]
      getEncounterDiscard >>= (\xs -> map toCardCode xs `shouldSatisfy` elem (toCardCode Locations.theStoneOfErechStandalone))

    it "does nothing when victory contains no locations" . gameTest $ \pid -> do
      willow <- stageEnemy Enemies.oldManWillow
      threat <- (.threat) <$> getPlayer pid
      run $ UseCardAbility pid (EnemySource willow) 1 [mkWhen $ EnemyAttacking willow pid]
      (.threat) <$> getPlayer pid `shouldReturn` threat
      getQuestion pid `shouldReturn` Nothing

    it "remains in play with damage greater than its hit points" . gameTest $ \_ -> do
      willow <- stageEnemy Enemies.oldManWillow
      run $ DealDamage GameSource (EnemyTarget willow) 20
      getDamage (EnemyTarget willow) `shouldReturn` 20
      (.placement) <$> getEnemy willow `shouldReturn` StagingArea

  describe "Grasping Root" do
    it "assigns damage equal to the number of victory locations rather than their points" . twoPlayerTest testDeck $ \one two -> do
      root <- stageEnemy Enemies.graspingRoot
      victoryLocation Locations.theStoneOfErechStandalone
      victoryLocation Locations.tarlangsNeck
      troll <- stageEnemy Enemies.hillTroll
      run $ AddToVictoryDisplay (EnemyTarget troll)
      beravor <- heroOf one "01012"
      eowyn <- heroOf one "01007"
      other <- heroOf two "01012"
      run $ EngageEnemy one root
      chooseTarget one beravor
      chooseTarget one eowyn
      getDamage beravor `shouldReturn` 1
      getDamage eowyn `shouldReturn` 1
      getDamage other `shouldReturn` 0

    it "assigns no damage with an empty victory display" . gameTest $ \pid -> do
      root <- stageEnemy Enemies.graspingRoot
      run $ EngageEnemy pid root
      getQuestion pid `shouldReturn` Nothing

  describe "Army of Flies" do
    it "discards an attachment controlled by the engaged player" . twoPlayerTest testDeck $ \one two -> do
      flies <- stageEnemy Enemies.armyOfFlies
      heroOne <- heroOf one "01012"
      heroTwo <- heroOf two "01012"
      own <- putAttachmentOn one Attachments.resourceful heroOne
      other <- putAttachmentOn two Attachments.resourceful heroTwo
      run $ EngageEnemy one flies
      getsGame (elem own . map toTarget . toList . (.entities.attachments)) `shouldReturn` False
      getsGame (elem other . map toTarget . toList . (.entities.attachments)) `shouldReturn` True

    it "also discards a defending player's attachment as a shadow effect" . gameTest $ \pid -> do
      flies <- stageEnemy Enemies.armyOfFlies
      cid <- (.id) . toCard <$> getEnemy flies
      hero <- heroOf pid "01012"
      attachment <- putAttachmentOn pid Attachments.resourceful hero
      run $ ResolveShadowEffect cid flies pid [hero]
      getsGame (elem attachment . map toTarget . toList . (.entities.attachments)) `shouldReturn` False

  describe "Long Trailer" do
    it "attacks the first player from staging when the active location is explored" . twoPlayerTest testDeck $ \one two -> do
      trailer <- stageEnemy Enemies.longTrailer
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame $ toId . fromJustNote "location missing" . headMay . toList . (.entities.locations)
      defender <- heroOf one "01012"
      other <- heroOf two "01012"
      run $ CheckWindows [mkAfter $ ActiveLocationExplored lid]
      passActions one
      passActions two
      chooseTarget one defender
      passActions one
      passActions two
      passActions one
      passActions two
      getDamage defender `shouldReturn` 2
      getDamage other `shouldReturn` 0
      (.placement) <$> getEnemy trailer `shouldReturn` StagingArea

    it "does not make the attack while engaged" . gameTest $ \pid -> do
      trailer <- stageEnemy Enemies.longTrailer
      run $ EngageEnemy pid trailer
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame $ toId . fromJustNote "location missing" . headMay . toList . (.entities.locations)
      run $ CheckWindows [mkAfter $ ActiveLocationExplored lid]
      getQuestion pid `shouldReturn` Nothing
