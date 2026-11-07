module LordOfTheRings.Standalone.DolGuldurEncountersSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Helpers.Window (shadowCardCancelable)
import LordOfTheRings.Keyword (Keyword (Archery, Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (getEnemy, getAlly, hasKeyword, getMainQuestProgress, getThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (EncounterPhase, QuestPhase))
import LordOfTheRings.Window (WindowType (..), mkWhen, mkAfter)
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Standalone.AnnuminasInvadersSpec (mainStage)
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, defendAttack, strongDefender)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

reveal :: CardDef -> GameT ()
reveal def = do
  _ <- setEncounterDeck [def, Locations.oldForestRoad]
  run RevealEncounterCard
  resolveRevealed

stagePower :: PlayerId -> GameT ObjectiveId
stagePower pid = do
  card <- genEncounterCard Objectives.powerOfLorien
  run $ PutObjectiveIntoPlayAttached card $ PlayerTarget pid
  getsGame $ toId . fromJustNote "power missing" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.objectives)

spec :: Spec
spec = describe "Dol Guldur encounter cards" do
  describe "Invaders" do
    it "makes the Host's threat and archery equal the local player count" . twoPlayerTest testDeck $ \_ _ -> do
      eid <- stageEnemy Enemies.hostOfDolGuldur
      getEnemyThreat eid `shouldReturn` 2
      foe <- getEnemy eid
      hasKeyword (Archery 2) (EnemyTarget eid) (toCardDef foe) `shouldReturn` True
      getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)

    it "removes progress for a staged Host at the end of encounter, but not an engaged one" . gameTest $ \pid -> do
      _ <- mainStage 5
      eid <- stageEnemy Enemies.hostOfDolGuldur
      run $ CheckWindows [mkWhen $ PhaseEnds EncounterPhase]
      getMainQuestProgress `shouldReturn` 4
      run $ EngageEnemy pid eid
      run $ CheckWindows [mkWhen $ PhaseEnds EncounterPhase]
      getMainQuestProgress `shouldReturn` 4

    it "adds a persistent resource and stat bonuses when the Marauder cannot remove progress" . gameTest $ \pid -> do
      _ <- mainStage 0
      eid <- stageEnemy Enemies.dolGuldurMarauder
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getEnemyAttack eid `shouldReturn` 4
      getEnemyDefense eid `shouldReturn` 2
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getEnemyAttack eid `shouldReturn` 5

    it "lets the Marauder remove a complete progress instead of gaining a resource" . gameTest $ \pid -> do
      _ <- mainStage 2
      eid <- stageEnemy Enemies.dolGuldurMarauder
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      chooseLabel "$label.dolGuldur.removeProgress count=i:1" pid
      getMainQuestProgress `shouldReturn` 1
      getEnemyAttack eid `shouldReturn` 3

    it "has the Warg discard a controlled attachment when no progress can be removed" . gameTest $ \pid -> do
      _ <- mainStage 0
      hero <- heroOf pid "01012"
      _ <- putAttachmentOn pid Attachments.citadelPlate hero
      eid <- stageEnemy Enemies.battleWargDolGuldur
      run $ EngageEnemy pid eid
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

    it "removes the available partial progress for an Archer when no ally can be damaged" . gameTest $ \pid -> do
      _ <- mainStage 1
      eid <- stageEnemy Enemies.amonLancArcher
      run $ EngageEnemy pid eid
      getMainQuestProgress `shouldReturn` 0

    it "damages an ally for the Archer when two progress are unavailable" . gameTest $ \pid -> do
      _ <- mainStage 0
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      eid <- stageEnemy Enemies.amonLancArcher
      run $ EngageEnemy pid eid
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      -- The chosen ally had only 2 hit points and is destroyed by the full damage.
      getQuestion pid `shouldReturn` Nothing

    it "does not offer an impossible additional shadow when the encounter deck is empty" . gameTest $ \pid -> do
      _ <- mainStage 1
      eid <- stageEnemy Enemies.beastOfTaurNuFuin
      _ <- setEncounterDeck []
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getMainQuestProgress `shouldReturn` 0
      getQuestion pid `shouldReturn` Nothing

    it "prevents cancellation of shadow cards dealt to the Beast" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.beastOfTaurNuFuin
      cards <- setEncounterDeck [Treacheries.fastnessOfMirkwood]
      run $ DealShadowCard eid
      for_ cards $ \card -> shadowCardCancelable card.id `shouldReturn` False

    it "returns the attacker immediately for the Olog shadow, without waiting for attack end" . gameTest $ \pid -> do
      _ <- mainStage 2
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy pid eid
      cards <- setEncounterDeck [Enemies.mirkwoodOlogHai]
      run $ DealShadowCard eid
      for_ cards $ \card -> run $ ResolveShadowCard card.id eid pid []
      chooseLabel "$label.dolGuldur.returnToStaging" pid
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      getMainQuestProgress `shouldReturn` 2

    it "removes partial progress when the Olog shadow's attacker cannot enter staging" . gameTest $ \pid -> do
      _ <- mainStage 1
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy pid eid
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotEnterStagingArea] UntilEndOfRound
      cards <- setEncounterDeck [Enemies.mirkwoodOlogHai]
      run $ DealShadowCard eid
      for_ cards $ \card -> run $ ResolveShadowCard card.id eid pid []
      getMainQuestProgress `shouldReturn` 0
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith pid

    it "gives all four Lieutenants printed indestructibility and player-card immunity" . gameTest $ \_ -> do
      for_ [Enemies.baneOfAmonLanc, Enemies.sorcererOfDolGuldurAttack, Enemies.chieftainMorlug, Enemies.dungeonDweller] $ \def -> do
        eid <- stageEnemy def
        foe <- getEnemy eid
        hasKeyword Indestructible (EnemyTarget eid) (toCardDef foe) `shouldReturn` True
        getModifiers eid >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

    it "has the Dungeon Dweller discard the attacked player's ally" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      eid <- stageEnemy Enemies.dungeonDweller
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getsGame (length . (.entities.allies)) `shouldReturn` 0

    it "puts Chieftain Morlug's discarded enemy into play engaged without revealing its surge" . gameTest $ \pid -> do
      _ <- mainStage 0
      eid <- stageEnemy Enemies.chieftainMorlug
      _ <- setEncounterDeck [Locations.oldForestRoad, Enemies.battleWargDolGuldur, Locations.oldForestRoad]
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      enemies <- getsGame $ toList . (.entities.enemies)
      map (.placement) [foe | foe <- enemies, toCardCode foe == toCardCode Enemies.battleWargDolGuldur] `shouldBe` [EngagedWith pid]
      length <$> getEncounterDeck `shouldReturn` 1
      getsGame (length . (.entities.locations)) `shouldReturn` 0

    it "removes only the actual excess damage from the main quest when the Olog destroys a defender" . gameTest $ \pid -> do
      _ <- mainStage 10
      eid <- stageEnemy Enemies.mirkwoodOlogHai
      run $ EngageEnemy pid eid
      defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      getMainQuestProgress `shouldReturn` 6

    it "uses the randomly discarded card's printed cost only for the Bane's current attack" . gameTest $ \pid -> do
      _ <- putCardIntoHand pid Events.sneakAttack
      eid <- stageEnemy Enemies.baneOfAmonLanc
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getEnemyAttack eid `shouldReturn` 6
      player <- getPlayer pid
      length player.hand `shouldBe` 0
      run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
      getEnemyAttack eid `shouldReturn` 5

    it "does not duplicate Morlug's enemy if discarding the final card resets the encounter deck" . gameTest $ \pid -> do
      _ <- mainStage 0
      eid <- stageEnemy Enemies.chieftainMorlug
      cards <- setEncounterDeck [Enemies.battleWargDolGuldur]
      current <- getGame
      putGame $ current & phaseL .~ QuestPhase
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      let ids = map (.id) cards
      getsGame (length . filter ((`elem` ids) . (.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` 1
      map (.id) <$> getEncounterDeck >>= (`shouldSatisfy` all (`notElem` ids))
      map (.id) <$> getEncounterDiscard >>= (`shouldSatisfy` all (`notElem` ids))

    it "discards all copies in play of the Sorcerer's discarded player card" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
      current <- getGame
      putGame $ current & playersL . ix pid . deckL .~ [PlayerCard card]
      eid <- stageEnemy Enemies.sorcererOfDolGuldurAttack
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      player <- getPlayer pid
      length player.deck `shouldBe` 0

  describe "Locations" do
    for_ [(Locations.gateOfDolGuldurAttack, Quests.theSiegeOfDolGuldur), (Locations.theSorcerersTower, Quests.theTowerOfSorcery), (Locations.theShadowRoad, Quests.battleUnderTheTrees), (Locations.dungeonDoor, Quests.dungeonsOfTheNecromancer)] $ \(def, stageDef) ->
      it ("allows travel and staged progress only at its matching stage: " <> show (toCardCode def)) . gameTest $ \_ -> do
        lid <- stageLocation def
        getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
        run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
        (.progress) <$> getLocation lid `shouldReturn` 0
        stage <- putQuestStage stageDef
        -- The matching B-side is active; its revelation is tested by the quest suite.
        current <- getGame
        let flipped = current & entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})
        putGame flipped
        putGame =<< preloadModifiers flipped
        getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
        run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
        (.progress) <$> getLocation lid `shouldReturn` 1
        stage `shouldSatisfy` (\case QuestTarget _ -> True; _ -> False)

    it "limits cumulative player-card threat reduction, while allowing encounter reductions" . gameTest $ \pid -> do
      run $ RaiseThreat pid 20
      _ <- stageLocation Locations.gateOfDolGuldurAttack
      card <- putCardIntoHand pid Events.theGaladhrimsGreeting
      runQueueT $ Lifted.lowerThreatBy (CardIdSource card.id) pid 6
      run $ Run []
      getThreat pid `shouldReturn` 19
      runQueueT $ Lifted.lowerThreatBy (CardIdSource card.id) pid 6
      run $ Run []
      getThreat pid `shouldReturn` 19
      runQueueT $ Lifted.lowerThreatBy GameSource pid 3
      run $ Run []
      getThreat pid `shouldReturn` 16

    it "recalculates the Forest's threat from the player controlling the most allies" . twoPlayerTest testDeck $ \one two -> do
      lid <- stageLocation Locations.forestBattleground
      _ <- putAllyIntoPlay one Allies.guardOfTheCitadel
      _ <- putAllyIntoPlay two Allies.guardOfTheCitadel
      _ <- putAllyIntoPlay two Allies.guardOfTheCitadel
      getLocationThreat lid `shouldReturn` 3

    it "captures a played event under Shadow Road and returns it when the Road leaves play" . gameTest $ \pid -> do
      lid <- stageLocation Locations.theShadowRoad
      card <- genPlayerCard Events.sneakAttack (Just pid)
      run $ PutInDiscard card
      current <- getGame
      putGame $ current & phaseL .~ QuestPhase
      run $ CheckWindows [mkAfter $ PlayedCard pid card]
      length . (.underneath) . toAttrs <$> getLocation lid `shouldReturn` 1
      getLocationThreat lid `shouldReturn` 4
      run $ Discard GameSource $ LocationTarget lid
      player <- getPlayer pid
      map (.id) player.discard `shouldSatisfy` elem card.id

    it "limits each character to one point healed per round while Dungeon Door is staged" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      _ <- stageLocation Locations.dungeonDoor
      run $ PlaceTokens GameSource hero Damage 3
      run $ HealDamage GameSource hero 3
      getDamage hero `shouldReturn` 2
      run $ HealDamage GameSource hero 3
      getDamage hero `shouldReturn` 2

    it "raises every player's threat by the explored Woods' discarded card's printed threat" . twoPlayerTest testDeck $ \one two -> do
      lid <- stageLocation Locations.blackenedWoods
      _ <- setEncounterDeck [Enemies.amonLancArcher, Locations.oldForestRoad]
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
      getThreat one `shouldReturn` 2
      getThreat two `shouldReturn` 2
      getsGame (length . (.entities.enemies)) `shouldReturn` 0

    it "has each player search for an Orc when Forest Battleground is explored" . twoPlayerTest testDeck $ \one two -> do
      lid <- stageLocation Locations.forestBattleground
      cards <- setEncounterDeck [Enemies.amonLancArcher, Enemies.dolGuldurMarauder]
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
      case cards of
        [archer, marauder] -> do
          chooseTarget one $ CardIdTarget archer.id
          chooseTarget two $ CardIdTarget marauder.id
        _ -> error "invalid encounter fixture"
      getsGame (length . (.entities.enemies)) `shouldReturn` 2
      enemies <- getsGame $ toList . (.entities.enemies)
      map (.placement) enemies `shouldBe` [StagingArea, StagingArea]

    it "has the staged Sorcerer's Tower reveal a new card after a when-revealed effect is canceled" . gameTest $ \pid -> do
      _ <- stageLocation Locations.theSorcerersTower
      cards <- setEncounterDeck [Locations.oldForestRoad]
      for_ cards $ \card -> run $ CheckWindows [mkAfter $ WhenRevealedCanceled pid $ CardIdTarget card.id]
      getsGame (length . (.entities.locations)) `shouldReturn` 2
      length <$> getEncounterDeck `shouldReturn` 0

  describe "Treacheries and Lorien support" do
    for_ [False, True] $ \chooseLocation ->
      it ("resolves Power Sevenfold choices against the remaining supply, location first: " <> show chooseLocation) . twoPlayerTest testDeck $ \one two -> do
        _ <- mainStage 0
        cards <- setEncounterDeck [Treacheries.powerSevenfold, Locations.gateOfDolGuldurAttack, Enemies.amonLancArcher]
        run RevealEncounterCard
        resolveRevealed
        chooseLabel (if chooseLocation then "$label.dolGuldur.searchLocation" else "$label.dolGuldur.searchOrc") one
        case cards of
          [_power, locationCard, orcCard] -> do
            chooseTarget one $ CardIdTarget $ if chooseLocation then locationCard.id else orcCard.id
            chooseTarget two $ CardIdTarget $ if chooseLocation then orcCard.id else locationCard.id
          _ -> error "invalid Power Sevenfold fixture"
        getThreat one `shouldReturn` 1
        getThreat two `shouldReturn` 1
        getsGame (length . (.entities.locations)) `shouldReturn` 1
        enemies <- getsGame $ toList . (.entities.enemies)
        map (.placement) enemies `shouldBe` [EngagedWith $ if chooseLocation then two else one]

    it "uses modified engagement cost to choose Servant of Sauron's attacker and still surges" . gameTest $ \pid -> do
      host <- stageEnemy Enemies.hostOfDolGuldur
      bane <- stageEnemy Enemies.baneOfAmonLanc
      run $ CreateEffect $ Effect GameSource (EnemyTarget host) [EngagementCostModifier 20] UntilEndOfRound
      defender <- strongDefender pid
      _ <- setEncounterDeck [Treacheries.servantOfSauronDolGuldur, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      defendAttack pid defender
      getModifiers host >>= (`shouldSatisfy` elem AttackedThisPhase)
      getModifiers bane >>= (`shouldSatisfy` notElem AttackedThisPhase)
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "damages exhausted characters and dynamically penalizes damaged willpower until phase end" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      run $ Exhaust hero
      reveal Treacheries.theNecromancersRage
      getDamage hero `shouldReturn` 1
      getWillpower hero `shouldReturn` 1
      run $ HealDamage GameSource hero 1
      getWillpower hero `shouldReturn` 2

    it "caps progress on the current quest and surges while city strength is at most five" . gameTest $ \_ -> do
      target <- mainStage 0
      reveal Treacheries.fastnessOfMirkwood
      getsGame (length . (.entities.locations)) `shouldReturn` 1
      run $ PlaceTokens GameSource target Progress 10
      getMainQuestProgress `shouldReturn` 5

    it "recalculates the Necromancer's shadow defense penalty as the defender is healed" . gameTest $ \pid -> do
      defender <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource defender [DefenseModifier 10] UntilEndOfRound
      run $ PlaceTokens GameSource defender Damage 1
      initialDefense <- getDefense defender
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Treacheries.theNecromancersRage, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      getDamage defender `shouldReturn` 2
      getDefense defender `shouldReturn` initialDefense - 2
      run $ HealDamage GameSource defender 1
      getDefense defender `shouldReturn` initialDefense - 1

    it "blocks player readying after Fear and Despair but allows encounter readying" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      reveal Treacheries.fearAndDespairDolGuldur
      chooseTarget pid hero
      runQueueT $ Lifted.ready hero
      run $ Run []
      h <- getHeroWithCode "01012"
      heroExhausted (toAttrs h) `shouldBe` True
      runQueueT $ Lifted.encounterReadies hero
      run $ Run []
      heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

    it "keeps Host of Lorien ready when it quests and removes it from the game on leaving" . gameTest $ \pid -> do
      card <- genEncounterCard Objectives.hostOfLorien
      run $ PutObjectiveAllyIntoPlayUnder pid card
      ally <- getsGame $ fromJustNote "host missing" . headMay . toList . (.entities.allies)
      getModifiers ally >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ CommitToQuest $ toTarget ally
      allyExhausted . toAttrs <$> getAlly (toId ally) `shouldReturn` False
      run $ Discard GameSource $ toTarget ally
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      map (.id) <$> getEncounterDiscard >>= (`shouldSatisfy` notElem card.id)

    it "pays Power of Lorien into victory and still cancels the whole revealed card" . gameTest $ \pid -> do
      oid <- stagePower pid
      _ <- setEncounterDeck [Enemies.battleWargDolGuldur, Locations.oldForestRoad]
      run RevealEncounterCard
      chooseAbilityOf pid oid
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      getsGame (length . (.entities.objectives)) `shouldReturn` 0
      length <$> getEncounterDeck `shouldReturn` 1
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Objectives.powerOfLorien]
      getsGame (null . gameResolvingObjectives) `shouldReturn` True

    it "keeps the Power response with its controller after the first player changes" . twoPlayerTest testDeck $ \one two -> do
      oid <- stagePower one
      run $ SetFirstPlayer two
      _ <- setEncounterDeck [Enemies.battleWargDolGuldur, Locations.oldForestRoad]
      run RevealEncounterCard
      offersAbilityOf one oid `shouldReturn` True
      offersAbilityOf two oid `shouldReturn` False
