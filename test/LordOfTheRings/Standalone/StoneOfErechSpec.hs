module LordOfTheRings.Standalone.StoneOfErechSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Window (travelableLocations, getActionAbilities)
import LordOfTheRings.Phase (Phase (PlanningPhase, EncounterPhase, RefreshPhase))
import LordOfTheRings.Sphere (Sphere (Lore, Spirit, Fellowship, Baggins))
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Character (getAttackStrength)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationThreat, getLocationQuestPoints)
import LordOfTheRings.Keyword (Keyword (Spectral))
import LordOfTheRings.Matcher (getObjective, getThreat, getEnemy, getAlly, getHero, heroHasResourceIcon, HeroMatcher (HeroWithSphere), Query (select), CardMatcher (CardWithType))
import LordOfTheRings.Modifier (ModifierType (AttackModifier, WillpowerModifier, AttackBonusAgainst, LoseKeyword, Blank, CannotPlay, RevealedEncounterCardsGainDoomed, GainResourceIcon, CannotExhaust, GainResourceIconFor, PrintedResourceIconForCard))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight, currentNightObjective)
import LordOfTheRings.Trait (Trait (Oathbreaker))
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

stageNight :: CardDef -> GameT Target
stageNight def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toTarget . fromJustNote "Night missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives)

prepareNights :: GameT Target
prepareNights = do
  for_ [Objectives.dusk, Objectives.midnight] \def -> run . SetAside =<< genEncounterCard def
  stageNight Objectives.eventide

nightProgress :: GameT Int
nightProgress = do
  oid <- fromJustNote "no current Night" <$> currentNightObjective
  (.progress) . toAttrs <$> getObjective oid

stageCaptain :: GameT EnemyId
stageCaptain = do
  run . AddToStaging =<< genEncounterCard Enemies.shadowHostCaptain
  getsGame $ toId . fromJustNote "captain missing" . headMay . toList . (.entities.enemies)

endRoundWindow :: GameT ()
endRoundWindow = run $ CheckWindows [mkWhen RoundEnds]

spec :: Spec
spec = describe "Stone of Erech" do
  describe "Erech locations" do
    it "gives The Stone of Erech two threat per player" . twoPlayerTest testDeck $ \_ _ -> do
      stone <- stageErechLocation Locations.theStoneOfErechStandalone
      getLocationThreat stone `shouldReturn` 4

    it "exhausts low-willpower characters to travel, prevents their readying, and reacts to willpower bonuses" . gameTest $ \pid -> do
      stone <- stageErechLocation Locations.theStoneOfErechStandalone
      theodred <- heroOf pid "01002"
      beravor <- heroOf pid "01012"
      run $ TravelTo stone
      (.placement) <$> getLocation stone `shouldReturn` ActiveLocation
      heroExhaustedTarget theodred `shouldReturn` True
      heroExhaustedTarget beravor `shouldReturn` False
      run $ Ready theodred
      heroExhaustedTarget theodred `shouldReturn` True
      run $ CreateEffect $ Effect GameSource theodred [WillpowerModifier 1] UntilEndOfRound
      run $ Ready theodred
      heroExhaustedTarget theodred `shouldReturn` False

    it "also evaluates an ally's current willpower when it readies" . gameTest $ \pid -> do
      stone <- stageErechLocation Locations.theStoneOfErechStandalone
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ TravelTo stone
      let exhausted = case ally of AllyTarget aid -> allyExhausted . toAttrs <$> getAlly aid; _ -> error "ally missing"
      exhausted `shouldReturn` True
      run $ Ready ally
      exhausted `shouldReturn` True
      run $ CreateEffect $ Effect GameSource ally [WillpowerModifier 2] UntilEndOfRound
      run $ Ready ally
      exhausted `shouldReturn` False

    it "bars travel if a character needed for its cost is already exhausted" . gameTest $ \pid -> do
      stone <- stageErechLocation Locations.theStoneOfErechStandalone
      run . Exhaust =<< heroOf pid "01002"
      travelableLocations >>= (`shouldSatisfy` notElem stone)
      run $ TravelTo stone
      (.placement) <$> getLocation stone `shouldReturn` StagingArea

    it "keeps the Stone immune to player progress effects, and puts it in victory when explored" . gameTest $ \pid -> do
      stone <- stageErechLocation Locations.theStoneOfErechStandalone
      hero <- heroOf pid "01012"
      let source = case hero of HeroTarget hid -> HeroSource hid; _ -> GameSource
      run $ PlaceTokens source (toTarget stone) Progress 6
      (.progress) <$> getLocation stone `shouldReturn` 0
      run $ PlaceTokens GameSource (toTarget stone) Progress 6
      cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      map toCardCode cards `shouldBe` [toCardCode Locations.theStoneOfErechStandalone]

    it "lets Tarlang's Neck end the encounter phase before engagement checks" . gameTest $ \pid -> do
      neck <- stageErechLocation Locations.tarlangsNeck
      run $ PlaceLocation neck ActiveLocation
      eid <- stageErechEnemy Enemies.restlessDead
      initial <- getThreat pid
      run $ Run [BeginPhase EncounterPhase, RaiseThreat pid 10, EndPhase EncounterPhase]
      chooseLabel "$label.erech.endEncounter" pid
      getThreat pid `shouldReturn` initial
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "lets Tarlang's Neck make the first player engage every staged enemy" . twoPlayerTest testDeck $ \one two -> do
      neck <- stageErechLocation Locations.tarlangsNeck
      run $ PlaceLocation neck ActiveLocation
      enemies <- traverse stageErechEnemy [Enemies.restlessDead, Enemies.shadowMan]
      initial <- getThreat one
      run $ Run [BeginPhase EncounterPhase, RaiseThreat one 1, EndPhase EncounterPhase]
      chooseLabel "$label.erech.engageAll" one
      replicateM_ 3 do
        passActions one
        passActions two
      for_ enemies \eid -> (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith one
      getThreat one `shouldReturn` initial + 1

    it "reduces staging locations' threat only while Banks of Morthond is active" . gameTest $ \_ -> do
      banks <- stageErechLocation Locations.banksOfMorthond
      vale <- stageErechLocation Locations.valeOfShadows
      eid <- stageErechEnemy Enemies.whisperer
      getLocationThreat vale `shouldReturn` 1
      run $ PlaceLocation banks ActiveLocation
      getLocationThreat vale `shouldReturn` 0
      getLocationThreat banks `shouldReturn` 2
      getEnemyThreat eid `shouldReturn` 2
      run $ PlaceLocation banks StagingArea
      getLocationThreat vale `shouldReturn` 1

    for_ [(True, 1), (False, 0)] \(killed, progress) ->
      it ("progresses Night from Banks of Morthond's shadow only if the attack kills: " <> show killed) . gameTest $ \pid -> do
        _ <- prepareNights
        eid <- stageErechEnemy Enemies.forestSpider
        defender <- if killed then putAllyIntoPlay pid Allies.guardOfTheCitadel else heroOf pid "01012"
        run $ EngageEnemy pid eid
        card <- genEncounterCard Locations.banksOfMorthond
        run $ AttachShadowCard eid card
        run $ EnemyAttack pid eid
        passActions pid
        chooseTarget pid defender
        passActions pid
        passActions pid
        nightProgress `shouldReturn` progress

    for_ [(Objectives.eventide, 3, 1), (Objectives.dusk, 5, 3), (Objectives.midnight, 7, 5)] \(night, points, threat) ->
      it ("applies the Night bonuses to Vale of Shadows and Shadow of Dwimorberg at " <> show (toCardCode night)) . gameTest $ \_ -> do
        _ <- stageNight night
        vale <- stageErechLocation Locations.valeOfShadows
        shadow <- stageErechLocation Locations.shadowOfDwimorberg
        getLocationQuestPoints vale `shouldReturn` points
        getLocationThreat shadow `shouldReturn` threat

    it "makes revealed Vale of Shadows active and returns the previous active location to staging" . gameTest $ \_ -> do
      banks <- stageErechLocation Locations.banksOfMorthond
      run $ PlaceLocation banks ActiveLocation
      _ <- setEncounterDeck [Locations.valeOfShadows]
      run RevealEncounterCard
      resolveRevealed
      (.placement) <$> getLocation banks `shouldReturn` StagingArea
      locations <- getsGame (toList . (.entities.locations))
      let vale = fromJustNote "vale missing" $ find ((== toCardCode Locations.valeOfShadows) . toCardCode) locations
      (.placement) (toAttrs vale) `shouldBe` ActiveLocation

    it "returns the topmost Undead enemy engaged with the first player when Blackroot Graves is explored, without revealing it" . twoPlayerTest testDeck $ \one _ -> do
      cards <- traverse genEncounterCard [Enemies.forestSpider, Enemies.shadowMan, Enemies.whisperer]
      run $ AddToEncounterDiscard cards
      graves <- stageErechLocation Locations.blackrootGraves
      run $ PlaceTokens GameSource (toTarget graves) Progress 1
      enemy <- getsGame $ fromJustNote "returned enemy missing" . headMay . toList . (.entities.enemies)
      toCardCode enemy `shouldBe` toCardCode Enemies.shadowMan
      (.placement) (toAttrs enemy) `shouldBe` EngagedWith one
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (CannotPlay $ CardWithType EventType))

    for_ [(0, 2), (1, 0)] \(willpower, damage) ->
      it ("deals Blackroot Graves' shadow damage to a defender with willpower " <> show willpower) . gameTest $ \pid -> do
        eid <- stageErechEnemy Enemies.forestSpider
        defender <- heroOf pid "01002"
        run $ CreateEffect $ Effect GameSource defender [WillpowerModifier (willpower - 1)] UntilEndOfRound
        card <- genEncounterCard Locations.blackrootGraves
        run $ AttachShadowCard eid card
        run $ ResolveShadowCard card.id eid pid [defender]
        getDamage defender `shouldReturn` damage

  describe "Haunted Valley" do
    it "removes printed and gained standard icons from ready heroes and restores them when exhausted" . gameTest $ \pid -> do
      _ <- stageErechLocation Locations.hauntedValley
      beravor <- heroOf pid "01012"
      let hid = case beravor of HeroTarget ident -> ident; _ -> error "hero missing"
      run $ CreateEffect $ Effect GameSource beravor [GainResourceIcon Spirit, GainResourceIcon Fellowship, GainResourceIcon Baggins] UntilEndOfRound
      hero <- getHero hid
      heroHasResourceIcon Lore hero `shouldReturn` False
      heroHasResourceIcon Spirit hero `shouldReturn` False
      heroHasResourceIcon Fellowship hero `shouldReturn` True
      heroHasResourceIcon Baggins hero `shouldReturn` True
      select (HeroWithSphere Lore) >>= (`shouldSatisfy` notElem hid)
      run $ Exhaust beravor
      heroHasResourceIcon Lore hero `shouldReturn` True
      heroHasResourceIcon Spirit hero `shouldReturn` True
      select (HeroWithSphere Lore) >>= (`shouldSatisfy` elem hid)

    it "blocks paying for both paid and zero-cost cards until the appropriate hero exhausts" . gameTest $ \pid -> do
      _ <- stageErechLocation Locations.hauntedValley
      beravor <- heroOf pid "01012"
      eowyn <- heroOf pid "01007"
      run $ PlaceTokens GameSource eowyn Resource 3
      free <- putCardIntoHand pid Events.daeronsRunes
      paid <- putCardIntoHand pid Events.theGaladhrimsGreeting
      player <- getPlayer pid
      canPlay player free `shouldReturn` False
      canPlay player paid `shouldReturn` False
      run $ Exhaust beravor
      run $ Exhaust eowyn
      canPlay player free `shouldReturn` True
      canPlay player paid `shouldReturn` True

    it "also removes card-specific gained icons used for payment" . gameTest $ \pid -> do
      _ <- stageErechLocation Locations.hauntedValley
      eowyn <- heroOf pid "01007"
      card <- putCardIntoHand pid Events.daeronsRunes
      run $ CreateEffect $ Effect GameSource eowyn [GainResourceIconFor (CardWithType EventType) Lore, PrintedResourceIconForCard card.id Lore] UntilEndOfRound
      player <- getPlayer pid
      canPlay player card `shouldReturn` False
      run $ Exhaust eowyn
      canPlay player card `shouldReturn` True

    it "lets either player exhaust a hero through its planning action" . twoPlayerTest testDeck $ \one two -> do
      valley <- stageErechLocation Locations.hauntedValley
      putGame . (phaseL .~ PlanningPhase) =<< getGame
      let available pid = any (\ab -> sourceTarget ab.abilitySource == Just (toTarget valley)) <$> getActionAbilities pid
      available one `shouldReturn` True
      available two `shouldReturn` True
      hero <- heroOf one "01012"
      run $ UseCardAbility two (LocationSource valley) 1 []
      chooseTarget two hero
      heroExhaustedTarget hero `shouldReturn` True
      putGame . (phaseL .~ RefreshPhase) =<< getGame
      available one `shouldReturn` False

    it "does not offer an exhaust action when no hero can exhaust" . gameTest $ \pid -> do
      valley <- stageErechLocation Locations.hauntedValley
      heroes <- traverse (heroOf pid) ["01002", "01007", "01012"]
      for_ heroes \hero -> run $ CreateEffect $ Effect GameSource hero [CannotExhaust] UntilEndOfRound
      abilities <- getActionAbilities pid
      abilities `shouldSatisfy` not . any (\ab -> sourceTarget ab.abilitySource == Just (toTarget valley))

    it "stops removing icons after leaving the staging area" . gameTest $ \pid -> do
      valley <- stageErechLocation Locations.hauntedValley
      target <- heroOf pid "01012"
      let hid = case target of HeroTarget ident -> ident; _ -> error "hero missing"
      hero <- getHero hid
      heroHasResourceIcon Lore hero `shouldReturn` False
      run $ PlaceLocation valley ActiveLocation
      heroHasResourceIcon Lore hero `shouldReturn` True

  describe "The Lord of the Dead" do
    it "blanks only the characters of the player engaged with him, and restores them when he leaves" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageErechEnemy Enemies.theLordOfTheDeadErech
      firstHero <- heroOf one "01012"
      secondHero <- heroOf two "01012"
      ally <- putAllyIntoPlay one Allies.guardOfTheCitadel
      run $ EngageEnemy one eid
      for_ [firstHero, ally] \target -> getModifiers target >>= (`shouldSatisfy` elem Blank)
      getModifiers secondHero >>= (`shouldSatisfy` notElem Blank)
      newcomer <- putAllyIntoPlay one Allies.guardOfTheCitadel
      getModifiers newcomer >>= (`shouldSatisfy` elem Blank)
      run $ PlaceEnemy eid StagingArea
      for_ [firstHero, ally, newcomer] \target -> getModifiers target >>= (`shouldSatisfy` notElem Blank)

    it "cannot receive player attachments" . gameTest $ \pid -> do
      eid <- stageErechEnemy Enemies.theLordOfTheDeadErech
      run $ EngageEnemy pid eid
      hosts <- cardHosts pid (toCardCode Attachments.forestSnare)
      hosts `shouldSatisfy` notElem (toTarget eid)

  describe "Shadow-man" do
    it "blocks events for the rest of the round, even after leaving play" . gameTest $ \pid -> do
      theodred <- heroOf pid "01002"
      run $ PlaceTokens GameSource theodred Resource 1
      event <- putCardIntoHand pid Events.campfireTales
      player <- getPlayer pid
      canPlay player event `shouldReturn` True
      _ <- setEncounterDeck [Enemies.shadowMan]
      run RevealEncounterCard
      resolveRevealed
      canPlay player event `shouldReturn` False
      eid <- getsGame $ toId . fromJustNote "Shadow-man missing" . headMay . toList . (.entities.enemies)
      run $ Discard GameSource (toTarget eid)
      canPlay player event `shouldReturn` False
      run $ EndEffects UntilEndOfRound
      canPlay player event `shouldReturn` True

    it "allows canceling its When Revealed before events become blocked" . gameTest $ \pid -> do
      event <- putCardIntoHand pid Events.aTestOfWill
      eowyn <- heroOf pid "01007"
      run $ PlaceTokens GameSource eowyn Resource 1
      _ <- setEncounterDeck [Enemies.shadowMan]
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget event.id)
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (CannotPlay $ CardWithType EventType))

    for_ [(Objectives.eventide, 1), (Objectives.dusk, 2), (Objectives.midnight, 3)] \(night, bonus) ->
      it ("scales its shadow attack bonus at " <> show (toCardCode night)) . gameTest $ \pid -> do
        _ <- stageNight night
        eid <- stageErechEnemy Enemies.forestSpider
        card <- genEncounterCard Enemies.shadowMan
        run $ AttachShadowCard eid card
        run $ ResolveShadowCard card.id eid pid []
        getEnemyAttack eid `shouldReturn` 2 + bonus
        run $ AttackEndedUnresolved eid
        getEnemyAttack eid `shouldReturn` 2

  describe "Restless Dead" do
    it "returns only the topmost Undead enemy, without revealing it" . gameTest $ \_ -> do
      cards <- traverse genEncounterCard [Locations.oldForestRoad, Enemies.forestSpider, Enemies.shadowMan, Enemies.whisperer]
      run $ AddToEncounterDiscard cards
      _ <- setEncounterDeck [Enemies.restlessDead]
      run RevealEncounterCard
      resolveRevealed
      foes <- getsGame (map toCardCode . toList . (.entities.enemies))
      sort foes `shouldBe` sort (map toCardCode [Enemies.restlessDead, Enemies.shadowMan])
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (CannotPlay $ CardWithType EventType))
      pile <- getEncounterDiscard
      map toCardCode pile `shouldBe` map toCardCode [Locations.oldForestRoad, Enemies.forestSpider, Enemies.whisperer]

    it "does nothing when no Undead enemy is in the discard" . gameTest $ \_ -> do
      cards <- traverse genEncounterCard [Locations.oldForestRoad, Enemies.forestSpider]
      run $ AddToEncounterDiscard cards
      _ <- setEncounterDeck [Enemies.restlessDead]
      run RevealEncounterCard
      resolveRevealed
      length <$> getsGame (.entities.enemies) `shouldReturn` 1
      getEncounterDiscard `shouldReturn` cards

  describe "Whisperer" do
    for_ [(Objectives.eventide, 2, 0), (Objectives.dusk, 3, 0), (Objectives.midnight, 4, 2)] \(night, threat, doom) ->
      it ("has the correct threat and Doomed at " <> show (toCardCode night)) . gameTest $ \pid -> do
        _ <- stageNight night
        initial <- getThreat pid
        _ <- setEncounterDeck [Enemies.whisperer]
        run RevealEncounterCard
        resolveRevealed
        eid <- getsGame $ toId . fromJustNote "Whisperer missing" . headMay . toList . (.entities.enemies)
        getEnemyThreat eid `shouldReturn` threat
        getThreat pid `shouldReturn` initial + doom
        getQuestion pid `shouldReturn` Nothing

    it "combines its Doomed with global gained Doomed" . twoPlayerTest testDeck $ \one two -> do
      _ <- stageNight Objectives.midnight
      initial <- traverse getThreat [one, two]
      run $ CreateEffect $ Effect GameSource GameTarget [RevealedEncounterCardsGainDoomed 3] UntilEndOfRound
      _ <- setEncounterDeck [Enemies.whisperer]
      run RevealEncounterCard
      resolveRevealed
      traverse getThreat [one, two] `shouldReturn` map (+ 5) initial

    it "does not offer When Revealed cancellation for its gained keyword" . gameTest $ \pid -> do
      _ <- stageNight Objectives.midnight
      initial <- getThreat pid
      event <- putCardIntoHand pid Events.aTestOfWill
      eowyn <- heroOf pid "01007"
      run $ PlaceTokens GameSource eowyn Resource 1
      _ <- setEncounterDeck [Enemies.whisperer]
      run RevealEncounterCard
      getQuestion pid `shouldReturn` Nothing
      getThreat pid `shouldReturn` initial + 2
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [event.id]

    it "does not resolve Doomed when put into play without being revealed" . gameTest $ \pid -> do
      _ <- stageNight Objectives.midnight
      initial <- getThreat pid
      eid <- stageErechEnemy Enemies.whisperer
      getEnemyThreat eid `shouldReturn` 4
      getThreat pid `shouldReturn` initial

    it "does not resolve Doomed again when Night changes on an existing enemy" . gameTest $ \pid -> do
      _ <- prepareNights
      initial <- getThreat pid
      eid <- stageErechEnemy Enemies.whisperer
      replicateM_ 8 endRoundWindow
      getEnemyThreat eid `shouldReturn` 4
      getThreat pid `shouldReturn` initial

  it "can snapshot revelation modifiers for an enemy whose text queries itself" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.urukOfMordor]
    run RevealEncounterCard
    resolveRevealed
    length <$> getsGame (.entities.enemies) `shouldReturn` 1

  describe "Regretful Shade" do
    it "surges when revealed" . gameTest $ \_ -> do
      _ <- setEncounterDeck [Enemies.regretfulShade, Enemies.shadowHostCaptain]
      run RevealEncounterCard
      resolveRevealed
      length <$> getsGame (.entities.enemies) `shouldReturn` 2

    for_ [(Objectives.eventide, 1), (Objectives.dusk, 2), (Objectives.midnight, 3)] \(night, additional) ->
      it ("deals and resolves additional shadow cards at " <> show (toCardCode night)) . gameTest $ \pid -> do
        _ <- stageNight night
        eid <- stageErechEnemy Enemies.forestSpider
        card <- genEncounterCard Enemies.regretfulShade
        run $ AttachShadowCard eid card
        _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
        run $ ResolveShadowCard card.id eid pid []
        length . (.shadowCards) . toAttrs <$> getEnemy eid `shouldReturn` 1 + additional

  describe "Night objectives" do
    it "has no current Night before the objectives enter play" . gameTest $ \_ -> do
      currentNight `shouldReturn` Nothing
      currentNightObjective `shouldReturn` Nothing

    it "progresses Eventide to Dusk after four rounds, without progressing Dusk in the same round" . gameTest $ \_ -> do
      _ <- prepareNights
      replicateM_ 3 endRoundWindow
      currentNight `shouldReturn` Just Eventide
      nightProgress `shouldReturn` 3
      endRoundWindow
      currentNight `shouldReturn` Just Dusk
      nightProgress `shouldReturn` 0
      cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      map toCardCode cards `shouldBe` [toCardCode Objectives.eventide]

    it "progresses Dusk to Midnight after eight rounds, leaving both earlier Nights in victory" . gameTest $ \_ -> do
      _ <- prepareNights
      replicateM_ 8 endRoundWindow
      currentNight `shouldReturn` Just Midnight
      nightProgress `shouldReturn` 0
      cards <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      sort (map toCardCode cards) `shouldBe` sort (map toCardCode [Objectives.eventide, Objectives.dusk])
      length <$> getsGame (.entities.objectives) `shouldReturn` 1

    it "advances as soon as an effect puts four or more progress on the current Night" . gameTest $ \_ -> do
      start <- prepareNights
      run $ PlaceTokens GameSource start Progress 6
      currentNight `shouldReturn` Just Dusk
      nightProgress `shouldReturn` 0

    it "raises every player's threat by Midnight's new total, only from its round-end ability" . twoPlayerTest testDeck $ \one two -> do
      midnight <- stageNight Objectives.midnight
      initial <- traverse getThreat [one, two]
      run $ PlaceTokens GameSource midnight Progress 2
      traverse getThreat [one, two] `shouldReturn` initial
      endRoundWindow
      nightProgress `shouldReturn` 3
      traverse getThreat [one, two] `shouldReturn` map (+ 3) initial
      endRoundWindow
      nightProgress `shouldReturn` 4
      traverse getThreat [one, two] `shouldReturn` map (+ 7) initial
      currentNight `shouldReturn` Just Midnight

    it "lets The Gloaming progress and replace the current Night, then resolve surge" . gameTest $ \_ -> do
      start <- prepareNights
      run $ PlaceTokens GameSource start Progress 3
      _ <- setEncounterDeck [Treacheries.theGloaming, Enemies.forestSpider]
      run RevealEncounterCard
      resolveRevealed
      currentNight `shouldReturn` Just Dusk
      nightProgress `shouldReturn` 0
      length <$> getsGame (.entities.enemies) `shouldReturn` 1
      getEncounterDiscard >>= (\cards -> map toCardCode cards `shouldBe` [toCardCode Treacheries.theGloaming])

  describe "Spectral" do
    it "uses modified willpower and ignores attack and attack-against-trait bonuses" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource eowyn [AttackModifier 10, AttackBonusAgainst Oathbreaker 5, WillpowerModifier 2] UntilEndOfRound
      getAttackStrength [eowyn] eid Enemies.shadowHostCaptain `shouldReturn` 6

    it "totals the willpower of all attackers" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      beravor <- heroOf pid "01012"
      getAttackStrength [eowyn, beravor] eid Enemies.shadowHostCaptain `shouldReturn` 6

    it "returns to attack when Spectral is lost" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource (toTarget eid) [LoseKeyword Spectral] UntilEndOfRound
      getAttackStrength [eowyn] eid Enemies.shadowHostCaptain `shouldReturn` 1

    it "removes printed Spectral when the enemy is blank" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ CreateEffect $ Effect GameSource (toTarget eid) [Blank] UntilEndOfRound
      getAttackStrength [eowyn] eid Enemies.shadowHostCaptain `shouldReturn` 1

    it "still subtracts enemy defense in a resolved attack" . gameTest $ \pid -> do
      eid <- stageCaptain
      eowyn <- heroOf pid "01007"
      run $ AttackWithAgainst eowyn eid
      run $ ResolveAttack pid eid
      passActions pid
      getDamage (toTarget eid) `shouldReturn` 3

    it "does not alter direct damage" . gameTest $ \_ -> do
      eid <- stageCaptain
      run $ DealDamage GameSource (toTarget eid) 2
      getDamage (toTarget eid) `shouldReturn` 2

  it "updates Shadow Host Captain's stats as Night changes" . gameTest $ \_ -> do
    _ <- prepareNights
    eid <- stageCaptain
    getEnemyAttack eid `shouldReturn` 4
    getEnemyDefense eid `shouldReturn` 1
    replicateM_ 4 endRoundWindow
    getEnemyAttack eid `shouldReturn` 5
    getEnemyDefense eid `shouldReturn` 2
    replicateM_ 4 endRoundWindow
    getEnemyAttack eid `shouldReturn` 6
    getEnemyDefense eid `shouldReturn` 3

stageErechEnemy :: CardDef -> GameT EnemyId
stageErechEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "enemy missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

stageErechLocation :: CardDef -> GameT LocationId
stageErechLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame $ toId . fromJustNote "location missing" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations)

heroExhaustedTarget :: Target -> GameT Bool
heroExhaustedTarget (HeroTarget hid) = heroExhausted . toAttrs <$> getHero hid
heroExhaustedTarget _ = error "expected a hero"
