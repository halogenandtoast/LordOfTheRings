module LordOfTheRings.Standalone.StoneOfErechSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Helpers.Character (getAttackStrength)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat)
import LordOfTheRings.Keyword (Keyword (Spectral))
import LordOfTheRings.Matcher (getObjective, getThreat, getEnemy, CardMatcher (CardWithType))
import LordOfTheRings.Modifier (ModifierType (AttackModifier, WillpowerModifier, AttackBonusAgainst, LoseKeyword, Blank, CannotPlay, RevealedEncounterCardsGainDoomed))
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
