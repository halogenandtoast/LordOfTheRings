module LordOfTheRings.Standalone.PrancingPonyEncountersSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getStagingThreat)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Scenario (murderAtThePrancingPony)
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers (Meta (..))
import LordOfTheRings.Standalone.PrancingPonyInvestigationSpec hiding (spec)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, strongDefender, defendAttack, passAttackActions)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

ponyTest :: (PlayerId -> GameT ()) -> IO ()
ponyTest body = gameTestIn murderAtThePrancingPony testDeck $ \pid -> run (DoStep 2 Setup) >> body pid

ponyTwo :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
ponyTwo body = twoPlayerTestIn murderAtThePrancingPony testDeck $ \one two -> run (DoStep 2 Setup) >> body one two

active :: CardDef -> GameT LocationId
active def = do
  card <- genEncounterCard def
  run $ PutIntoPlayAsActiveLocation card
  getsGame (toId . fromJustNote "missing active location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations))

will :: PlayerId -> GameT Card
will pid = do
  card <- putCardIntoHand pid Events.aTestOfWill
  payer <- heroOf pid "01007"
  run $ PlaceTokens GameSource payer Resource 1
  pure card

cardChoices :: PlayerId -> GameT [Target]
cardChoices pid = maybe [] (mapMaybe (\case TargetLabel target _ -> Just target; _ -> Nothing) . questionChoices) <$> getQuestion pid

spec :: Spec
spec = describe "Prancing Pony remaining encounters" do
  describe "Bill Ferny" do
    it "cannot be optionally engaged" . ponyTest $ \pid -> do
      _ <- stageEnemy Enemies.billFernyMurder
      run $ OptionalEngagement pid
      getQuestion pid `shouldReturn` Nothing

    it "gives only the first treachery doomed 1 and prevents its cancellation" . ponyTest $ \pid -> do
      _ <- stageEnemy Enemies.billFernyMurder
      cancellation <- will pid
      _ <- setEncounterDeck [Treacheries.deadlyAccusations, Treacheries.deadlyAccusations]
      run RevealEncounterCard
      cardChoices pid >>= (`shouldSatisfy` notElem (CardIdTarget cancellation.id))
      resolveRevealed
      getThreat pid `shouldReturn` 29
      run RevealEncounterCard
      cardChoices pid >>= (`shouldSatisfy` elem (CardIdTarget cancellation.id))
      chooseTarget pid (CardIdTarget cancellation.id)
      resolveRevealed
      getThreat pid `shouldReturn` 29

    it "does not keep later treacheries uncancelable when the first one stays attached" . ponyTest $ \pid -> do
      _ <- stageEnemy Enemies.billFernyMurder
      _ <- stageEnemy Enemies.lawlessRuffian
      cancellation <- will pid
      _ <- setEncounterDeck [Treacheries.hiredMuscle, Treacheries.deadlyAccusations]
      run RevealEncounterCard
      resolveRevealed
      cardChoices pid >>= (`shouldSatisfy` elem (CardIdTarget cancellation.id))
      chooseTarget pid (CardIdTarget cancellation.id)
      resolveRevealed
      getThreat pid `shouldReturn` 28

    it "counts a treachery revealed before Bill entered play" . ponyTest $ \pid -> do
      _ <- setEncounterDeck [Treacheries.deadlyAccusations, Treacheries.deadlyAccusations]
      run RevealEncounterCard
      resolveRevealed
      _ <- stageEnemy Enemies.billFernyMurder
      run RevealEncounterCard
      resolveRevealed
      getThreat pid `shouldReturn` 29

    it "starts a new first-treachery count each round and stops granting it while blank" . ponyTest $ \pid -> do
      eid <- stageEnemy Enemies.billFernyMurder
      _ <- setEncounterDeck $ replicate 3 Treacheries.deadlyAccusations
      run RevealEncounterCard
      resolveRevealed
      run $ EndEffects UntilEndOfRound
      run RevealEncounterCard
      resolveRevealed
      getThreat pid `shouldReturn` 31
      run $ EndEffects UntilEndOfRound
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      run RevealEncounterCard
      resolveRevealed
      getThreat pid `shouldReturn` 32

    for_ [False, True] $ \undefended ->
      it ("resolves its shadow choice without applying the unchosen effect, undefended: " <> show undefended) . ponyTest $ \pid -> do
        eid <- stageEnemy Enemies.lawlessRuffian
        run $ EngageEnemy pid eid
        defender <- strongDefender pid
        _ <- setEncounterDeck [Enemies.billFernyMurder]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        chooseLabel (if undefended then "$label.prancingPony.undefended" else "$label.prancingPony.raiseThreat4") pid
        passAttackActions
        when undefended $ chooseTarget pid defender
        passAttackActions
        getDamage defender `shouldReturn` if undefended then 3 else 0
        getThreat pid `shouldReturn` if undefended then 27 else 31

  describe "Bree-land Informant" do
    it "lets only the attacked player pay to look at the bottom card, then discards the Informant" . ponyTwo $ \one two -> do
      initialize
      clues <- safeClues
      setClues clues
      eid <- stageEnemy Enemies.breeLandInformant
      run $ EngageEnemy two eid
      for_ [one, two] $ \pid -> heroOf pid "01007" >>= \hero -> run $ PlaceTokens GameSource hero Resource 1
      defender <- strongDefender two
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack two eid
      defendAttack two defender
      getQuestion one `shouldReturn` Nothing
      offersAbilityOf two (EnemyTarget eid) `shouldReturn` True
      chooseAbilityOf two (EnemyTarget eid)
      map (.id) <$> shown two `shouldReturn` map (.id) (take 1 $ reverse clues)
      getThreat two `shouldReturn` 28
      acceptLook two
      getsGame (null . (.entities.enemies)) `shouldReturn` True
      getThreat one `shouldReturn` 27

    for_ [(False, False), (True, False), (True, True)] $ \(resource, emptyDeck) ->
      it ("requires a blank shadow, an affordable cost and a card to look at: " <> show (resource, emptyDeck)) . ponyTest $ \pid -> do
        initialize
        if emptyDeck then setClues [] else setClues =<< safeClues
        eid <- stageEnemy Enemies.breeLandInformant
        run $ EngageEnemy pid eid
        when resource $ heroOf pid "01007" >>= \hero -> run $ PlaceTokens GameSource hero Resource 1
        defender <- strongDefender pid
        _ <- setEncounterDeck [if resource && not emptyDeck then Enemies.greenwayFootpad else Locations.oldForestRoad]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        offersAbilityOf pid (EnemyTarget eid) `shouldReturn` False

  describe "Hired Muscle" do
    it "attaches to an eligible Brigand, moves it to staging, boosts its stats, and preserves surge" . ponyTest $ \pid -> do
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Treacheries.hiredMuscle, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      getEnemyThreat eid `shouldReturn` 3
      getEnemyAttack eid `shouldReturn` 4
      getEnemyDefense eid `shouldReturn` 3
      getsGame (length . (.entities.treacheries)) `shouldReturn` 1
      getEncounterDeck `shouldReturn` []

    it "does not attach to a Suspect or add a second copy to the same enemy" . ponyTest $ \_ -> do
      _ <- stageEnemy Enemies.toddTheTroll
      _ <- stageEnemy Enemies.lawlessRuffian
      _ <- setEncounterDeck [Treacheries.hiredMuscle, Treacheries.hiredMuscle, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      getsGame (length . (.entities.treacheries)) `shouldReturn` 1
      getsGame (map (.placement) . toList . (.entities.treacheries)) >>= (`shouldSatisfy` all (\case AttachedTo (EnemyTarget _) -> True; _ -> False))

    it "looks at the bottom card when the attached enemy is defeated" . ponyTest $ \pid -> do
      initialize
      clues <- safeClues
      setClues clues
      eid <- stageEnemy Enemies.lawlessRuffian
      _ <- setEncounterDeck [Treacheries.hiredMuscle, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      run $ Destroy GameSource $ EnemyTarget eid
      map (.id) <$> shown pid `shouldReturn` map (.id) (take 1 $ reverse clues)
      acceptLook pid
      getsGame (null . (.entities.enemies)) `shouldReturn` True

    it "keeps its attachment bonuses but suppresses the gained Forced ability on a blank host" . ponyTest $ \pid -> do
      initialize
      setClues =<< safeClues
      eid <- stageEnemy Enemies.lawlessRuffian
      _ <- setEncounterDeck [Treacheries.hiredMuscle, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      getEnemyAttack eid `shouldReturn` 4
      run $ Destroy GameSource $ EnemyTarget eid
      getQuestion pid `shouldReturn` Nothing
      getsGame (null . (.entities.enemies)) `shouldReturn` True

  describe "Smoke and Mirrors / Deadly Accusations" do
    for_ [False, True] $ \hasActive ->
      it ("uses the active Investigate value for threat, including no active location: " <> show hasActive) . ponyTwo $ \one two -> do
        when hasActive $ void $ active Locations.darkAlleyMurder
        _ <- setEncounterDeck [Treacheries.deadlyAccusations]
        run RevealEncounterCard
        resolveRevealed
        getThreat one `shouldReturn` if hasActive then 31 else 28
        getThreat two `shouldReturn` if hasActive then 31 else 28

    it "adds the printed threat of all discarded cards for the phase, then expires" . ponyTest $ \_ -> do
      _ <- active Locations.ramshackleManor
      _ <- setEncounterDeck [Treacheries.smokeAndMirrors, Enemies.greenwayFootpad, Locations.marketSquareMurder, Treacheries.risingBodyCount, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      getStagingThreat `shouldReturn` 4
      length <$> getEncounterDeck `shouldReturn` 1
      run $ EndEffects UntilEndOfPhase
      getStagingThreat `shouldReturn` 0

    it "discards only an attachment controlled by the defending player for its shadow" . ponyTwo $ \one two -> do
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy one eid
      defender <- strongDefender one
      other <- heroOf two "01012"
      _ <- putAttachmentOn one Attachments.citadelPlate defender
      _ <- putAttachmentOn two Attachments.citadelPlate other
      _ <- setEncounterDeck [Treacheries.smokeAndMirrors]
      run $ DealShadowCard eid
      run $ EnemyAttack one eid
      defendAttack one defender
      getsGame (length . (.entities.attachments)) `shouldReturn` 1

    for_ [False, True] $ \hasActive ->
      it ("adds the active Investigate value to only the current shadow attack: " <> show hasActive) . ponyTest $ \pid -> do
        when hasActive $ void $ active Locations.darkAlleyMurder
        eid <- stageEnemy Enemies.lawlessRuffian
        run $ EngageEnemy pid eid
        defender <- heroOf pid "01012"
        _ <- putAttachmentOn pid Attachments.citadelPlate defender
        _ <- setEncounterDeck [Treacheries.deadlyAccusations]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        getDamage defender `shouldReturn` if hasActive then 4 else 1
        getEnemyAttack eid `shouldReturn` 3

  describe "Rising Body Count" do
    for_ [False, True] $ \shuffleCard ->
      it ("resolves exactly one revelation option, shuffle: " <> show shuffleCard) . ponyTwo $ \one two -> do
        initialize
        initial <- length . (.investigationDeck) <$> meta
        body : _ <- setEncounterDeck [Treacheries.risingBodyCount, Locations.oldForestRoad]
        run RevealEncounterCard
        resolveRevealed
        chooseLabel (if shuffleCard then "$label.prancingPony.shuffleBodyCount" else "$label.prancingPony.raiseEachThreat2") one
        getThreat one `shouldReturn` if shuffleCard then 27 else 29
        getThreat two `shouldReturn` if shuffleCard then 27 else 29
        length . (.investigationDeck) <$> meta `shouldReturn` initial + if shuffleCard then 1 else 0
        getsGame (null . (.entities.treacheries)) `shouldReturn` True
        discarded <- getEncounterDiscard
        any ((== body.id) . (.id)) discarded `shouldBe` not shuffleCard

    for_ [False, True] $ \shuffleCard ->
      it ("resolves its shadow only after a destroyed character, preserving physical identity: " <> show shuffleCard) . ponyTest $ \pid -> do
        initialize
        initial <- length . (.investigationDeck) <$> meta
        eid <- stageEnemy Enemies.lawlessRuffian
        run $ EngageEnemy pid eid
        defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
        body : _ <- setEncounterDeck [Treacheries.risingBodyCount, Locations.oldForestRoad]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        chooseLabel (if shuffleCard then "$label.prancingPony.shuffleBodyCount" else "$label.prancingPony.raiseThreat2") pid
        passAttackActions
        getThreat pid `shouldReturn` if shuffleCard then 27 else 29
        clues <- (.investigationDeck) <$> meta
        length clues `shouldBe` initial + if shuffleCard then 1 else 0
        any ((== body.id) . (.id)) clues `shouldBe` shuffleCard
        run $ DiscardShadowCards eid
        discarded <- getEncounterDiscard
        any ((== body.id) . (.id)) discarded `shouldBe` not shuffleCard

    it "does nothing after a surviving defender and does not reveal itself when looked at in the Investigation deck" . ponyTest $ \pid -> do
      initialize
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy pid eid
      defender <- strongDefender pid
      [body] <- setEncounterDeck [Treacheries.risingBodyCount]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      getQuestion pid `shouldReturn` Nothing
      getThreat pid `shouldReturn` 27
      run $ RemoveShadowCard eid body.id
      run $ ShuffleIntoInvestigationDeck body
      setClues [body]
      run $ LookAtInvestigationDeck pid GameSource 1
      acceptLook pid
      getQuestion pid `shouldReturn` Nothing
      getThreat pid `shouldReturn` 27
      (.investigationDeck) <$> meta `shouldReturn` [body]
