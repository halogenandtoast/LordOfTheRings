module LordOfTheRings.Standalone.AnnuminasTreacheriesSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getMainQuestProgress, getThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Standalone.AnnuminasInvadersSpec (mainStage)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, strongDefender, defendAttack, passAttackActions)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

annuminasTest :: (PlayerId -> GameT ()) -> IO ()
annuminasTest body = gameTest $ \pid -> run (DoStep 2 Setup) >> body pid

annuminasTwo :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
annuminasTwo body = twoPlayerTest testDeck $ \one two -> run (DoStep 2 Setup) >> body one two

reveal :: CardDef -> GameT ()
reveal def = do
  _ <- setEncounterDeck [def, Locations.oldForestRoad]
  run RevealEncounterCard
  resolveRevealed

spec :: Spec
spec = describe "Annúminas treacheries" do
  describe "The Hordes of Angmar" do
    it "has the attacked player reveal the encounter required by its shadow" . annuminasTwo $ \one two -> do
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy two eid
      defender <- putAllyIntoPlay two Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Treacheries.theHordesOfAngmar, Treacheries.bitterReek, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack two eid
      defendAttack two defender
      resolveRevealed
      getQuestion one `shouldReturn` Nothing
      getQuestion two >>= (`shouldSatisfy` isJust)

    it "returns the highest modified-threat enemy engaged with the choosing player" . annuminasTest $ \pid -> do
      low <- stageEnemy Enemies.lawlessRuffian
      high <- stageEnemy Enemies.chetwoodOutlaw
      run $ EngageEnemy pid low
      run $ EngageEnemy pid high
      _ <- setEncounterDeck [Treacheries.theHordesOfAngmar, Enemies.forestSpider, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.annuminas.returnHighest" pid
      (.placement) . toAttrs <$> getEnemy high `shouldReturn` StagingArea
      (.placement) . toAttrs <$> getEnemy low `shouldReturn` EngagedWith pid

    it "rechecks the encounter supply after the first player's search and engages without revealing" . annuminasTwo $ \one two -> do
      _ <- mainStage 0
      _ : found : _ <- setEncounterDeck [Treacheries.theHordesOfAngmar, Enemies.wargOfCarnDum, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      chooseTarget one (CardIdTarget found.id)
      hero <- heroOf one "01012"
      chooseTarget one hero
      getQuestion two `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 1
      getsGame (length . (.entities.enemies)) `shouldReturn` 1

    for_ [False, True] $ \destroyed ->
      it ("reveals its shadow's encounter only if the attack destroys a character: " <> show destroyed) . annuminasTest $ \pid -> do
        eid <- stageEnemy Enemies.lawlessRuffian
        run $ EngageEnemy pid eid
        defender <- if destroyed then putAllyIntoPlay pid Allies.guardOfTheCitadel else strongDefender pid
        _ <- setEncounterDeck [Treacheries.theHordesOfAngmar, Locations.oldForestRoad, Locations.oldForestRoad]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        resolveRevealed
        length <$> getEncounterDeck `shouldReturn` if destroyed then 1 else 2

  describe "Sorcery of Carn Dûm" do
    for_ [False, True] $ \discardHighest ->
      it ("uses the attachment count for damage or discards only a highest-cost attachment: " <> show discardHighest) . annuminasTest $ \pid -> do
        hero <- heroOf pid "01012"
        _ <- putAttachmentOn pid Attachments.citadelPlate hero
        _ <- putAttachmentOn pid Attachments.hornOfGondor hero
        reveal Treacheries.sorceryOfCarnDum
        chooseLabel (if discardHighest then "$label.annuminas.discardHighest" else "$label.annuminas.assignDamage count=i:2") pid
        unless discardHighest $ replicateM_ 2 $ chooseTarget pid hero
        getsGame (sort . map toCardCode . toList . (.entities.attachments)) `shouldReturn` sort (if discardHighest then [toCardCode Attachments.hornOfGondor] else [toCardCode Attachments.citadelPlate, toCardCode Attachments.hornOfGondor])
        getDamage hero `shouldReturn` if discardHighest then 0 else 2
        getThreat pid `shouldReturn` 28

    it "does not offer an attachment that cannot leave play" . annuminasTest $ \pid -> do
      hero <- heroOf pid "01012"
      target <- putAttachmentOn pid Attachments.citadelPlate hero
      run $ CreateEffect $ Effect GameSource target [CannotLeavePlay] UntilEndOfRound
      reveal Treacheries.sorceryOfCarnDum
      -- Damage is the sole legal option, so it goes directly to assignment.
      chooseTarget pid hero
      getDamage hero `shouldReturn` 1
      getsGame (length . (.entities.attachments)) `shouldReturn` 1

    it "resolves only doomed when there are no attachments" . annuminasTest $ \pid -> do
      reveal Treacheries.sorceryOfCarnDum
      getThreat pid `shouldReturn` 28
      getQuestion pid `shouldReturn` Nothing

    it "discards only the defending player's attachment for its shadow" . annuminasTwo $ \one two -> do
      eid <- stageEnemy Enemies.lawlessRuffian
      run $ EngageEnemy one eid
      defender <- strongDefender one
      other <- heroOf two "01012"
      _ <- putAttachmentOn one Attachments.citadelPlate defender
      _ <- putAttachmentOn two Attachments.citadelPlate other
      _ <- setEncounterDeck [Treacheries.sorceryOfCarnDum, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack one eid
      defendAttack one defender
      getsGame (length . (.entities.attachments)) `shouldReturn` 1

  describe "Caught in the Press" do
    it "applies each player's choice only to that player's committed characters" . annuminasTwo $ \one two -> do
      _ <- mainStage 0
      firstHero <- heroOf one "01007"
      secondHero <- heroOf two "01007"
      idle <- heroOf one "01012"
      run $ CommitToQuest firstHero
      run $ CommitToQuest secondHero
      reveal Treacheries.caughtInThePress
      chooseLabel "$label.annuminas.damageQuesting" one
      chooseLabel "$label.annuminas.reduceWillpower" two
      getDamage firstHero `shouldReturn` 1
      getDamage idle `shouldReturn` 0
      getWillpower firstHero `shouldReturn` 4
      getWillpower secondHero `shouldReturn` 3
      run $ EndEffects UntilEndOfPhase
      getWillpower secondHero `shouldReturn` 4

    for_ [False, True] $ \removeProgress ->
      it ("resolves only the chosen shadow option and defers returning the attacker: " <> show removeProgress) . annuminasTest $ \pid -> do
        _ <- mainStage 3
        eid <- stageEnemy Enemies.lawlessRuffian
        run $ EngageEnemy pid eid
        defender <- strongDefender pid
        _ <- setEncounterDeck [Treacheries.caughtInThePress, Locations.oldForestRoad]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid defender
        chooseLabel (if removeProgress then "$label.annuminas.removeProgress count=i:2" else "$label.annuminas.returnAfterAttack") pid
        passAttackActions
        getMainQuestProgress `shouldReturn` if removeProgress then 1 else 3
        (.placement) . toAttrs <$> getEnemy eid `shouldReturn` if removeProgress then EngagedWith pid else StagingArea

  describe "Weary Warriors" do
    it "damages and uncommits the chosen hero, then continuously blanks damaged characters until round end" . annuminasTest $ \pid -> do
      _ <- mainStage 0
      hero <- heroOf pid "01007"
      other <- heroOf pid "01012"
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run $ DealDamage GameSource ally 1
      run $ CommitToQuest hero
      reveal Treacheries.wearyWarriors
      chooseTarget pid hero
      getDamage hero `shouldReturn` 1
      getModifiers hero >>= (`shouldSatisfy` elem Blank)
      getModifiers ally >>= (`shouldSatisfy` elem Blank)
      getModifiers other >>= (`shouldSatisfy` notElem Blank)
      getsGame (map heroCommitted . map toAttrs . toList . (.entities.heroes)) >>= (`shouldSatisfy` all not)
      run $ HealDamage GameSource hero 1
      getModifiers hero >>= (`shouldSatisfy` notElem Blank)
      run $ DealDamage GameSource other 1
      getModifiers other >>= (`shouldSatisfy` elem Blank)
      run $ EndEffects UntilEndOfRound
      getModifiers ally >>= (`shouldSatisfy` notElem Blank)

    for_ [False, True] $ \alreadyDamaged ->
      it ("checks damage on the defender when resolving its shadow: " <> show alreadyDamaged) . annuminasTest $ \pid -> do
        eid <- stageEnemy Enemies.lawlessRuffian
        run $ EngageEnemy pid eid
        hero <- heroOf pid "01012"
        _ <- putAttachmentOn pid Attachments.citadelPlate hero
        when alreadyDamaged $ run $ DealDamage GameSource hero 1
        _ <- setEncounterDeck [Treacheries.wearyWarriors, Locations.oldForestRoad]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid hero
        getDamage hero `shouldReturn` if alreadyDamaged then 4 else 2
        getEnemyAttack eid `shouldReturn` 3
