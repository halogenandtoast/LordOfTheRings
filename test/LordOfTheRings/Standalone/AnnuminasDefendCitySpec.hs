module LordOfTheRings.Standalone.AnnuminasDefendCitySpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Standalone.AnnuminasCitySpec (setReplica)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, strongDefender, defendAttack)
import LordOfTheRings.Window (WindowType (AllPlayersCommittedToQuest, RoundEnds), mkAfter, mkWhen)
import TestImport

city :: GameT Target
city = do
  target <- putQuestStage Quests.defendTheCityAnnuminas
  run $ Flip target
  resolveRevealed
  pure target

commitFinished :: Message
commitFinished = CheckWindows [mkAfter AllPlayersCommittedToQuest]

spec :: Spec
spec = describe "Annúminas Defend the City" do
  it "reveals a card when there is no staged enemy, including when an enemy is engaged" . gameTest $ \pid -> do
    _ <- city
    eid <- stageEnemy Enemies.wargOfCarnDum
    run $ EngageEnemy pid eid
    -- resolve the Warg's engagement choice before triggering the quest.
    chooseOptionMatching "pay Warg engagement" pid (const True)
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run commitFinished
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    length <$> getEncounterDeck `shouldReturn` 1

  it "uses modified attack to select its enemy, deals a fresh shadow, and keeps the attacker in staging" . gameTest $ \pid -> do
    _ <- city
    warg <- stageEnemy Enemies.wargOfCarnDum
    _ <- stageEnemy Enemies.orcOfAngmar
    run $ CreateEffect $ Effect GameSource (EnemyTarget warg) [AttackModifier 5] UntilEndOfRound
    defender <- strongDefender pid
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    run commitFinished
    defendAttack pid defender
    (.placement) <$> getEnemy warg `shouldReturn` StagingArea
    (.shadowCards) <$> getEnemy warg `shouldReturn` []
    length <$> getEncounterDeck `shouldReturn` 2
    getsGame (length . (.entities.locations)) `shouldReturn` 0

  it "lets the first player break highest-attack ties and persists the enemy choice through reload" . gameTest $ \pid -> do
    _ <- city
    chosen <- stageEnemy Enemies.wargOfCarnDum
    _ <- stageEnemy Enemies.wargOfCarnDum
    defender <- strongDefender pid
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    run commitFinished
    length . maybe [] questionChoices <$> getQuestion pid `shouldReturn` 2
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    chooseTarget pid chosen
    defendAttack pid defender
    length <$> getEncounterDeck `shouldReturn` 2

  it "reveals instead if the highest-attack enemy cannot attack, without choosing a weaker enemy" . gameTest $ \_ -> do
    _ <- city
    eid <- stageEnemy Enemies.orcOfAngmar
    _ <- stageEnemy Enemies.wargOfCarnDum
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
    _ <- setEncounterDeck $ replicate 2 Locations.oldForestRoad
    run commitFinished
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    length <$> getEncounterDeck `shouldReturn` 1

  it "attacks each player in current turn order, with one shadow per attack" . twoPlayerTest testDeck $ \one two -> do
    _ <- city
    run $ SetFirstPlayer two
    eid <- stageEnemy Enemies.wargOfCarnDum
    former <- strongDefender two
    latter <- strongDefender one
    _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
    run commitFinished
    defendAttack two former
    defendAttack one latter
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
    length <$> getEncounterDeck `shouldReturn` 2
    getsGame (length . (.entities.locations)) `shouldReturn` 0

  it "skips a player the enemy cannot attack but does not reveal if it attacks another" . twoPlayerTest testDeck $ \one two -> do
    _ <- city
    eid <- stageEnemy Enemies.wargOfCarnDum
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttackPlayer one] UntilEndOfRound
    defender <- strongDefender two
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    run commitFinished
    defendAttack two defender
    length <$> getEncounterDeck `shouldReturn` 2
    getsGame (length . (.entities.locations)) `shouldReturn` 0

  it "does not attack before all players have finished their commitment" . twoPlayerTest testDeck $ \one two -> do
    _ <- city
    _ <- stageEnemy Enemies.wargOfCarnDum
    current <- getGame
    putGame $ current & phaseL .~ QuestPhase
    _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
    run $ BeginPhase QuestPhase
    chooseLabel "$label.doneCommitting" one
    length <$> getEncounterDeck `shouldReturn` 4
    chooseLabel "$label.doneCommitting" two
    -- The attack has now dealt its shadow, before the normal staging step reveals.
    length <$> getEncounterDeck `shouldReturn` 3
    getsGame (length . (.entities.locations)) `shouldReturn` 0

  it "uses the common arrival and Host victory-display reduction without the Gate's extra reveal" . gameTest $ \_ -> do
    _ <- setEncounterDeck $ replicate 3 Enemies.wargOfCarnDum
    target <- city
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    length <$> getEncounterDeck `shouldReturn` 2
    eid <- stageEnemy Enemies.hostOfAngmar
    run $ AddToVictoryDisplay $ EnemyTarget eid
    (getQuestPoints . toAttrs =<< getActiveQuest) `shouldReturn` 40
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))
    run $ CreateEffect $ Effect GameSource target [Blank] UntilEndOfRound
    (getQuestPoints . toAttrs =<< getActiveQuest) `shouldReturn` 50

  it "heals one damage on each player's chosen hero or ally, only at the selected stage" . twoPlayerTest testDeck $ \one two -> do
    target <- city
    own <- heroOf one "01012"
    ally <- putAllyIntoPlay two Allies.guardOfTheCitadel
    run $ PlaceTokens GameSource own Damage 2
    run $ PlaceTokens GameSource ally Damage 1
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf one target
    chooseLabel "$label.epic.thisGroup" one
    -- Each player has just one legal damaged character and resolves automatically.
    getDamage own `shouldReturn` 1
    getDamage ally `shouldReturn` 0

  it "sends a remote heal once and never heals a healthy or unhealable character" . gameTest $ \pid -> do
    setReplica []
    own <- heroOf pid "01012"
    blocked <- heroOf pid "01007"
    run $ PlaceTokens GameSource own Damage 2
    run $ PlaceTokens GameSource blocked Damage 1
    run $ CreateEffect $ Effect GameSource blocked [CannotBeHealed] UntilEndOfRound
    let command = EpicCommand UUID.nil (GroupOrdinal 1) (GroupOrdinal 0) (toCardCode Quests.defendTheCityAnnuminas) EpicHealControlledCharacter ScenarioTarget
    run $ ApplyEpicCommand command
    getDamage own `shouldReturn` 1
    getDamage blocked `shouldReturn` 1
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    run $ ApplyEpicCommand command
    getDamage own `shouldReturn` 1

  it "queues healing for a selected remote stage without healing local characters" . gameTest $ \pid -> do
    let stage = EpicStage 0 [EpicQuest (toCardCode Quests.guardTheGateAnnuminas) 2 0 (Just 50) True] [] True
    setReplica [EpicBoard (GroupOrdinal 1) "Other" [] False (Just stage)]
    target <- city
    own <- heroOf pid "01012"
    run $ PlaceTokens GameSource own Damage 2
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    chooseOptionMatching "choose Other" pid $ \case Label label _ -> "Other" `isInfixOf` label; _ -> False
    getDamage own `shouldReturn` 2
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.epicCommandEffect) replica.epicPendingCommands `shouldBe` [EpicHealControlledCharacter]

  it "does not offer healing when no stage can benefit, including damage blocked by healing limits" . gameTest $ \pid -> do
    target <- city
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False
    hero <- heroOf pid "01012"
    run $ PlaceTokens GameSource hero Damage 1
    run $ CreateEffect $ Effect GameSource hero [HealLimit 0] UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False

  it "does not reveal a fallback card if the attacker is destroyed during its first attack" . twoPlayerTest testDeck $ \one two -> do
    _ <- city
    eid <- stageEnemy Enemies.wargOfCarnDum
    defender <- putAllyIntoPlay one Allies.horsebackArcher
    spear <- putAttachmentOn one Attachments.spearOfTheCitadel defender
    run $ PlaceTokens GameSource (EnemyTarget eid) Damage 1
    _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
    run commitFinished
    defendAttack one defender
    chooseAbilityOf one spear
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    length <$> getEncounterDeck `shouldReturn` 3
    getQuestion two `shouldReturn` Nothing
