module LordOfTheRings.Hobbit.WeMustAwayEreBreakOfDaySpec (spec) where

import LordOfTheRings.Campaign (campaignLogPool, newCampaignLog)
import LordOfTheRings.Card (CardDef (cdEncounterSet), HasCardCode, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, weMustAwayEreBreakOfDay)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), sackDeckL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkWhen)
import TestImport

wmTest :: (PlayerId -> GameT ()) -> IO ()
wmTest = gameTestIn weMustAwayEreBreakOfDay testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

codes :: HasCardCode c => [c] -> [CardCode]
codes = sort . map toCardCode

-- | The first player takes control of Bilbo Baggins, as the setup's third step does.
withBilbo :: PlayerId -> GameT Target
withBilbo pid = do
  run $ DoStep 3 Setup
  hero <- getHeroWithCode "131001"
  hero.owner `shouldBe` pid
  pure $ toTarget hero

setSackDeck :: [CardDef] -> GameT ()
setSackDeck defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (sackDeckL .~ cards)) =<< getGame

sackDeckCodes :: GameT [CardCode]
sackDeckCodes = codes . scenarioSackDeck <$> scenarioAttrs

-- | The Sack objectives in play, with where they are.
sacksInPlay :: GameT [(CardCode, Placement)]
sacksInPlay = getsGame \g -> [(toCardCode o, o.placement) | o <- toList g.entities.objectives, toCardCode o `elem` sackCodes]
 where
  sackCodes = map toCardCode [Objectives.aSmellySack, Objectives.aStrongSack, Objectives.aToughSack, Objectives.aLargeSack, Objectives.aDeepSack, Objectives.aFoulSack, Objectives.aWornSack]

objectiveOf :: CardDef -> GameT ObjectiveAttrs
objectiveOf def = getsGame (toAttrs . fromJustNote "no such objective" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

-- | Stage 2 in play, flipped and its When Revealed resolved, the Trolls and Troll Cave
-- set aside beforehand as stage 1 sets them.
atRoastMutton :: PlayerId -> GameT ()
atRoastMutton pid = do
  aside <- traverse genEncounterCard [Enemies.william, Enemies.bert, Enemies.tom, Locations.trollCave]
  putGame . (scenarioL %~ overAttrs (\a -> a {scenarioSetAside = aside, scenarioQuestDeck = []})) =<< getGame
  dawn <- genEncounterCard Quests.dawnTakeYouAll
  putGame . (scenarioL %~ overAttrs (\a -> a {scenarioQuestDeck = [dawn]})) =<< getGame
  _ <- setEncounterDeck [Locations.hobbitLands]
  card <- genEncounterCard Quests.roastMutton
  run $ AdvanceToQuest card
  resolveWhenRevealed pid

spec :: Spec
spec = describe "We Must Away, Ere Break of Day" do
  it "is a scenario the game can be dealt" . wmTest $ \_ ->
    isJust (lookupScenario "we-must-away-ere-break-of-day") `shouldBe` True

  describe "setup" do
    it "builds the sack deck, sets the Trolls and Troll Cave aside, and gives the first player Bilbo Baggins" . wmTest $ \pid -> do
      flipFirstQuest pid
      attrs <- scenarioAttrs
      length attrs.sackDeck `shouldBe` 7
      codes (scenarioSetAside attrs) `shouldBe` codes [Enemies.william, Enemies.bert, Enemies.tom, Locations.trollCave]
      let inDeck = attrs.encounterDeck <> attrs.encounterDiscard
      all ((`elem` map Just [WeMustAwayEreBreakOfDay, WesternLands]) . cdEncounterSet . toCardDef) inDeck `shouldBe` True
      any ((`elem` codes [Objectives.aStrongSack, Objectives.aWornSack]) . toCardCode) inDeck `shouldBe` False
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.roastMutton, Quests.dawnTakeYouAll]
      activeQuestCode `shouldReturn` toCardCode Quests.anUnexpectedParty
      bilbo <- getHeroWithCode "131001"
      bilbo.owner `shouldBe` pid

  describe "the sack deck" do
    it "attaches A Deep Sack to the character with the highest printed willpower, who cannot ready, attack, defend, quest or trigger effects" . wmTest $ \pid -> do
      _ <- withBilbo pid
      setSackDeck [Objectives.aDeepSack, Objectives.aStrongSack]
      run $ DrawFromSackDeck 1
      eowyn <- toTarget <$> getHeroWithCode "01007"
      sacksInPlay `shouldReturn` [(toCardCode Objectives.aDeepSack, AttachedTo eowyn)]
      sackDeckCodes `shouldReturn` [toCardCode Objectives.aStrongSack]
      getModifiers eowyn >>= (`shouldSatisfy` \ms -> all (`elem` ms) [CannotReady, CannotAttackOrDefend, CannotCommitToQuest, CannotTriggerAbilities])

    it "lets the first player choose among the tied, and skips a character with a Sack" . wmTest $ \pid -> do
      _ <- withBilbo pid
      setSackDeck [Objectives.aStrongSack, Objectives.aLargeSack]
      run $ DrawFromSackDeck 2
      beravor <- toTarget <$> getHeroWithCode "01012"
      theodred <- toTarget <$> getHeroWithCode "01002"
      -- Beravor and Théodred share the highest printed attack, 2
      chooseTarget pid beravor
      -- both have 4 hit points, but Beravor is in a sack already
      sort <$> sacksInPlay `shouldReturn` sort [(toCardCode Objectives.aStrongSack, AttachedTo beravor), (toCardCode Objectives.aLargeSack, AttachedTo theodred)]

    it "shuffles a Sack card with no eligible target back into the sack deck, and ignores an empty deck" . wmTest $ \pid -> do
      _ <- withBilbo pid
      setSackDeck [Objectives.aSmellySack]
      run $ DrawFromSackDeck 1
      sacksInPlay `shouldReturn` []
      sackDeckCodes `shouldReturn` [toCardCode Objectives.aSmellySack]
      setSackDeck []
      run $ DrawFromSackDeck 2
      sacksInPlay `shouldReturn` []

    it "shuffles a Sack card back into the sack deck as it leaves play" . wmTest $ \pid -> do
      _ <- withBilbo pid
      setSackDeck [Objectives.aDeepSack]
      run $ DrawFromSackDeck 1
      o <- objectiveOf Objectives.aDeepSack
      run $ Discard (ObjectiveTarget o.objectiveId)
      sacksInPlay `shouldReturn` []
      sackDeckCodes `shouldReturn` [toCardCode Objectives.aDeepSack]

    it "lets Bilbo Baggins, at Troll Camp, exhaust and spend 1 Baggins resource to remove a Sack" . wmTest $ \pid -> do
      bilbo <- withBilbo pid
      setSackDeck [Objectives.aDeepSack]
      run $ DrawFromSackDeck 1
      camp <- stageLocation Locations.trollCamp
      run $ PlaceTokens GameSource bilbo Resource 1
      run $ ActionWindow pid
      chooseAbilityOf pid (LocationTarget camp)
      -- Bilbo is exhausted to pay
      chooseTarget pid bilbo
      sacksInPlay `shouldReturn` []
      sackDeckCodes `shouldReturn` [toCardCode Objectives.aDeepSack]
      getHeroResources (case bilbo of HeroTarget h -> h; _ -> error "hero") `shouldReturn` 0

  describe "the Trolls" do
    it "sacks 2 as William engages, and keeps Trolls not engaged from taking damage" . wmTest $ \pid -> do
      _ <- withBilbo pid
      setSackDeck [Objectives.aDeepSack, Objectives.aWornSack, Objectives.aStrongSack]
      william <- stage Enemies.william
      bert <- stage Enemies.bert
      getModifiers (EnemyTarget bert) >>= (`shouldSatisfy` \ms -> CannotTakeDamage `elem` ms && CannotHavePlayerAttachments `elem` ms)
      run $ EngageEnemy pid william
      useForcedAbilities pid
      -- A Worn Sack's heroes all have 0 resources: the first player picks one
      getQuestion pid >>= \case
        Just _ -> chooseTarget pid =<< (toTarget <$> getHeroWithCode "01012")
        Nothing -> pure ()
      length <$> sacksInPlay `shouldReturn` 2
      getModifiers (EnemyTarget william) >>= (`shouldSatisfy` notElem CannotTakeDamage)

    it "returns a Troll to the staging area at the end of the combat phase unless its player raises his threat" . wmTest $ \pid -> do
      tom <- stage Enemies.tom
      getModifiers (EnemyTarget tom) >>= (`shouldSatisfy` elem AttackedByOneCharacterAtATime)
      run $ PlaceEnemy tom (EngagedWith pid)
      threat <- threatOf pid
      run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
      useForcedAbilities pid
      chooseLabelStarting "$label.troll.raiseThreat" pid
      threatOf pid `shouldReturn` (threat + 1)
      placementOfEnemy tom `shouldReturn` EngagedWith pid
      run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
      useForcedAbilities pid
      chooseLabelStarting "$label.troll.return" pid
      placementOfEnemy tom `shouldReturn` StagingArea

    it "gives Trolls +1 threat per player while Troll Camp is in the staging area" . wmTest $ \_ -> do
      bert <- stage Enemies.bert
      getEnemyThreat bert `shouldReturn` 3
      _ <- stageLocation Locations.trollCamp
      getEnemyThreat bert `shouldReturn` 4

  describe "Troll Purse and Troll Key" do
    it "waits in the staging area until a Troll arrives, then goes to it" . wmTest $ \_ -> do
      run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.trollKey
      (.objectivePlacement) <$> objectiveOf Objectives.trollKey `shouldReturn` StagingArea
      bert <- stage Enemies.bert
      (.objectivePlacement) <$> objectiveOf Objectives.trollKey `shouldReturn` AttachedTo (EnemyTarget bert)

    it "lets the first player spend 1 Baggins resource to claim Troll Purse for Bilbo as its Troll is destroyed" . wmTest $ \pid -> do
      bilbo <- withBilbo pid
      run $ PlaceTokens GameSource bilbo Resource 1
      bert <- stage Enemies.bert
      run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.trollPurse
      (.objectivePlacement) <$> objectiveOf Objectives.trollPurse `shouldReturn` AttachedTo (EnemyTarget bert)
      run $ Destroy GameSource (EnemyTarget bert)
      chooseLabelStarting "$label.trollPurse.spend" pid
      (.objectivePlacement) <$> objectiveOf Objectives.trollPurse `shouldReturn` AttachedTo bilbo

    it "puts a Troll Key discarded from the encounter deck into the staging area" . wmTest $ \_ -> do
      _ <- setEncounterDeck [Objectives.trollKey, Locations.hobbitLands]
      run $ DiscardTopOfEncounterDeck GameSource GameTarget 1
      (.objectivePlacement) <$> objectiveOf Objectives.trollKey `shouldReturn` StagingArea
      map toCardCode <$> getEncounterDiscard `shouldReturn` []

    it "bars travel to Troll Cave until Bilbo has the Troll Key and 5 Baggins resources, 2 with the Troll Purse" . wmTest $ \pid -> do
      bilbo <- withBilbo pid
      cave <- stageLocation Locations.trollCave
      getModifiers (LocationTarget cave) >>= (`shouldSatisfy` \ms -> CannotBeTraveledTo `elem` ms && ImmuneToPlayerCardEffects `elem` ms)
      run . (`PutObjectiveIntoPlayAttached` bilbo) =<< genEncounterCard Objectives.trollKey
      run $ PlaceTokens GameSource bilbo Resource 2
      getModifiers (LocationTarget cave) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run . (`PutObjectiveIntoPlayAttached` bilbo) =<< genEncounterCard Objectives.trollPurse
      getModifiers (LocationTarget cave) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo cave
      getHeroResources (case bilbo of HeroTarget h -> h; _ -> error "hero") `shouldReturn` 0

  describe "the encounter cards" do
    it "sacks 1 for Lots or None at All with a Troll in the staging area, else raises threat by 2" . wmTest $ \pid -> do
      _ <- withBilbo pid
      setSackDeck [Objectives.aDeepSack]
      threat <- threatOf pid
      reveal [Treacheries.lotsOrNoneAtAll]
      threatOf pid `shouldReturn` (threat + 2)
      _ <- stage Enemies.bert
      reveal [Treacheries.lotsOrNoneAtAll]
      length <$> sacksInPlay `shouldReturn` 1

    it "discards all Baggins resources with More Like a Grocer" . wmTest $ \pid -> do
      bilbo <- withBilbo pid
      run $ PlaceTokens GameSource bilbo Resource 3
      reveal [Treacheries.moreLikeAGrocer]
      getHeroResources (case bilbo of HeroTarget h -> h; _ -> error "hero") `shouldReturn` 0

    it "gives Bilbo Baggins 1 resource as Dreary Hills is explored" . wmTest $ \pid -> do
      bilbo <- withBilbo pid
      hills <- stageLocation Locations.drearyHills
      run $ PlaceLocation hills ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget hills) Progress 2
      -- the Forced discard, then the Response as it leaves play explored
      useForcedAbilities pid
      getHeroResources (case bilbo of HeroTarget h -> h; _ -> error "hero") `shouldReturn` 1

  describe "the stages" do
    it "Roast Mutton brings the Trolls and discards from the encounter deck for its progress, advancing once it is empty" . wmTest $ \pid -> do
      _ <- withBilbo pid
      atRoastMutton pid
      codes <$> getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` codes [Enemies.william, Enemies.bert, Enemies.tom]
      _ <- setEncounterDeck [Locations.hobbitLands, Locations.loneLands, Locations.drearyHills]
      inPhase QuestPhase
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 2
      length <$> getEncounterDeck `shouldReturn` 1
      activeQuestCode `shouldReturn` toCardCode Quests.roastMutton
      -- the last card discarded leaves the deck empty: not reset, the stage advances
      run $ PlaceTokens GameSource q Progress 2
      activeQuestCode `shouldReturn` toCardCode Quests.dawnTakeYouAll

    it "Roast Mutton advances once no Troll is left in play" . wmTest $ \pid -> do
      _ <- withBilbo pid
      atRoastMutton pid
      _ <- setEncounterDeck [Locations.hobbitLands]
      for_ [Enemies.william, Enemies.bert, Enemies.tom] \def -> do
        eid <- enemyOf def
        run $ Destroy GameSource (EnemyTarget eid)
      activeQuestCode `shouldReturn` toCardCode Quests.dawnTakeYouAll

    it "Dawn Take You All wins the game, the troll hoard discovered with Troll Cave in the victory display" . campaignTestIn weMustAwayEreBreakOfDay (newCampaignLog "the-hobbit") testDeck $ \pid -> do
      _ <- withBilbo pid
      cave <- genEncounterCard Locations.trollCave
      putGame . (scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard cave])) =<< getGame
      _ <- stage Enemies.bert
      card <- genEncounterCard Quests.dawnTakeYouAll
      run $ AdvanceToQuest card
      resolveWhenRevealed pid
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      sort (map (\p -> (p.code, p.seat)) (campaignLogPool l)) `shouldBe` sort [("131020", Just 0), ("131021", Just 0), ("131022", Just 0)]

    it "Dawn Take You All discovers nothing without Troll Cave in the victory display" . campaignTestIn weMustAwayEreBreakOfDay (newCampaignLog "the-hobbit") testDeck $ \pid -> do
      _ <- withBilbo pid
      card <- genEncounterCard Quests.dawnTakeYouAll
      run $ AdvanceToQuest card
      resolveWhenRevealed pid
      getsGame (.outcome) `shouldReturn` Just Won
      campaignLogPool <$> getCampaignLog `shouldReturn` []
