module LordOfTheRings.ShadowsOfMirkwood.TheHuntForGollumSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theHuntForGollum)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

huntTest :: (PlayerId -> GameT ()) -> IO ()
huntTest = gameTestIn theHuntForGollum testDeck

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

signsGuardedBy :: [CardDef] -> GameT Target
signsGuardedBy guards = do
  _ <- setEncounterDeck guards
  run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.signsOfGollum
  resolveRevealed
  getsGame (toTarget . fromJustNote "no objective" . headMay . toList . (.entities.objectives))

objectivePlacement' :: Target -> GameT Placement
objectivePlacement' (ObjectiveTarget oid) = getsGame (maybe Unplaced (objectivePlacement . toAttrs) . lookup oid . mapToList . (.entities.objectives))
objectivePlacement' _ = pure Unplaced

-- | Commit the hero and resolve the quest against an empty staging area.
questWith :: CardCode -> GameT ()
questWith code = do
  h <- hero code
  run $ CommitToQuest h
  run QuestResolution

-- | Claim free Signs of Gollum after questing successfully with Éowyn.
claimSigns :: PlayerId -> Target -> GameT ()
claimSigns pid signs = do
  questWith "01007"
  chooseAbilityOf pid signs
  chooseTarget pid =<< hero "01007"

spec :: Spec
spec = describe "The Hunt for Gollum" do
  it "reveals 1 card per player as the first stage is set up" . huntTest $ \pid -> do
    flipFirstQuest pid
    resolveRevealed
    deck <- length <$> getEncounterDeck
    discard <- length <$> getEncounterDiscard
    inPlay <- getsGame (\g -> length g.entities.enemies + length g.entities.locations + length g.entities.objectives)
    (deck < 48 && (discard + inPlay) >= 1) `shouldBe` True

  it "reveals 1 of the top 3 cards after questing successfully at The Hunt Begins" . huntTest $ \pid -> do
    _ <- putQuestStage Quests.theHuntBegins
    _ <- setEncounterDeck [Locations.theEavesOfMirkwood, Locations.riverNinglor, Locations.theOldFord, Enemies.huntersFromMordor]
    questWith "01007"
    chooseOptionMatching "reveal Eaves" pid \case
      TargetLabel (CardIdTarget _) _ -> True
      _ -> False
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    length <$> getEncounterDiscard `shouldReturn` 2
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.huntersFromMordor]

  it "shuffles the Signs of Gollum into the encounter deck at setup" . huntTest $ \_ -> do
    run Setup
    codes <- map toCardCode <$> getEncounterDeck
    take 4 codes `shouldSatisfy` (/= replicate 4 (toCardCode Objectives.signsOfGollum))

  it "guards Signs of Gollum with the next card when it reveals another Signs of Gollum" . huntTest $ \_ -> do
    _ <- setEncounterDeck [Objectives.signsOfGollum, Enemies.huntersFromMordor, Locations.theOldFord]
    run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.signsOfGollum
    resolveRevealed
    objectives <- getsGame (toList . (.entities.objectives))
    length objectives `shouldBe` 2
    guards <- getsGame (\g -> map toCardCode (toList g.entities.enemies) <> map toCardCode (toList g.entities.locations))
    sort guards `shouldBe` sort [toCardCode Enemies.huntersFromMordor, toCardCode Locations.theOldFord]
    all (isJust . objectiveGuard . toAttrs) objectives `shouldBe` True
    getEncounterDeck `shouldReturn` []

  it "lets Signs of Gollum be claimed onto a committed hero after questing successfully" . huntTest $ \pid -> do
    signs <- signsGuardedBy [Treacheries.oldWivesTales]
    claimSigns pid signs
    eowyn <- hero "01007"
    objectivePlacement' signs `shouldReturn` AttachedTo eowyn

  it "returns Signs of Gollum to the top of the encounter deck when its hero is damaged" . huntTest $ \pid -> do
    signs <- signsGuardedBy [Treacheries.oldWivesTales]
    claimSigns pid signs
    passActions pid
    eowyn <- hero "01007"
    run $ DealDamage GameSource eowyn 1
    map toCardCode . take 1 <$> getEncounterDeck `shouldReturn` [toCardCode Objectives.signsOfGollum]

  it "gives Hunters from Mordor +2 threat and +2 attack for each Clue in play" . huntTest $ \_ -> do
    _ <- signsGuardedBy [Treacheries.oldWivesTales]
    _ <- setEncounterDeck [Enemies.huntersFromMordor]
    run RevealEncounterCard
    eid <- getsGame (fromJustNote "no hunters" . headMay . map toId . toList . (.entities.enemies))
    getEnemyAttack eid `shouldReturn` 4
    getEnemyThreat eid `shouldReturn` 4

  it "resets to stage 2 when On the Trail has no hero with a Clue attached" . huntTest $ \_ -> do
    stage <- putQuestStage Quests.onTheTrail
    run $ Flip stage
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.aNewTerrorAbroad

  it "wins when On the Trail is defeated" . huntTest $ \pid -> do
    signs <- signsGuardedBy [Treacheries.oldWivesTales]
    claimSigns pid signs
    passActions pid
    stage <- putQuestStage Quests.onTheTrail
    run $ Flip stage
    run $ PlaceTokens GameSource stage Progress 8
    getsGame (.outcome) `shouldReturn` Just Won

  it "makes ally cards cost 1 more while The East Bank is active" . huntTest $ \pid -> do
    card <- putCardIntoHand pid Allies.rivendellMinstrel
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 3
    let offered = maybe False (any \case TargetLabel t _ -> t == toTarget card; _ -> False) . fmap questionChoices <$> getQuestion pid
    run $ BeginPhase PlanningPhase
    offered `shouldReturn` True
    passActions pid
    _ <- setEncounterDeck [Locations.theEastBank]
    run RevealEncounterCard
    lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
    run $ PlaceLocation lid ActiveLocation
    run $ BeginPhase PlanningPhase
    offered `shouldReturn` False

  it "takes any number of Eagles from the top 5 with The Eagles Are Coming!" . huntTest $ \pid -> do
    eagles <- traverse (\d -> genPlayerCard d (Just pid)) [Allies.wingedGuardian, Allies.wingedGuardian]
    g <- getGame
    putGame $ g & playersL . ix pid . deckL %~ (map PlayerCard eagles <>)
    card <- putCardIntoHand pid Events.theEaglesAreComing
    run $ BeginPhase PlanningPhase
    chooseTarget pid card
    for_ eagles \e -> chooseTarget pid (CardIdTarget (PlayerCard e).id)
    hand <- (.hand) <$> getPlayer pid
    all (\e -> any ((== (PlayerCard e).id) . (.id)) hand) eagles `shouldBe` True

  it "has Bilbo Baggins draw the first player an extra card in the resource phase" . gameTestIn theHuntForGollum bilboDeck $ \pid -> do
    hand <- length . (.hand) <$> getPlayer pid
    run $ BeginPhase ResourcePhase
    length . (.hand) <$> getPlayer pid `shouldReturn` hand + 2
 where
  bilboDeck = DeckList [toCardCode Heroes.bilboBaggins, "01007", "01002"] testDeck.cards
