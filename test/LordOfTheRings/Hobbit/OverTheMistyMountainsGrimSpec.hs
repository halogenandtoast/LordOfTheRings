module LordOfTheRings.Hobbit.OverTheMistyMountainsGrimSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdEncounterSet), HasCardCode, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getCommittedCharacters)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, overTheMistyMountainsGrim)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

omTest :: (PlayerId -> GameT ()) -> IO ()
omTest = gameTestIn overTheMistyMountainsGrim testDeck

-- | The test deck with Sting, discovered in We Must Away, Ere Break of Day.
treasureDeck :: DeckList
treasureDeck =
  deckList
    ["01012", "01007", "01002"]
    [ ("131020", 1), ("01013", 3), ("01014", 2), ("01015", 2), ("01016", 3), ("01017", 2), ("01031", 1)
    , ("01045", 2), ("01059", 2), ("01060", 1), ("01061", 2), ("01062", 2), ("01073", 3)
    , ("01026", 2), ("01027", 1), ("01057", 1), ("01069", 2), ("01070", 2), ("01072", 2)
    , ("01020", 2), ("01023", 2), ("01025", 1), ("01046", 2), ("01048", 2), ("01050", 2)
    , ("01051", 2), ("01053", 1), ("01064", 1)
    ]

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

codes :: HasCardCode c => [c] -> [CardCode]
codes = sort . map toCardCode

-- | The first player takes control of Bilbo Baggins, as the setup's third step does.
withBilbo :: PlayerId -> GameT HeroId
withBilbo pid = do
  run $ DoStep 3 Setup
  hero <- getHeroWithCode "131001"
  hero.owner `shouldBe` pid
  pure $ toId hero

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (codes . toList . (.entities.enemies))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (codes . toList . (.entities.locations))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

displayGreatGoblin :: GameT ()
displayGreatGoblin = do
  goblin <- genEncounterCard Enemies.theGreatGoblin
  g <- (scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard goblin])) <$> getGame
  -- the modifiers are read again from the game as it now is
  putGame g
  putGame =<< preloadModifiers g

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

-- | Take the first card a search of the encounter deck offers, if it asks.
takeFirstFound :: PlayerId -> GameT ()
takeFirstFound pid =
  getQuestion pid >>= \case
    Just q | any (\case TargetLabel {} -> True; _ -> False) (questionChoices q) ->
      chooseOptionMatching "take a card" pid \case TargetLabel {} -> True; _ -> False
    _ -> pure ()

withQuestDeck :: [CardDef] -> GameT ()
withQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (\a -> a {scenarioQuestDeck = cards})) =<< getGame

spec :: Spec
spec = describe "Over the Misty Mountains Grim" do
  it "is a scenario the game can be dealt" . omTest $ \_ ->
    isJust (lookupScenario "over-the-misty-mountains-grim") `shouldBe` True

  describe "setup" do
    it "builds the active encounter deck and sets the second one aside, inactive" . omTest $ \pid -> do
      flipFirstQuest pid
      attrs <- scenarioAttrs
      let inDeck = attrs.encounterDeck <> attrs.encounterDiscard
      all ((`elem` map Just [OverTheMistyMountainsGrim, WesternLands]) . cdEncounterSet . toCardDef) inDeck `shouldBe` True
      notNull inDeck `shouldBe` True
      all ((`elem` map Just [TheGreatGoblin, MistyMountainGoblins]) . cdEncounterSet . toCardDef) attrs.scenarioSetAside `shouldBe` True
      toCardCode Enemies.theGreatGoblin `elem` map toCardCode attrs.scenarioSetAside `shouldBe` True
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.theMountainPass, Quests.downDownToGoblinTown]
      activeQuestCode `shouldReturn` toCardCode Quests.aShortRest
      bilbo <- getHeroWithCode "131001"
      bilbo.owner `shouldBe` pid

  describe "the stages" do
    it "A Short Rest lets each player search his deck for a treasure, then advances" . gameTestIn overTheMistyMountainsGrim treasureDeck $ \pid -> do
      withQuestDeck [Quests.theMountainPass]
      q <- putQuestStage Quests.aShortRest
      run $ Flip q
      resolveWhenRevealed pid
      deck <- (.deck) <$> getPlayer pid
      let sting = fromJustNote "no Sting" $ find ((== "131020") . toCardCode) deck
      -- only the treasure can be taken
      Just question <- getQuestion pid
      length [() | TargetLabel {} <- questionChoices question] `shouldBe` 1
      chooseTarget pid (CardIdTarget sting.id)
      map toCardCode . (.hand) <$> getPlayer pid `shouldReturn` ["131020"]
      activeQuestCode `shouldReturn` toCardCode Quests.theMountainPass

    it "The Mountain Pass adds a Stone-giant and reveals 1 card per player" . omTest $ \pid -> do
      _ <- setEncounterDeck [Locations.overhangingRock, Enemies.stoneGiant, Locations.overhangingRock]
      run . AdvanceToQuest =<< genEncounterCard Quests.theMountainPass
      resolveWhenRevealed pid
      takeFirstFound pid
      resolveRevealed
      enemyCodes `shouldReturn` [toCardCode Enemies.stoneGiant]
      locationCodes `shouldReturn` [toCardCode Locations.overhangingRock]
      length <$> getEncounterDeck `shouldReturn` 1

    it "Down, Down to Goblin Town sets the first deck aside, makes the second active and brings The Great Goblin" . omTest $ \pid -> do
      bilbo <- withBilbo pid
      run $ PlaceTokens GameSource (HeroTarget bilbo) Resource 2
      _ <- stage Enemies.stoneGiant
      engaged <- stage Enemies.goblinAxeman
      run $ EngageEnemy pid engaged
      _ <- stageLocation Locations.overhangingRock
      _ <- setEncounterDeck [Treacheries.guffawingOfGiants]
      run $ DiscardTopOfEncounterDeck GameSource GameTarget 1
      _ <- setEncounterDeck [Locations.theHighPass]
      secondDeck <- traverse genEncounterCard [Enemies.theGreatGoblin, Enemies.goblinBentSwords, Enemies.goblinBentSwords, Locations.frontPorch, Locations.greatCavernRoom]
      putGame . (scenarioL %~ overAttrs (\a -> a {scenarioSetAside = secondDeck})) =<< getGame
      run . AdvanceToQuest =<< genEncounterCard Quests.downDownToGoblinTown
      resolveWhenRevealed pid
      takeFirstFound pid
      -- 3 cards for the one player: Bilbo spends 2 to reveal only 1
      Just question <- getQuestion pid
      length (questionChoices question) `shouldBe` 3
      void $ answerQuestion pid 2
      resolveRevealed
      getHeroResources bilbo `shouldReturn` 0
      attrs <- scenarioAttrs
      getEncounterDiscard `shouldReturn` []
      attrs.scenarioSetAside `shouldBe` []
      length attrs.encounterDeck `shouldBe` 3
      all ((`elem` map Just [TheGreatGoblin, MistyMountainGoblins]) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      -- nothing of the first deck is left in play
      onBoard <- (<>) <$> enemyCodes <*> locationCodes
      toCardCode Enemies.theGreatGoblin `elem` onBoard `shouldBe` True
      length onBoard `shouldBe` 2
      all (`elem` map toCardCode [Enemies.theGreatGoblin, Enemies.goblinBentSwords, Locations.frontPorch, Locations.greatCavernRoom]) onBoard `shouldBe` True

    it "Down, Down to Goblin Town cannot be defeated until The Great Goblin is in the victory display" . omTest $ \_ -> do
      q <- putQuestStage Quests.downDownToGoblinTown
      -- turned to side B, its When Revealed effect left waiting
      run $ Flip q
      getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
      displayGreatGoblin
      getModifiers q >>= (`shouldSatisfy` notElem CannotBeDefeated)

  describe "Over the Misty Mountains Grim's cards" do
    it "has a Stone-giant make the player it engages discard an ally" . omTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      giant <- stage Enemies.stoneGiant
      run $ EngageEnemy pid giant
      useForcedAbilities pid
      getsGame (length . (.entities.allies)) `shouldReturn` 0

    it "gives Galloping Boulders surge while a Stone-giant is in the staging area" . omTest $ \pid -> do
      _ <- withBilbo pid
      _ <- stage Enemies.stoneGiant
      _ <- setEncounterDeck [Treacheries.gallopingBoulders, Locations.overhangingRock]
      run RevealEncounterCard
      resolveRevealed
      locationCodes `shouldReturn` [toCardCode Locations.overhangingRock]

    it "has Galloping Boulders deal 3 damage to a questing character and remove it from the quest" . omTest $ \pid -> do
      _ <- withBilbo pid
      _ <- putQuestStage Quests.theMountainPass
      beravor <- toTarget <$> getHeroWithCode "01012"
      run $ CommitToQuest beravor
      reveal [Treacheries.gallopingBoulders, Locations.overhangingRock]
      getQuestion pid >>= \case
        Just _ -> chooseTarget pid beravor
        Nothing -> pure ()
      getCharacterDamage beravor `shouldReturn` 3
      getCommittedCharacters `shouldReturn` []
      -- no Stone-giant: no surge
      locationCodes `shouldReturn` []

    it "has the Stone-giant Guffawing of Giants chose engage the player with the highest threat at the end of the quest phase" . omTest $ \pid -> do
      giant <- stage Enemies.stoneGiant
      inPhase QuestPhase
      reveal [Treacheries.guffawingOfGiants]
      placementOfEnemy giant `shouldReturn` StagingArea
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      useForcedAbilities pid
      placementOfEnemy giant `shouldReturn` EngagedWith pid

    it "has Guffawing of Giants search for a Stone-giant when none is in the staging area" . omTest $ \pid -> do
      reveal [Treacheries.guffawingOfGiants, Locations.overhangingRock, Enemies.stoneGiant]
      takeFirstFound pid
      enemyCodes `shouldReturn` [toCardCode Enemies.stoneGiant]

    it "has The High Pass's threat be the number of players and remove that much progress at the end of the round" . omTest $ \pid -> do
      pass' <- stageLocation Locations.theHighPass
      getModifiers (LocationTarget pass') >>= (`shouldSatisfy` elem (ThreatModifier 1))
      q <- putQuestStage Quests.theMountainPass
      run $ PlaceTokens GameSource q Progress 3
      run $ CheckWindows [mkAfter RoundEnds]
      useForcedAbilities pid
      (.progress) <$> getActiveQuest `shouldReturn` 2

    it "lets the first player spend 1 Baggins resource at Overhanging Rock to take 1 of his top 2 cards" . omTest $ \pid -> do
      bilbo <- withBilbo pid
      run $ PlaceTokens GameSource (HeroTarget bilbo) Resource 1
      rock <- stageLocation Locations.overhangingRock
      run $ PlaceLocation rock ActiveLocation
      hand <- length . (.hand) <$> getPlayer pid
      deck <- length . (.deck) <$> getPlayer pid
      run $ ActionWindow pid
      chooseAbilityOf pid (LocationTarget rock)
      chooseOptionMatching "take a card" pid \case TargetLabel {} -> True; _ -> False
      length . (.hand) <$> getPlayer pid `shouldReturn` (hand + 1)
      length . (.deck) <$> getPlayer pid `shouldReturn` (deck - 2)
      getHeroResources bilbo `shouldReturn` 0

  describe "The Great Goblin's cards" do
    it "gives Goblin Bent-Swords surge once The Great Goblin is in the victory display" . omTest $ \_ -> do
      reveal [Enemies.goblinBentSwords, Locations.frontPorch]
      locationCodes `shouldReturn` []
      displayGreatGoblin
      reveal [Enemies.goblinBentSwords, Locations.frontPorch]
      locationCodes `shouldReturn` [toCardCode Locations.frontPorch]

    it "has The Great Goblin discard X cards after it attacks and add the Goblins among them to the staging area" . omTest $ \pid -> do
      goblin <- stage Enemies.theGreatGoblin
      _ <- setEncounterDeck [Enemies.goblinBentSwords, Locations.frontPorch]
      run $ CheckWindows [mkAfter $ EnemyAttacking goblin pid]
      useForcedAbilities pid
      enemyCodes `shouldReturn` codes [Enemies.theGreatGoblin, Enemies.goblinBentSwords]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.frontPorch]

    it "keeps Goblins from being attacked while Front Porch is active, unless the first player spends 2 Baggins resources" . omTest $ \pid -> do
      bilbo <- withBilbo pid
      run $ PlaceTokens GameSource (HeroTarget bilbo) Resource 2
      bent <- stage Enemies.goblinBentSwords
      porch <- stageLocation Locations.frontPorch
      run $ PlaceLocation porch ActiveLocation
      getModifiers (EnemyTarget bent) >>= (`shouldSatisfy` elem CannotBeAttacked)
      run $ ActionWindow pid
      chooseAbilityOf pid (LocationTarget porch)
      getModifiers (EnemyTarget bent) >>= (`shouldSatisfy` notElem CannotBeAttacked)
      getHeroResources bilbo `shouldReturn` 0

    it "makes the players travel to Great Cavern Room once The Great Goblin is in the victory display" . omTest $ \_ -> do
      room <- stageLocation Locations.greatCavernRoom
      getModifiers (LocationTarget room) >>= (`shouldSatisfy` notElem MustBeTraveledTo)
      displayGreatGoblin
      getModifiers (LocationTarget room) >>= (`shouldSatisfy` elem MustBeTraveledTo)

    it "has a player engaging a Goblin deal 1 damage to a character while Great Cavern Room is active" . omTest $ \pid -> do
      room <- stageLocation Locations.greatCavernRoom
      run $ PlaceLocation room ActiveLocation
      bent <- stage Enemies.goblinBentSwords
      run $ EngageEnemy pid bent
      useForcedAbilities pid
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 1

    it "returns engaged enemies with Chaos in the Cavern, Goblins getting +1 threat for the phase" . omTest $ \pid -> do
      bent <- stage Enemies.goblinBentSwords
      run $ EngageEnemy pid bent
      inPhase QuestPhase
      reveal [Treacheries.chaosInTheCavern]
      placementOfEnemy bent `shouldReturn` StagingArea
      getEnemyThreat bent `shouldReturn` 3

    it "gives the attacking enemy +1 attack with the Goblin Bent-Swords shadow" . omTest $ \pid -> do
      giant <- stage Enemies.stoneGiant
      run $ EngageEnemy pid giant
      run . AttachShadowCard giant =<< genEncounterCard Enemies.goblinBentSwords
      run $ ResolveShadow giant pid []
      getEnemyAttack giant `shouldReturn` 7

    it "gives the attacking enemy +2 attack with the Goblin Bent-Swords shadow once The Great Goblin is displayed" . omTest $ \pid -> do
      displayGreatGoblin
      giant <- stage Enemies.stoneGiant
      run $ EngageEnemy pid giant
      run . AttachShadowCard giant =<< genEncounterCard Enemies.goblinBentSwords
      run $ ResolveShadow giant pid []
      getEnemyAttack giant `shouldReturn` 8
