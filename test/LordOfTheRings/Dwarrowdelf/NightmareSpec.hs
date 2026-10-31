module LordOfTheRings.Dwarrowdelf.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theRedhornGate)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (scenarioSetAside), encounterDiscardL, modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

inPlayCount :: CardDef -> GameT Int
inPlayCount def = getsGame \g ->
  length . filter (== toCardCode def)
    $ map toCardCode (toList g.entities.enemies)
    <> map toCardCode (toList g.entities.locations)
    <> map toCardCode (toList g.entities.treacheries)

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

setEncounterDiscard :: [CardDef] -> GameT ()
setEncounterDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

setAsideCards :: GameT [EncounterCard]
setAsideCards = getsGame (scenarioSetAside . toAttrs . (.scenario))

spec :: Spec
spec = describe "Dwarrowdelf nightmare decks" do
  describe "The Redhorn Gate" do
    it "builds the nightmare deck and sets the Blinding Blizzards aside" . nightmare theRedhornGate $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 44
      map (`copies` deck) [Enemies.mountainGoblin, Enemies.blackUruks, Enemies.silvertineMarauder, Treacheries.blindingBlizzard]
        `shouldBe` [0, 3, 4, 0]
      copies Treacheries.blindingBlizzard <$> setAsideCards `shouldReturn` 2

    it "shuffles the Blinding Blizzards in as stage 2b is revealed" . nightmare theRedhornGate $ \pid -> do
      run $ DoStep 1 Setup
      stage <- putQuestStage Quests.snowdrifts
      run $ Flip stage
      resolveWhenRevealed pid
      copies Treacheries.blindingBlizzard <$> getEncounterDeck `shouldReturn` 2

    it "Wolfpelt Goblin deals the active location's remaining quest points as damage" . nightmare theRedhornGate $ \pid -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.frozenSilverlode
      lid <- locationOf Locations.frozenSilverlode
      run $ PlaceTokens GameSource (toTarget lid) Progress 1
      reveal Enemies.wolfpeltGoblin [Locations.rockyCrags]
      eid <- enemyOf Enemies.wolfpeltGoblin
      run $ EngageEnemy pid eid
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 2
      (.progress) <$> getLocation lid `shouldReturn` 0

    it "Silvertine Marauder's shadow strikes a defender of little willpower" . nightmare theRedhornGate $ \pid -> do
      reveal Enemies.snowWarg []
      eid <- enemyOf Enemies.snowWarg
      theodred <- toTarget <$> getHeroWithCode "01002"
      shadow pid eid Enemies.silvertineMarauder [theodred]
      getEnemyAttack eid `shouldReturn` 5

    it "Icy Cavern's threat is the Snow cards in the staging area" . nightmare theRedhornGate $ \_ -> do
      reveal Locations.icyCavern []
      reveal Enemies.snowWarg []
      getStagingThreat `shouldReturn` 5

    it "Blinding Blizzard takes the quest's progress and leaves at 5" . nightmare theRedhornGate $ \_ -> do
      quest <- putQuestStage Quests.upThePass
      reveal Treacheries.blindingBlizzard []
      inPlayCount Treacheries.blindingBlizzard `shouldReturn` 1
      run $ PlaceTokens GameSource quest Progress 3
      (.progress) <$> getActiveQuest `shouldReturn` 0
      run $ PlaceTokens GameSource quest Progress 2
      inPlayCount Treacheries.blindingBlizzard `shouldReturn` 0

    it "Caradhras the Cruel returns the Snow cards and reveals one" . nightmare theRedhornGate $ \_ -> do
      setEncounterDiscard [Enemies.snowWarg, Treacheries.fellVoices]
      reveal Treacheries.caradhrasTheCruel [Locations.rockyCrags]
      inPlayCount Enemies.snowWarg `shouldReturn` 1
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.rockyCrags]
