module LordOfTheRings.QuestPhaseSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Effect
import LordOfTheRings.Keyword
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import TestImport

commitHeroes :: PlayerId -> [CardCode] -> GameT ()
commitHeroes pid codes = do
  for_ codes $ getHeroWithCode >=> chooseTarget pid
  whenJustM (getQuestion pid) $ \q ->
    when (notNull [() | Label "$label.doneCommitting" _ <- questionChoices q]) $ chooseLabel "$label.doneCommitting" pid
  -- decline any response to committing, such as Théodred's
  whenJustM (getQuestion pid) $ \q ->
    when (notNull [() | Label "$label.pass" _ <- questionChoices q]) $ chooseLabel "$label.pass" pid
  passActions pid

-- Flies and Spiders setup leaves Forest Spider (2 threat) and Old Forest Road (1 threat) in staging.
questWith :: PlayerId -> [CardDef] -> [CardCode] -> GameT ()
questWith pid deck codes = do
  flipFirstQuest pid
  _ <- setEncounterDeck deck
  run $ BeginPhase QuestPhase
  commitHeroes pid codes

spec :: Spec
spec = describe "Quest phase" do
  it "asks the player which characters to commit" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase QuestPhase
    Just question <- getQuestion pid
    length [() | TargetLabel {} <- questionChoices question] `shouldBe` 3

  it "waits for every character to be committed before responses to committing" . gameTestWith testDeck {heroes = ["01001", "01002", "01007"]} $ \pid -> do
    flipFirstQuest pid
    _ <- setEncounterDeck [Locations.forestGate]
    run $ BeginPhase QuestPhase
    aragorn <- getHeroWithCode "01001"
    theodred <- getHeroWithCode "01002"
    chooseTarget pid aragorn
    chooseTarget pid theodred
    chooseLabel "$label.doneCommitting" pid
    -- Théodred gives Aragorn the resource his response costs
    chooseAbilityOf pid theodred
    chooseTarget pid aragorn
    chooseAbilityOf pid aragorn
    aragorn' <- toAttrs <$> getHeroWithCode "01001"
    heroExhausted aragorn' `shouldBe` False
    heroCommitted aragorn' `shouldBe` True

  it "exhausts committed characters" . gameTest $ \pid -> do
    questWith pid [Locations.forestGate] ["01007"]
    eowyn <- getHeroWithCode "01007"
    heroExhausted (toAttrs eowyn) `shouldBe` True

  it "reveals one encounter card per player during staging" . gameTest $ \pid -> do
    questWith pid [Locations.forestGate] []
    getsGame (length . (.entities.locations)) `shouldReturn` 2

  it "places the difference as progress when the quest succeeds" . gameTest $ \pid -> do
    -- willpower 4 + 2 + 1 against staging threat 2 + 1 + 2
    questWith pid [Locations.forestGate] ["01007", "01012", "01002"]
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "raises every player's threat when the quest fails" . gameTest $ \pid -> do
    questWith pid [Locations.forestGate] ["01002"]
    (.threat) <$> getPlayer pid `shouldReturn` 31

  it "stops characters questing when the phase ends" . gameTest $ \pid -> do
    questWith pid [Locations.forestGate] ["01007"]
    runOnly $ EndPhase QuestPhase
    heroCommitted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False

  it "places progress on the active location before the quest" . gameTest $ \pid -> do
    flipFirstQuest pid
    location <- getsGame (fromJustNote "no location" . headMay . toList . (.entities.locations))
    run $ PlaceLocation (toId location) ActiveLocation
    run $ PlaceProgressOnQuest 5
    -- Old Forest Road has 3 quest points: it is explored and the other 2 go to the quest
    getsGame (Map.member (toId location) . (.entities.locations)) `shouldReturn` False
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "does not count the active location's threat in staging" . gameTest $ \pid -> do
    flipFirstQuest pid
    location <- getsGame (fromJustNote "no location" . headMay . toList . (.entities.locations))
    run $ PlaceLocation (toId location) ActiveLocation
    _ <- setEncounterDeck [Locations.forestGate]
    run $ BeginPhase QuestPhase
    -- willpower 4 against staging threat 2 + 2
    commitHeroes pid ["01007"]
    (.threat) <$> getPlayer pid `shouldReturn` 27
    locationPlacement . toAttrs <$> getsGame (fromJustNote "gone" . Map.lookup (toId location) . (.entities.locations))
      `shouldReturn` ActiveLocation

  it "quests with attack under battle" . gameTest $ \pid -> do
    flipFirstQuest pid
    quest <- toTarget <$> getActiveQuest
    run $ CreateEffect $ Effect GameSource quest [GainKeyword Battle] UntilEndOfRound
    _ <- setEncounterDeck [Locations.forestGate]
    run $ BeginPhase QuestPhase
    -- Théodred's attack 2 against staging threat 5
    commitHeroes pid ["01002"]
    (.threat) <$> getPlayer pid `shouldReturn` 30

  it "quests with defense under siege" . gameTest $ \pid -> do
    flipFirstQuest pid
    quest <- toTarget <$> getActiveQuest
    run $ CreateEffect $ Effect GameSource quest [GainKeyword Siege] UntilEndOfRound
    _ <- setEncounterDeck [Locations.forestGate]
    run $ BeginPhase QuestPhase
    -- Éowyn's defense 1 against staging threat 5
    commitHeroes pid ["01007"]
    (.threat) <$> getPlayer pid `shouldReturn` 31

  it "deals the quest's archery among the characters as combat begins" . gameTest $ \pid -> do
    flipFirstQuest pid
    quest <- toTarget <$> getActiveQuest
    run $ CreateEffect $ Effect GameSource quest [GainKeyword (Archery 2)] UntilEndOfRound
    run $ BeginPhase CombatPhase
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    chooseTarget pid eowyn
    getCharacterDamage eowyn `shouldReturn` 2

  it "deals the archery of an engaged enemy" . gameTest $ \pid -> do
    flipFirstQuest pid
    _ <- setEncounterDeck [Locations.forestGate]
    spider <- getsGame (fromJustNote "no enemy" . headMay . Map.keys . (.entities.enemies))
    run $ EngageEnemy pid spider
    run $ CreateEffect $ Effect GameSource (toTarget spider) [GainKeyword (Archery 2)] UntilEndOfRound
    run $ BeginPhase CombatPhase
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    chooseTarget pid eowyn
    getCharacterDamage eowyn `shouldReturn` 2
