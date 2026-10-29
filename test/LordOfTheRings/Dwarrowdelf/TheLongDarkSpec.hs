module LordOfTheRings.Dwarrowdelf.TheLongDarkSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotBeAttacked, DefenseModifier, ThreatModifier))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theLongDark)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (QuestedUnsuccessfully), mkAfter)
import TestImport

darkTest :: (PlayerId -> GameT ()) -> IO ()
darkTest = gameTestIn theLongDark testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
      locations = [l.placement | l <- toList g.entities.locations, toCardCode l == code]
      enemies = [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
      objectives = [o.placement | o <- toList g.entities.objectives, toCardCode o == code]
      treacheries = [t.placement | t <- toList g.entities.treacheries, toCardCode t == code]
  pure $ headMay (locations <> enemies <> objectives <> treacheries)

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

progressOn :: LocationId -> GameT Int
progressOn lid = getsGame (maybe 0 (.progress) . lookup lid . (.entities.locations))

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

beravorExhausted :: GameT Bool
beravorExhausted = heroExhausted . toAttrs <$> getHeroWithCode "01012"

spec :: Spec
spec = describe "The Long Dark" do
  it "attaches Cave Torch and adds a location to the staging area, each location +1 threat" . darkTest $ \pid -> do
    flipFirstQuest pid
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    placementOf Objectives.caveTorch `shouldReturn` Just (AttachedTo beravor)
    staged <- getsGame (filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
    length staged `shouldBe` 1
    for_ staged \l -> getModifiers (toTarget l) >>= (`shouldSatisfy` elem (ThreatModifier 1))

  it "fails Foul Air's locate test when the first player stops: 2 damage to all, and Lost" . darkTest $ \pid -> do
    stage Locations.silentCaverns
    _ <- putCardIntoHand pid Events.dwarvenTomb
    reveal [Treacheries.foulAir, Enemies.goblinSneak]
    chooseLabel "$label.locateTest.fail" pid
    beravor <- toTarget <$> getHeroWithCode "01012"
    getCharacterDamage beravor `shouldReturn` 2
    beravorExhausted `shouldReturn` True
    length <$> getEncounterDeck `shouldReturn` 1

  it "discards a card from hand for each encounter card discarded, until one with the pass icon" . darkTest $ \pid -> do
    stage Locations.silentCaverns
    one <- putCardIntoHand pid Events.dwarvenTomb
    two <- putCardIntoHand pid Events.dwarvenTomb
    reveal [Treacheries.foulAir, Enemies.goblinSneak, Locations.abandonedMine]
    chooseTarget pid (CardIdTarget one.id)
    chooseTarget pid (CardIdTarget two.id)
    beravor <- toTarget <$> getHeroWithCode "01012"
    getCharacterDamage beravor `shouldReturn` 0
    beravorExhausted `shouldReturn` False
    (.hand) <$> getPlayer pid `shouldReturn` []
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.abandonedMine))

  it "cannot pass a locate test with nothing left to discard from the encounter deck" . darkTest $ \pid -> do
    stage Locations.silentCaverns
    card <- putCardIntoHand pid Events.dwarvenTomb
    reveal [Treacheries.foulAir]
    chooseTarget pid (CardIdTarget card.id)
    beravor <- toTarget <$> getHeroWithCode "01012"
    getCharacterDamage beravor `shouldReturn` 2

  it "places progress on Twisting Passage only when its locate test passes" . darkTest $ \pid -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.twistingPassage
    passage <- locationOf Locations.twistingPassage
    _ <- setEncounterDeck [Treacheries.fatigue]
    run $ PlaceProgressOnQuest 3
    progressOn passage `shouldReturn` 0
    _ <- setEncounterDeck [Treacheries.foulAir]
    card <- putCardIntoHand pid Events.dwarvenTomb
    run $ PlaceProgressOnQuest 3
    chooseTarget pid (CardIdTarget card.id)
    progressOn passage `shouldReturn` 3

  it "makes Twisting Passage's locate test before Cave Torch's Forced effect discards" . darkTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.caveTorch
    torch <- getsGame (fromJustNote "no torch" . fmap toTarget . find ((== toCardCode Objectives.caveTorch) . toCardCode) . toList . (.entities.objectives))
    stage Locations.twistingPassage
    card <- putCardIntoHand pid Events.dwarvenTomb
    _ <- setEncounterDeck [Enemies.goblinSneak, Locations.abandonedMine]
    run $ ActionWindow pid
    chooseAbilityOf pid torch
    void $ answerQuestion pid 3
    -- the locate test discards Goblin Sneak and fails; Cave Torch then discards the location
    chooseTarget pid (CardIdTarget card.id)
    getsGame (length . filter ((== StagingArea) . (.placement)) . toList . (.entities.enemies)) `shouldReturn` 0

  it "returns the top 2 Goblin enemies of the discard pile to the staging area when Abandoned Mine is lost" . darkTest $ \_ -> do
    stage Locations.abandonedMine
    pile <- traverse genEncounterCard [Enemies.goblinSneak, Treacheries.fatigue, Enemies.goblinWarlord, Enemies.goblinSneak]
    run $ AddToEncounterDiscard pile
    run $ TriggerLostEffects GameSource
    staged <- getsGame (map toCardCode . filter ((== StagingArea) . (.placement)) . toList . (.entities.enemies))
    sort staged `shouldBe` sort [toCardCode Enemies.goblinSneak, toCardCode Enemies.goblinWarlord]

  it "returns no Goblin when Abandoned Mine is lost with only 1 in the discard pile, as it is not able" . darkTest $ \_ -> do
    stage Locations.abandonedMine
    pile <- traverse genEncounterCard [Enemies.goblinSneak, Treacheries.fatigue]
    run $ AddToEncounterDiscard pile
    run $ TriggerLostEffects GameSource
    getsGame (length . filter ((== StagingArea) . (.placement)) . toList . (.entities.enemies)) `shouldReturn` 0

  it "has Cave Spider discard nothing from a hand of fewer than 4 cards, as the first player is not able" . darkTest $ \pid -> do
    _ <- putCardIntoHand pid Events.dwarvenTomb
    _ <- putCardIntoHand pid Events.dwarvenTomb
    reveal [Enemies.caveSpider]
    isNothing <$> getQuestion pid `shouldReturn` True
    length . (.hand) <$> getPlayer pid `shouldReturn` 3

  it "triggers Lost effects when the players quest unsuccessfully" . darkTest $ \_ -> do
    _ <- putQuestStage Quests.continuingEastward
    stage Locations.silentCaverns
    run $ CheckWindows [mkAfter QuestedUnsuccessfully]
    beravorExhausted `shouldReturn` True

  it "keeps Rock Adder from being attacked until it has dealt damage this round" . darkTest $ \_ -> do
    reveal [Enemies.rockAdder]
    adder <- enemyOf Enemies.rockAdder
    getModifiers (toTarget adder) >>= (`shouldSatisfy` elem CannotBeAttacked)
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ DealDamage (EnemySource adder) beravor 1
    getModifiers (toTarget adder) >>= (`shouldSatisfy` notElem CannotBeAttacked)

  it "attaches Durin's Greaves to the chosen hero, who gets +1 defense" . darkTest $ \pid -> do
    _ <- setEncounterDeck [Objectives.durinsGreaves]
    run RevealEncounterCard
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    placementOf Objectives.durinsGreaves `shouldReturn` Just (AttachedTo eowyn)
    getModifiers eowyn >>= (`shouldSatisfy` elem (DefenseModifier 1))

  it "gives each enemy revealed surge while Gathering Ground is attached" . darkTest $ \_ -> do
    stage Locations.dwarvenForge
    reveal [Treacheries.gatheringGround]
    forge <- locationOf Locations.dwarvenForge
    placementOf Treacheries.gatheringGround `shouldReturn` Just (AttachedTo (toTarget forge))
    reveal [Enemies.rockAdder, Locations.silentCaverns]
    placementOf Locations.silentCaverns `shouldReturn` Just StagingArea

  it "surges Fatigue when a player has no ready character left" . darkTest $ \pid -> do
    for_ ["01012", "01007"] \code -> run . Exhaust . toTarget =<< getHeroWithCode code
    reveal [Treacheries.fatigue, Locations.silentCaverns]
    chooseTarget pid =<< getHeroWithCode "01002"
    placementOf Locations.silentCaverns `shouldReturn` Just StagingArea

  it "wins once Continuing Eastward is defeated" . darkTest $ \_ -> do
    quest <- putQuestStage Quests.continuingEastward
    run $ PlaceTokens GameSource quest Progress 17
    getsGame (.outcome) `shouldReturn` Just Won
