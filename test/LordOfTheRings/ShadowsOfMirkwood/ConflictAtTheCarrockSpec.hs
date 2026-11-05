module LordOfTheRings.ShadowsOfMirkwood.ConflictAtTheCarrockSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfAttack))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getDefense)
import LordOfTheRings.Modifier (ModifierType (DefenseModifier))
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkAfter)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Scenario (conflictAtTheCarrock)
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

carrockTest :: (PlayerId -> GameT ()) -> IO ()
carrockTest = gameTestIn conflictAtTheCarrock testDeck

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

codesOf :: [EncounterCard] -> [CardCode]
codesOf = map toCardCode

spec :: Spec
spec = describe "Conflict at the Carrock" do
  it "sets The Carrock out and the Trolls and Sacked! aside, shuffling 1 Sacked! per player back" . carrockTest $ \pid -> do
    flipFirstQuest pid
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.theCarrock]
    aside <- getsGame (codesOf . scenarioSetAside . toAttrs . (.scenario))
    -- 4 set aside, then 1 per player shuffled back: 3 aside, and 2 of the 5 in the deck
    length (filter (== toCardCode Treacheries.sacked) aside) `shouldBe` 3
    length (filter (`elem` map toCardCode [Enemies.louis, Enemies.morris, Enemies.stuart, Enemies.rupert]) aside) `shouldBe` 4
    deck <- codesOf <$> getEncounterDeck
    length (filter (== toCardCode Treacheries.sacked) deck) `shouldBe` 2

  it "makes The Carrock active once the first stage has 7 progress" . carrockTest $ \pid -> do
    flipFirstQuest pid
    quest <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource quest Progress 7
    getsGame (map (\l -> (toCardCode l, l.placement)) . toList . (.entities.locations))
      `shouldReturn` [(toCardCode Locations.theCarrock, ActiveLocation)]

  it "brings the Trolls out at Against the Trolls, and wins once they are gone" . carrockTest $ \pid -> do
    flipFirstQuest pid
    quest <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource quest Progress 7
    resolveWhenRevealed pid
    trolls <- getsGame (map toId . toList . (.entities.enemies))
    length trolls `shouldBe` 4
    stage <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource stage Progress 1
    getsGame (.outcome) `shouldReturn` Nothing
    for_ trolls \t -> do
      run $ Discard GameSource (toTarget t)
      getQuestion pid >>= \case
        Just _ -> chooseLabel "$label.pass" pid
        Nothing -> pure ()
    getsGame (.outcome) `shouldReturn` Just Won

  it "keeps a Sacked! hero from committing or collecting resources" . carrockTest $ \pid -> do
    _ <- setEncounterDeck [Treacheries.sacked]
    run RevealEncounterCard
    resolveRevealed
    eowyn <- getHeroWithCode "01007"
    chooseTarget pid eowyn
    resources <- getHeroResources eowyn.id
    run $ BeginPhase ResourcePhase
    getHeroResources eowyn.id `shouldReturn` resources
    run $ CommitCharacters pid
    offered <- maybe False (any \case TargetLabel t _ -> t == toTarget eowyn; _ -> False) . fmap questionChoices <$> getQuestion pid
    offered `shouldBe` False

  it "cannot have Sacked!'s when revealed effect canceled" . carrockTest $ \pid -> do
    card <- putCardIntoHand pid Events.aTestOfWill
    eowyn <- getHeroWithCode "01007"
    run $ PlaceTokens GameSource (toTarget eowyn) Resource 1
    _ <- setEncounterDeck [Treacheries.sacked]
    run RevealEncounterCard
    offered <- maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices <$> getQuestion pid
    offered `shouldBe` False

  it "has Grimbeorn the Old join the first player at 8 resources" . carrockTest $ \pid -> do
    card <- genEncounterCard Objectives.grimbeornTheOld
    run $ AddToStaging card
    grimbeorn <- getsGame (toTarget . fromJustNote "no Grimbeorn" . headMay . toList . (.entities.objectives))
    run $ PlaceTokens GameSource grimbeorn Resource 8
    getsGame (map (\a -> (toCardCode a, a.placement)) . toList . (.entities.allies))
      `shouldReturn` [(toCardCode Objectives.grimbeornTheOld, InPlayArea pid)]

  it "has Grimbeorn the Old follow the first player token" . twoPlayerTest testDeck $ \one two -> do
    run . AddToStaging =<< genEncounterCard Objectives.grimbeornTheOld
    grimbeorn <- getsGame (toTarget . fromJustNote "no Grimbeorn" . headMay . toList . (.entities.objectives))
    run $ PlaceTokens GameSource grimbeorn Resource 8
    let placements = getsGame (map (.placement) . toList . (.entities.allies))
    placements `shouldReturn` [InPlayArea one]
    run $ SetFirstPlayer two
    placements `shouldReturn` [InPlayArea two]

  it "feeds Grimbeorn the Old a Leadership resource, not a Lore one" . carrockTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.grimbeornTheOld
    grimbeorn <- getsGame (fromJustNote "no Grimbeorn" . headMay . toList . (.entities.objectives))
    theodred <- getHeroWithCode "01002"
    beravor <- getHeroWithCode "01012"
    for_ [theodred, beravor] \h -> run $ PlaceTokens GameSource (toTarget h) Resource 1
    run $ ActionWindow pid
    chooseAbilityOf pid grimbeorn
    getsGame (map (.resources) . toList . (.entities.objectives)) `shouldReturn` [1]
    getHeroResources (toId theodred) `shouldReturn` 0
    getHeroResources (toId beravor) `shouldReturn` 1

  it "discards a character Muck Adder damages" . carrockTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.muckAdder]
    run RevealEncounterCard
    adder <- getsGame (fromJustNote "no adder" . headMay . map toId . toList . (.entities.enemies))
    eowyn <- hero "01007"
    run $ DealDamage (EnemySource adder) eowyn 1
    getsGame (any ((== eowyn) . toTarget) . toList . (.entities.heroes)) `shouldReturn` False

  it "lets Frodo Baggins turn damage into threat" . gameTestIn conflictAtTheCarrock frodoDeck $ \pid -> do
    frodo <- hero (toCardCode Heroes.frodoBaggins)
    threat <- (.threat) <$> getPlayer pid
    run $ DealDamage GameSource frodo 1
    chooseAbilityOf pid frodo
    getCharacterDamage frodo `shouldReturn` 0
    (.threat) <$> getPlayer pid `shouldReturn` threat + 1

  it "ends Muck Adder's shadow -1 defense when the attack resolves" . carrockTest $ \pid -> do
    _ <- setEncounterDeck [Enemies.muckAdder]
    run RevealEncounterCard
    adder <- getsGame (fromJustNote "no adder" . headMay . map toId . toList . (.entities.enemies))
    eowyn <- hero "01007"
    defense <- getDefense eowyn
    run $ CreateEffect $ Effect (EnemySource adder) eowyn [DefenseModifier (-1)] UntilEndOfAttack
    getDefense eowyn `shouldReturn` defense - 1
    run $ CheckWindows [mkAfter $ EnemyAttacking adder pid]
    getDefense eowyn `shouldReturn` defense

 where
  frodoDeck = deckList [toCardCode Heroes.frodoBaggins, "01007", "01002"] testDeck.cards
