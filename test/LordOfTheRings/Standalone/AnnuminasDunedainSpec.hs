module LordOfTheRings.Standalone.AnnuminasDunedainSpec (spec) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getDefense, getWillpower, getHitPoints)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getAlly, getHero, hasKeyword)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, defendAttack, passAttackActions)
import TestImport

revealDunedain :: GameT [EncounterCard]
revealDunedain = do
  cards <- setEncounterDeck [Objectives.dunedainOfAnnuminas, Locations.oldForestRoad, Locations.oldForestRoad]
  run RevealEncounterCard
  resolveRevealed
  pure cards

controlledDunedain :: GameT [AllyAttrs]
controlledDunedain = getsGame $ map toAttrs . filter ((== toCardCode Objectives.dunedainOfAnnuminas) . toCardCode) . toList . (.entities.allies)

spec :: Spec
spec = describe "Annúminas Dúnedain objective ally" do
  it "lets a player pay from any sphere, preserves the physical card, and still resolves printed surge" . gameTest $ \pid -> do
    payer <- heroOf pid "01002"
    run $ PlaceTokens GameSource payer Resource 1
    cards <- revealDunedain
    chooseTarget pid pid
    allies <- controlledDunedain
    map allyCardId allies `shouldBe` [(.id) $ fromJustNote "missing encounter" $ headMay cards]
    map (.owner) allies `shouldBe` [pid]
    getsGame (length . (.entities.objectives)) `shouldReturn` 0
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    (.resources) <$> getHero (case payer of HeroTarget hid -> hid; _ -> error "expected hero") `shouldReturn` 0
    let aid = toId $ fromJustNote "missing ally" $ headMay allies
    getWillpower (AllyTarget aid) `shouldReturn` 1
    getDefense (AllyTarget aid) `shouldReturn` 2
    getHitPoints (AllyTarget aid) `shouldReturn` 3
    ally <- getAlly aid
    hasKeyword Ranged (AllyTarget aid) (toCardDef ally) `shouldReturn` True
    hasKeyword Sentinel (AllyTarget aid) (toCardDef ally) `shouldReturn` True

  it "allows a player other than the first player to pay and take control" . twoPlayerTest testDeck $ \one two -> do
    payer <- heroOf two "01007"
    run $ PlaceTokens GameSource payer Resource 1
    _ <- revealDunedain
    chooseTarget one two
    map (.owner) <$> controlledDunedain `shouldReturn` [two]
    getQuestion two `shouldReturn` Nothing

  it "automatically discards when nobody can pay, while printed surge reveals another card" . gameTest $ \_ -> do
    _ <- revealDunedain
    controlledDunedain `shouldReturn` []
    getsGame (length . (.entities.objectives)) `shouldReturn` 0
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.dunedainOfAnnuminas]

  it "allows all players to decline without spending the available resource" . gameTest $ \pid -> do
    payer <- heroOf pid "01012"
    run $ PlaceTokens GameSource payer Resource 1
    _ <- revealDunedain
    chooseLabel "$label.annuminas.declineDunedain" pid
    controlledDunedain `shouldReturn` []
    getsGame (length . (.entities.objectives)) `shouldReturn` 0
    (.resources) <$> getHero (case payer of HeroTarget hid -> hid; _ -> error "expected hero") `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "does not count resources from a hero forbidden to spend them" . gameTest $ \pid -> do
    payer <- heroOf pid "01012"
    run $ PlaceTokens GameSource payer Resource 1
    run $ CreateEffect $ Effect GameSource payer [CannotSpendResources] UntilEndOfRound
    _ <- revealDunedain
    controlledDunedain `shouldReturn` []
    getQuestion pid `shouldReturn` Nothing

  it "keeps printed surge when A Test of Will cancels the When Revealed effect" . gameTest $ \pid -> do
    payer <- heroOf pid "01007"
    run $ PlaceTokens GameSource payer Resource 1
    cancellation <- putCardIntoHand pid Events.aTestOfWill
    _ <- setEncounterDeck [Objectives.dunedainOfAnnuminas, Locations.oldForestRoad, Locations.oldForestRoad]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget cancellation.id)
    resolveRevealed
    controlledDunedain `shouldReturn` []
    getsGame (length . (.entities.objectives)) `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "supports two non-unique copies under the same player's control" . gameTest $ \pid -> do
    payer <- heroOf pid "01007"
    run $ PlaceTokens GameSource payer Resource 2
    cards <- setEncounterDeck [Objectives.dunedainOfAnnuminas, Objectives.dunedainOfAnnuminas, Locations.oldForestRoad]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid pid
    resolveRevealed
    chooseTarget pid pid
    allies <- controlledDunedain
    sort (map allyCardId allies) `shouldBe` sort (map (.id) $ take 2 cards)
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "preserves the claim choice through save and reload without duplicating payment" . gameTest $ \pid -> do
    payer <- heroOf pid "01007"
    run $ PlaceTokens GameSource payer Resource 1
    _ <- revealDunedain
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    chooseTarget pid pid
    length <$> controlledDunedain `shouldReturn` 1
    (.resources) <$> getHero (case payer of HeroTarget hid -> hid; _ -> error "expected hero") `shouldReturn` 0

  it "can enter directly as a quest reward without payment or surge, and reloads as a controlled ally" . gameTest $ \pid -> do
    card <- genEncounterCard Objectives.dunedainOfAnnuminas
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ PutObjectiveAllyIntoPlayUnder pid card
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    allies <- controlledDunedain
    map allyCardId allies `shouldBe` [card.id]
    map (.owner) allies `shouldBe` [pid]
    map allyExhausted allies `shouldBe` [False]
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    length <$> getEncounterDeck `shouldReturn` 1

  for_ [False, True] $ \barred ->
    it ("readies the defender as an encounter effect and gives defense only for the current attack, absolute readying prohibition: " <> show barred) . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      defender <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource defender [if barred then CannotReady else CannotBeReadiedByPlayerCards] UntilEndOfRound
      _ <- setEncounterDeck [Objectives.dunedainOfAnnuminas, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid defender
      passAttackActions
      getDamage defender `shouldReturn` 0
      heroExhausted . toAttrs <$> getHero (case defender of HeroTarget hid -> hid; _ -> error "expected hero") `shouldReturn` barred
      getDefense defender `shouldReturn` 2
      getModifiers defender >>= (`shouldSatisfy` notElem (DefenseModifier 2))
