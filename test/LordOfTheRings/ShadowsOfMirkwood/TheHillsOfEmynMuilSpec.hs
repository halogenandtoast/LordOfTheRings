module LordOfTheRings.ShadowsOfMirkwood.TheHillsOfEmynMuilSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theHillsOfEmynMuil)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

emynMuilTest :: (PlayerId -> GameT ()) -> IO ()
emynMuilTest = gameTestIn theHillsOfEmynMuil testDeck

revealLocation :: CardDef -> GameT LocationId
revealLocation def = do
  _ <- setEncounterDeck [def]
  run RevealEncounterCard
  getsGame (fromJustNote "no location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

offers :: PlayerId -> Target -> GameT Bool
offers pid t = maybe False (any \case TargetLabel t' _ -> t == t'; _ -> False) . fmap questionChoices <$> getQuestion pid

spec :: Spec
spec = describe "The Hills of Emyn Muil" do
  it "sets out Amon Hen and Amon Lhaw" . emynMuilTest $ \pid -> do
    flipFirstQuest pid
    codes <- getsGame (map toCardCode . toList . (.entities.locations))
    sort codes `shouldBe` sort (map toCardCode [Locations.amonHen, Locations.amonLhaw])

  it "puts explored locations with victory points in the victory display" . emynMuilTest $ \_ -> do
    lid <- revealLocation Locations.theHighlands
    run $ PlaceTokens GameSource (toTarget lid) Progress 1
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.theHighlands]

  it "lets an Ancient Mathom on the active Amon Lhaw answer its exploring" . emynMuilTest $ \pid -> do
    lid <- revealLocation Locations.amonLhaw
    run $ PlaceLocation lid ActiveLocation
    mathom <- putAttachmentOn pid Attachments.ancientMathom (toTarget lid)
    hand <- length . (.hand) <$> getPlayer pid
    run $ PlaceTokens GameSource (toTarget lid) Progress 5
    chooseAbilityOf pid mathom
    length . (.hand) <$> getPlayer pid `shouldReturn` hand + 3

  it "has no attachment played while Amon Lhaw is the active location" . emynMuilTest $ \pid -> do
    lid <- revealLocation Locations.amonLhaw
    notNull <$> cardHosts pid (toCardCode Attachments.ancientMathom) `shouldReturn` True
    run $ PlaceLocation lid ActiveLocation
    cardHosts pid (toCardCode Attachments.ancientMathom) `shouldReturn` []

  it "cannot be defeated without 20 victory points" . emynMuilTest $ \_ -> do
    stage <- putQuestStage Quests.theHillsOfEmynMuil
    run $ PlaceTokens GameSource stage Progress 5
    getsGame (.outcome) `shouldReturn` Nothing

  it "is won with 20 victory points and no Emyn Muil location in play" . emynMuilTest $ \_ -> do
    stage <- putQuestStage Quests.theHillsOfEmynMuil
    run $ PlaceTokens GameSource stage Progress 5
    for_ [Locations.amonHen, Locations.amonLhaw, Locations.theEastWallOfRohan, Locations.theNorthStair, Locations.raurosFalls, Locations.theOuterRidge] \def -> do
      lid <- revealLocation def
      run $ PlaceTokens GameSource (toTarget lid) Progress 10
    getsGame (.outcome) `shouldReturn` Just Won

  it "resolves only the When Revealed of the card The North Stair moves from the discard pile" . emynMuilTest $ \pid -> do
    run . AddToEncounterDiscard . pure =<< genEncounterCard Treacheries.massingInTheDeep
    lid <- revealLocation Locations.theNorthStair
    _ <- setEncounterDeck [Locations.theHighlands]
    threat <- (.threat) <$> getPlayer pid
    run $ TravelTo lid
    resolveRevealed
    -- Massing in the Deep reveals The Highlands, but its Doomed 1 does not fire
    (.threat) <$> getPlayer pid `shouldReturn` threat
    getsGame (map toCardCode . toList . (.entities.locations)) >>= (`shouldSatisfy` elem (toCardCode Locations.theHighlands))
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Treacheries.massingInTheDeep]

  it "keeps events from being played while Amon Hen is active" . emynMuilTest $ \pid -> do
    lid <- revealLocation Locations.amonHen
    run $ PlaceLocation lid ActiveLocation
    card <- putCardIntoHand pid Events.gildorsCounsel
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 3
    run $ BeginPhase QuestPhase
    offers pid (toTarget card) `shouldReturn` False

  it "offers Gildor's Counsel before committing, and reveals 1 card fewer" . emynMuilTest $ \pid -> do
    card <- putCardIntoHand pid Events.gildorsCounsel
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 3
    _ <- setEncounterDeck [Locations.theHighlands, Locations.theHighlands]
    run $ BeginPhase QuestPhase
    chooseTarget pid card
    chooseLabel "$label.doneCommitting" pid
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "keeps the players committing everyone at Rauros Falls" . emynMuilTest $ \pid -> do
    lid <- revealLocation Locations.raurosFalls
    run $ PlaceLocation lid ActiveLocation
    run $ CommitCharacters pid
    hasDone <- maybe False (any \case Label l _ -> l == "$label.doneCommitting"; _ -> False) . fmap questionChoices <$> getQuestion pid
    hasDone `shouldBe` False

  it "returns the active location to the staging area with Impassable Chasm" . emynMuilTest $ \_ -> do
    lid <- revealLocation Locations.theOuterRidge
    run $ PlaceLocation lid ActiveLocation
    _ <- setEncounterDeck [Treacheries.impassableChasm]
    run RevealEncounterCard
    resolveRevealed
    getsGame (fmap (.placement) . find ((== lid) . toId) . toList . (.entities.locations)) `shouldReturn` Just StagingArea

  it "gives surge to the first treachery of the quest phase with no location staged" . emynMuilTest $ \_ -> do
    _ <- putQuestStage Quests.theHillsOfEmynMuil
    putGame . (phaseL .~ QuestPhase) =<< getGame
    _ <- setEncounterDeck [Treacheries.treacherousFog, Treacheries.treacherousFog, Treacheries.treacherousFog]
    run RevealEncounterCard
    resolveRevealed
    length <$> getEncounterDeck `shouldReturn` 1

  it "looks only at the first treachery of the quest phase" . emynMuilTest $ \_ -> do
    _ <- putQuestStage Quests.theHillsOfEmynMuil
    putGame . (phaseL .~ QuestPhase) =<< getGame
    lid <- revealLocation Locations.theHighlands
    _ <- setEncounterDeck [Treacheries.treacherousFog, Treacheries.treacherousFog, Treacheries.treacherousFog]
    run RevealEncounterCard
    resolveRevealed
    run $ Discard GameSource (toTarget lid)
    run RevealEncounterCard
    resolveRevealed
    length <$> getEncounterDeck `shouldReturn` 1

  it "switches a card in hand with one on top of the deck with Gildor Inglorion" . emynMuilTest $ \pid -> do
    gildor <- putAllyIntoPlay pid Allies.gildorInglorion
    passActions pid
    _ <- putCardIntoHand pid Events.gildorsCounsel
    player <- getPlayer pid
    let fromHand = fromJustNote "empty hand" $ headMay player.hand
        fromDeck = fromJustNote "empty deck" $ headMay player.deck
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid gildor
    chooseTarget pid fromDeck
    chooseTarget pid fromHand
    updated <- getPlayer pid
    take 1 (map (.id) updated.deck) `shouldBe` [fromHand.id]
    (fromDeck.id `elem` map (.id) updated.hand) `shouldBe` True
