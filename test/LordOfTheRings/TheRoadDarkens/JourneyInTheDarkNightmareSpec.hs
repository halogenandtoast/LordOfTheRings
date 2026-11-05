module LordOfTheRings.TheRoadDarkens.JourneyInTheDarkNightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (CannotHaveAttachments))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "staged location"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.locations)
    )

stageSlabBearer :: GameT EnemyId
stageSlabBearer = do
  run . AddToStaging =<< genEncounterCard Enemies.slabBearer
  getsGame
    ( toId . fromJustNote "Slab-bearer" . find ((== toCardCode Enemies.slabBearer) . toCardCode)
        . toList . (.entities.enemies)
    )

spec :: Spec
spec = describe "Journey in the Dark Nightmare" do
  it "Darkened Mine prevents progress on itself and other staged locations, but permits active-location progress" . gameTest $ \_ -> do
    mine <- stageLocation Locations.darkenedMine
    other <- stageLocation Locations.theOldFord
    run $ PlaceTokens GameSource (toTarget mine) Progress 1
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation mine `shouldReturn` 0
    (.progress) <$> getLocation other `shouldReturn` 0
    run $ PlaceLocation other ActiveLocation
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation other `shouldReturn` 1

  it "Darkened Mine stops blocking staged locations when it becomes active" . gameTest $ \_ -> do
    mine <- stageLocation Locations.darkenedMine
    other <- stageLocation Locations.theOldFord
    run $ PlaceLocation mine ActiveLocation
    run $ PlaceTokens GameSource (toTarget mine) Progress 1
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation mine `shouldReturn` 1
    (.progress) <$> getLocation other `shouldReturn` 1

  it "Darkened Mine's shadow discards one card from the defending player's hand" . gameTest $ \pid -> do
    _ <- putCardIntoHand pid Events.aTestOfWill
    _ <- putCardIntoHand pid Events.fortuneOrFate
    handBefore <- length . (.hand) <$> getPlayer pid
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    eid <- getsGame (toId . fromJustNote "shadow enemy" . headMay . toList . (.entities.enemies))
    run . AttachShadowCard eid =<< genEncounterCard Locations.darkenedMine
    run $ ResolveShadow eid pid []
    length . (.hand) <$> getPlayer pid `shouldReturn` (handBefore - 1)

  it "Very Weary damages a quester and reduces damaged characters' willpower even when they are not questing" . gameTest $ \_ -> do
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    quester <- toTarget <$> getHeroWithCode "01012"
    other <- toTarget <$> getHeroWithCode "01007"
    base <- getWillpower quester
    otherBase <- getWillpower other
    run $ CommitToQuest quester
    run $ PlaceTokens GameSource other Damage 1
    _ <- setEncounterDeck [Treacheries.veryWeary]
    run RevealEncounterCard
    resolveRevealed
    getDamage quester `shouldReturn` 1
    getDamage other `shouldReturn` 1
    getWillpower quester `shouldReturn` max 0 (base - 2)
    getWillpower other `shouldReturn` max 0 (otherBase - 2)

  it "Very Weary tracks newly damaged and healed characters until the phase ends" . gameTest $ \_ -> do
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    hero <- toTarget <$> getHeroWithCode "01012"
    base <- getWillpower hero
    _ <- setEncounterDeck [Treacheries.veryWeary]
    run RevealEncounterCard
    resolveRevealed
    getDamage hero `shouldReturn` 0
    getWillpower hero `shouldReturn` base
    run $ PlaceTokens GameSource hero Damage 1
    getWillpower hero `shouldReturn` max 0 (base - 2)
    run $ RemoveTokens GameSource hero Damage 1
    getWillpower hero `shouldReturn` base
    run $ PlaceTokens GameSource hero Damage 1
    run $ EndPhase PlanningPhase
    getDamage hero `shouldReturn` 1
    getWillpower hero `shouldReturn` base

  it "Very Weary's shadow damages the defender without applying its when-revealed penalty" . gameTest $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    base <- getWillpower hero
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    eid <- getsGame (toId . fromJustNote "shadow enemy" . headMay . toList . (.entities.enemies))
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.veryWeary
    run $ ResolveShadow eid pid [hero]
    getDamage hero `shouldReturn` 1
    getWillpower hero `shouldReturn` base

  it "Slab-bearer cannot have attachments and stages an enemy when destroyed without revealing it" . gameTest $ \_ -> do
    eid <- stageSlabBearer
    getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem CannotHaveAttachments)
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ CommitToQuest hero
    _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
    run $ Destroy GameSource (EnemyTarget eid)
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.dolGuldurOrcs]
    getDamage hero `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.slabBearer]

  it "Slab-bearer leaves a discarded location in the discard pile" . gameTest $ \_ -> do
    eid <- stageSlabBearer
    _ <- setEncounterDeck [Locations.darkenedMine]
    run $ Destroy GameSource (EnemyTarget eid)
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    sort . map toCardCode <$> getEncounterDiscard
      `shouldReturn` sort [toCardCode Enemies.slabBearer, toCardCode Locations.darkenedMine]

  it "discarding Slab-bearer does not trigger its destruction effect" . gameTest $ \_ -> do
    eid <- stageSlabBearer
    _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
    run $ Discard GameSource (EnemyTarget eid)
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.slabBearer]
