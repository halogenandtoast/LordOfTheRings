module LordOfTheRings.EredMithrinEngineSpec (spec) where

import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Deep))
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (..), playerCardsGuardedBy, select)
import LordOfTheRings.Message.Lifted (arrangeTopOfCavesDeck)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Placement (GuardRelease (..))
import LordOfTheRings.Scenario.Types (cavesDeckL)
import TestImport

-- | An enemy in the staging area, and its card.
stagedEnemy :: CardDef -> GameT (EnemyId, EncounterCard)
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  e <- getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))
  pure (enemyId e, card)

-- | A location in the staging area.
stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no location" . fmap toId . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

lasting :: Target -> ModifierType -> GameT ()
lasting t m = run $ CreateEffect $ Effect GameSource t [m] UntilEndOfRound

setCavesDeck :: [CardDef] -> GameT [EncounterCard]
setCavesDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (cavesDeckL .~ cards)
  pure cards

getCavesDeck :: GameT [EncounterCard]
getCavesDeck = getsGame ((.cavesDeck) . toAttrs . (.scenario))

activeLocationCodes :: GameT [CardCode]
activeLocationCodes = getsGame (map toCardCode . filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations))

spec :: Spec
spec = describe "The Ered Mithrin engine" do
  it "guards a Guarded (X) attachment with the first card of its types, attaching it once free" . gameTest $ \pid -> do
    void $ setEncounterDeck [Locations.oldForestRoad, Enemies.forestSpider]
    steward <- genPlayerCard Attachments.stewardOfGondor (Just pid)
    run $ ResolveGuardedPlayerCard pid steward [EnemyType]
    [eid] <- select EnemyGuardingACard
    spider <- getsGame (fromJustNote "no spider" . fmap toAttrs . find ((== eid) . toId) . toList . (.entities.enemies))
    playerCardsGuardedBy (enemyCardId spider) `shouldReturn` [steward]
    map toCardCode <$> getEncounterDiscard `shouldReturn` ["01099"]
    run $ Discard (EnemyTarget eid)
    hero <- heroOf pid "01012"
    chooseTarget pid hero
    getsGame (map (attachmentPlacement . toAttrs) . filter ((== steward.id) . attachmentCardId . toAttrs) . toList . (.entities.attachments))
      `shouldReturn` [AttachedTo hero]
    select EnemyGuardingACard `shouldReturn` []

  it "shuffles the discard pile in for a guard when the deck has none of its types" . gameTest $ \pid -> do
    void $ setEncounterDeck [Locations.oldForestRoad]
    spider <- genEncounterCard Enemies.forestSpider
    run $ AddToEncounterDiscard [spider]
    steward <- genPlayerCard Attachments.stewardOfGondor (Just pid)
    run $ ResolveGuardedPlayerCard pid steward [EnemyType]
    playerCardsGuardedBy spider.id `shouldReturn` [steward]

  it "returns a card it holds to its owner's hand once its guard leaves play" . gameTest $ \pid -> do
    (eid, card) <- stagedEnemy Enemies.forestSpider
    held <- genPlayerCard Attachments.stewardOfGondor (Just pid)
    run $ GuardPlayerCard card.id ReturnToHandWhenFree held
    select EnemyGuardingACard `shouldReturn` [eid]
    run $ Discard (EnemyTarget eid)
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [held.id]

  it "replaces a Deep location traveled to with the top card of the Caves deck" . gameTest $ \_ -> do
    void $ setCavesDeck [Locations.oldForestRoad, Locations.banksOfTheAnduin]
    lid <- stagedLocation Locations.enchantedStream
    lasting (LocationTarget lid) (GainKeyword Deep)
    run $ TravelTo lid
    activeLocationCodes `shouldReturn` ["01099"]
    map toCardCode <$> getEncounterDiscard `shouldReturn` ["01095"]
    map toCardCode <$> getCavesDeck `shouldReturn` ["01113"]

  it "ignores the Deep keyword while a card says to" . gameTest $ \_ -> do
    void $ setCavesDeck [Locations.oldForestRoad]
    lid <- stagedLocation Locations.enchantedStream
    lasting (LocationTarget lid) (GainKeyword Deep)
    lasting GameTarget IgnoreDeep
    run $ TravelTo lid
    activeLocationCodes `shouldReturn` ["01095"]

  it "keeps one of the Caves deck's top two cards on top and puts the other on the bottom" . gameTest $ \pid -> do
    [a, b, c] <- setCavesDeck [Locations.oldForestRoad, Locations.banksOfTheAnduin, Locations.enchantedStream]
    runQueueT $ arrangeTopOfCavesDeck pid GameTarget
    run $ Run []
    chooseTarget pid (CardIdTarget b.id)
    map (.id) <$> getCavesDeck `shouldReturn` map (.id) [b, c, a]
