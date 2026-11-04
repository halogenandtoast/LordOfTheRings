module LordOfTheRings.EredMithrinEngineSpec (spec) where

import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (..), playerCardsGuardedBy, select)
import LordOfTheRings.Placement (GuardRelease (..))
import TestImport

-- | An enemy in the staging area, and its card.
stagedEnemy :: CardDef -> GameT (EnemyId, EncounterCard)
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  e <- getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))
  pure (enemyId e, card)

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
