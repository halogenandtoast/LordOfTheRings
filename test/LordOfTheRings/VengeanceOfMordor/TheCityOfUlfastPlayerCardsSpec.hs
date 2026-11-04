module LordOfTheRings.VengeanceOfMordor.TheCityOfUlfastPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (GainKeyword))
import LordOfTheRings.Scenario.Types (victoryDisplayL)
import TestImport

beravorCode :: CardCode
beravorCode = "01012"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . fmap enemyId . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))

enemyInPlay :: EnemyId -> GameT Bool
enemyInPlay eid = getsGame (Map.member eid . (.entities.enemies))

inVictoryDisplay :: CardDef -> GameT ()
inVictoryDisplay def = do
  card <- genEncounterCard def
  putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (EncounterCard card :))) =<< getGame

inDiscard :: PlayerId -> CardDef -> GameT CardId
inDiscard pid def = do
  card <- genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . discardL %~ (PlayerCard card :)) =<< getGame
  pure card.id

spec :: Spec
spec = describe "The City of Ulfast player cards" do
  describe "Woodland Sentry" do
    it "played from hand, discards an enemy with a copy in the victory display" . gameTest $ \pid -> do
      inVictoryDisplay Enemies.forestSpider
      spider <- stagedEnemy Enemies.forestSpider
      troll <- stagedEnemy Enemies.hillTroll
      _ <- playWith pid beravorCode 3 Allies.woodlandSentry
      sentry <- getsGame (toTarget . fromJustNote "no sentry" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid sentry
      question <- getQuestion pid
      [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (EnemyTarget troll)
      chooseTarget pid spider
      enemyInPlay spider `shouldReturn` False
      enemyInPlay troll `shouldReturn` True

    it "discards a location with a copy in the victory display" . gameTest $ \pid -> do
      inVictoryDisplay Locations.oldForestRoad
      road <- genEncounterCard Locations.oldForestRoad
      run $ AddToStaging road
      _ <- playWith pid beravorCode 3 Allies.woodlandSentry
      sentry <- getsGame (toTarget . fromJustNote "no sentry" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid sentry
      lid <- getsGame (toTarget . fromJustNote "no location" . headMay . toList . (.entities.locations))
      chooseTarget pid lid
      getsGame (length . (.entities.locations)) `shouldReturn` 0

    it "is not offered when it is put into play rather than played" . gameTest $ \pid -> do
      inVictoryDisplay Enemies.forestSpider
      _ <- stagedEnemy Enemies.forestSpider
      sentry <- putAllyIntoPlay pid Allies.woodlandSentry
      offersAbilityOf pid sentry `shouldReturn` False

  describe "Keen Longbow" do
    it "goes on a Lore hero and gives it ranged" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      cardHosts pid (toCardCode Attachments.keenLongbow) `shouldReturn` [beravor]
      _ <- putAttachmentOn pid Attachments.keenLongbow beravor
      getModifiers beravor >>= (`shouldSatisfy` elem (GainKeyword Ranged))

    it "discards up to 3 cards to deal that much damage to the enemy attacked" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      bow <- putAttachmentOn pid Attachments.keenLongbow beravor
      cards <- traverse (putCardIntoHand pid) (replicate 4 Allies.gondorianSpearman :: [CardDef])
      eid <- stagedEnemy Enemies.hillTroll
      run $ EngageEnemy pid eid
      run $ PlayerAttacks pid
      chooseTarget pid beravor
      chooseAbilityOf pid bow
      for_ (take 2 cards) \c -> chooseTarget pid (CardIdTarget c.id)
      chooseLabel "$label.doneDiscarding" pid
      enemyDamage eid `shouldReturn` 2
      length . (.hand) <$> getPlayer pid `shouldReturn` 2

    it "stops at 3 cards" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      bow <- putAttachmentOn pid Attachments.keenLongbow beravor
      cards <- traverse (putCardIntoHand pid) (replicate 4 Allies.gondorianSpearman :: [CardDef])
      eid <- stagedEnemy Enemies.hillTroll
      run $ EngageEnemy pid eid
      run $ PlayerAttacks pid
      chooseTarget pid beravor
      chooseAbilityOf pid bow
      for_ (take 3 cards) \c -> chooseTarget pid (CardIdTarget c.id)
      enemyDamage eid `shouldReturn` 3
      length . (.hand) <$> getPlayer pid `shouldReturn` 1

  describe "Knight of Belfalas" do
    it "returns a Gondor ally from your discard pile to your hand" . gameTest $ \pid -> do
      spearman <- inDiscard pid Allies.gondorianSpearman
      knight <- putAllyIntoPlay pid Allies.knightOfBelfalas
      chooseAbilityOf pid knight
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [spearman]
