module LordOfTheRings.HaradrimEngineSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (AllyMatcher (..), EnemyMatcher (..), HeroMatcher (..), PlayerMatcher (..), charactersGuardedBy, select)
import LordOfTheRings.Message.Lifted (chooseOrRunOneM, placeProgressOnMainQuest, ready, removeMainQuestProgressLabeled, removeProgressFromMainQuest)
import LordOfTheRings.Modifier (ModifierType (..))
import TestImport

-- | An enemy in the staging area, and its card.
stagedEnemy :: CardDef -> GameT (EnemyId, EncounterCard)
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  eid <- getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))
  pure (enemyId eid, card)

lasting :: Target -> ModifierType -> GameT ()
lasting t m = run $ CreateEffect $ Effect GameSource t [m] UntilEndOfRound

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

spec :: Spec
spec = describe "The Haradrim engine" do
  it "places no more progress on a quest card than it may take now" . gameTest $ \pid -> do
    flipFirstQuest pid
    quest <- toTarget <$> getActiveQuest
    lasting quest (ProgressLimit 3)
    run $ PlaceTokens GameSource quest Progress 5
    (.progress) <$> getActiveQuest `shouldReturn` 3

  it "places and removes progress on the main quest, offering a removal only in full" . gameTest $ \pid -> do
    flipFirstQuest pid
    runQueueT $ placeProgressOnMainQuest GameSource 3
    run $ Run []
    runQueueT $ removeProgressFromMainQuest GameSource 1
    run $ Run []
    (.progress) <$> getActiveQuest `shouldReturn` 2
    runQueueT $ chooseOrRunOneM pid $ removeMainQuestProgressLabeled GameSource 3
    run $ Run []
    (.progress) <$> getActiveQuest `shouldReturn` 2
    runQueueT $ chooseOrRunOneM pid $ removeMainQuestProgressLabeled GameSource 2
    run $ Run []
    (.progress) <$> getActiveQuest `shouldReturn` 0

  it "keeps a stage that cannot be defeated, and defeats it once it can be" . gameTest $ \pid -> do
    flipFirstQuest pid
    quest <- getActiveQuest
    lasting (toTarget quest) CannotBeDefeated
    run $ PlaceTokens GameSource (toTarget quest) Progress quest.questPoints
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode quest
    putGame . (effectsL .~ []) =<< getGame
    run $ Run []
    toCardCode <$> getActiveQuest `shouldReturn` "01120"

  it "has a player discard down to his hand size limit at once" . gameTest $ \pid -> do
    replicateM_ 7 $ putCardIntoHand pid Allies.guardOfTheCitadel
    lasting (PlayerTarget pid) (HandSizeLimit 5)
    run $ Run []
    excess <- subtract 5 <$> handSize pid
    replicateM_ excess do
      c <- fromJustNote "empty hand" . headMay . (.hand) <$> getPlayer pid
      chooseTarget pid (CardIdTarget c.id)
    handSize pid `shouldReturn` 5

  it "lets no player card effect ready a character while barred, but the refresh phase still does" . gameTest $ \_ -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ Exhaust eowyn
    lasting GameTarget CannotBeReadiedByPlayerCards
    runQueueT $ ready eowyn
    run $ Run []
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
    run $ Ready eowyn
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False

  it "bars a character from defending against matching enemies" . gameTest $ \_ -> do
    eowyn <- toTarget <$> getHeroWithCode "01007"
    (eid, _) <- stagedEnemy Enemies.forestSpider
    canDefend eid eowyn `shouldReturn` True
    lasting eowyn (CannotDefendAgainst (EnemyWithId eid))
    canDefend eid eowyn `shouldReturn` False

  it "holds a captive ally and hero under its guard, rescuing them exhausted as it leaves play" . gameTest $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    eowyn <- getHeroWithCode "01007"
    (eid, card) <- stagedEnemy Enemies.forestSpider
    run $ GuardCharacter ally card.id
    run $ GuardCharacter (toTarget eowyn) card.id
    select AnyAlly `shouldReturn` []
    select (HeroControlledBy (PlayerWithId pid) <> HeroWithId eowyn.id) `shouldReturn` []
    charactersGuardedBy card.id >>= (`shouldSatisfy` ((== 2) . length))
    run $ Discard (EnemyTarget eid)
    charactersGuardedBy card.id `shouldReturn` []
    a <- getsGame (fromJustNote "no ally" . headMay . toList . (.entities.allies))
    (toAttrs a).allyPlacement `shouldBe` InPlayArea pid
    (toAttrs a).allyExhausted `shouldBe` True
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
