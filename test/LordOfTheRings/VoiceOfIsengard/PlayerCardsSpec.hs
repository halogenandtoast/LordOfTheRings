module LordOfTheRings.VoiceOfIsengard.PlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Phase
import TestImport

-- | Gríma (lore), Beravor (lore) and Éowyn (spirit).
voiTest :: (PlayerId -> GameT ()) -> IO ()
voiTest = gameTestWith (DeckList [grimaCode, beravorCode, eowynCode] testDeck.cards)

grimaCode, beravorCode, eowynCode :: CardCode
grimaCode = "07002"
beravorCode = "01012"
eowynCode = "01007"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

onTopOfDeck :: PlayerId -> CardDef -> GameT Card
onTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (card :)
  pure card

inHand :: PlayerId -> CardId -> GameT Bool
inHand pid cid = elem cid . map (.id) . (.hand) <$> getPlayer pid

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  eid <- getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies)) <&> toId
  run $ EngageEnemy pid eid
  pure eid

shadowRevealed :: EnemyId -> GameT [CardId]
shadowRevealed eid = getsGame (maybe [] (enemyShadowRevealed . toAttrs) . lookup eid . mapToList . (.entities.enemies))

spec :: Spec
spec = describe "The Voice of Isengard player cards" do
  describe "Gríma" do
    it "lowers the next card's cost by 1, and it gains Doomed 1" . voiTest $ \pid -> do
      card <- putCardIntoHand pid Allies.isengardMessenger
      grima <- hero grimaCode
      run $ PlaceTokens GameSource grima Resource 1
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid grima
      was <- threat pid
      chooseTarget pid (CardIdTarget card.id)
      getsGame (length . (.entities.allies)) `shouldReturn` 1
      threat pid `shouldReturn` was + 1

  describe "Orthanc Guard" do
    it "readies after its controller raises his threat from Doomed" . voiTest $ \pid -> do
      orthanc <- putAllyIntoPlay pid Allies.orthancGuard
      run $ Exhaust orthanc
      was <- threat pid
      _ <- playWith pid beravorCode 0 Events.deepKnowledge
      threat pid `shouldReturn` was + 2
      chooseAbilityOf pid orthanc
      isExhausted orthanc `shouldReturn` False

  describe "Keys of Orthanc" do
    it "adds a resource to the attached hero after a Doomed threat raise" . voiTest $ \pid -> do
      beravor <- hero beravorCode
      theKeys <- putAttachmentOn pid Attachments.keysOfOrthanc beravor
      _ <- playWith pid beravorCode 0 Events.deepKnowledge
      chooseAbilityOf pid theKeys
      (getHeroResources . toId =<< getHeroWithCode beravorCode) `shouldReturn` 1

  describe "The Seeing-stone" do
    it "searches the deck for a card with Doomed" . voiTest $ \pid -> do
      knowledge <- onTopOfDeck pid Events.deepKnowledge
      was <- threat pid
      _ <- playWith pid grimaCode 0 Events.theSeeingStone
      threat pid `shouldReturn` was + 1
      chooseTarget pid (CardIdTarget knowledge.id)
      inHand pid knowledge.id `shouldReturn` True

  describe "Silver Lamp" do
    it "deals shadow cards face up while its hero is ready, and they still resolve" . voiTest $ \pid -> do
      _ <- putAttachmentOn pid Attachments.silverLamp =<< hero eowynCode
      eid <- engaged pid Enemies.blackForestBats
      [orcs] <- setEncounterDeck [Enemies.dolGuldurOrcs]
      run $ DealShadowCard eid
      shadowRevealed eid `shouldReturn` [orcs.id]
      attack <- getEnemyAttack eid
      run $ ResolveShadow eid pid []
      getEnemyAttack eid `shouldReturn` attack + 3

    it "leaves them face down while its hero is exhausted" . voiTest $ \pid -> do
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.silverLamp eowyn
      run $ Exhaust eowyn
      eid <- engaged pid Enemies.blackForestBats
      _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
      run $ DealShadowCard eid
      shadowRevealed eid `shouldReturn` []
