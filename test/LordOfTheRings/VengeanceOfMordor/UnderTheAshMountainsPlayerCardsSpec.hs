module LordOfTheRings.VengeanceOfMordor.UnderTheAshMountainsPlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardDef (cdKeywords))
import LordOfTheRings.Card.CardType (CardType (LocationType))
import LordOfTheRings.Enemy (EnemyAttrs (enemyCardId, enemyTokens))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (GuardedBy))
import LordOfTheRings.Modifier (ModifierType (AdditionalResourcePhaseDraws))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

gildorCode, gimliCode, beravorCode, eowynCode :: CardCode
gildorCode = "22081"
gimliCode = "01004"
beravorCode = "01012"
eowynCode = "01007"

-- | Gildor Inglorion (leadership), Gimli (tactics) and Éowyn (spirit).
ashTest :: (PlayerId -> GameT ()) -> IO ()
ashTest = gameTestWith (deckList [gildorCode, gimliCode, eowynCode] testDeck.cards)

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

handIds :: PlayerId -> GameT [CardId]
handIds pid = map (.id) . (.hand) <$> getPlayer pid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

onTopOfDeck :: PlayerId -> [CardDef] -> GameT [Card]
onTopOfDeck pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (cards <>)
  pure cards

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  eid <- getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies)) <&> toId
  run $ EngageEnemy pid eid
  pure eid

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . lookup eid . mapToList . (.entities.enemies))

offers :: PlayerId -> Target -> GameT Bool
offers pid t = maybe False (any \case TargetLabel t' _ -> t == t'; _ -> False) . fmap questionChoices <$> getQuestion pid

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

spec :: Spec
spec = describe "Under the Ash Mountains player cards" do
  describe "Gildor Inglorion" do
    it "pays a resource from his pool for a player to draw a card, once a round" . ashTest $ \pid -> do
      gildor <- hero gildorCode
      run $ PlaceTokens GameSource gildor Resource 2
      hand <- length <$> handIds pid
      run $ ActionWindow pid
      chooseAbilityOf pid gildor
      length <$> handIds pid `shouldReturn` hand + 1
      (getHeroResources . toId =<< getHeroWithCode gildorCode) `shouldReturn` 1
      offersAbilityOf pid gildor `shouldReturn` False

  describe "Wandering Exile" do
    it "discards a card from hand to ready after committing to the quest" . gameTest $ \pid -> do
      exile <- putAllyIntoPlay pid Allies.wanderingExile
      card <- putCardIntoHand pid Events.deepKnowledge
      run $ Exhaust exile
      run $ CheckWindows [mkAfter $ CommittedToQuest exile]
      chooseAbilityOf pid exile
      chooseTarget pid (CardIdTarget card.id)
      isExhausted exile `shouldReturn` False
      elem card.id <$> handIds pid `shouldReturn` False

  describe "Valiant Sword" do
    it "gives its Noble hero +1 attack, +2 at 40 threat" . ashTest $ \pid -> do
      gildor <- hero gildorCode
      _ <- putAttachmentOn pid Attachments.valiantSword gildor
      getAttack gildor `shouldReturn` 3
      t <- threat pid
      run $ RaiseThreat pid (40 - t)
      getAttack gildor `shouldReturn` 4

  describe "Stone of Elostirion" do
    it "is Guarded (location), and gives +2 willpower and an extra resource phase draw" . gameTest $ \pid -> do
      cdKeywords Attachments.stoneOfElostirion `shouldSatisfy` elem (GuardedBy [LocationType])
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.stoneOfElostirion eowyn
      getWillpower eowyn `shouldReturn` 6
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (AdditionalResourcePhaseDraws 1))

  describe "Northern Bowmaster" do
    it "cannot be played from hand unless you are engaged with an enemy" . ashTest $ \pid -> do
      bowmaster <- putCardIntoHand pid Allies.northernBowmaster
      gimli <- hero gimliCode
      run $ PlaceTokens GameSource gimli Resource 1
      run $ BeginPhase PlanningPhase
      offers pid (CardIdTarget bowmaster.id) `shouldReturn` False

    it "is played while you are engaged with an enemy" . ashTest $ \pid -> do
      bowmaster <- putCardIntoHand pid Allies.northernBowmaster
      gimli <- hero gimliCode
      run $ PlaceTokens GameSource gimli Resource 1
      _ <- engaged pid Enemies.dolGuldurOrcs
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget bowmaster.id)
      getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.northernBowmaster]

  describe "Reckless Attack" do
    it "has each player deal 2 damage to an enemy engaged with him" . ashTest $ \pid -> do
      eid <- engaged pid Enemies.dolGuldurOrcs
      _ <- playWith pid gimliCode 0 Events.recklessAttack
      threat pid `shouldReturn` 2
      enemyDamage eid `shouldReturn` 2

  describe "Odo Proudfoot" do
    it "searches the top 5 cards for a Hobbit ally after entering play" . gameTest $ \pid -> do
      [hobbit] <- onTopOfDeck pid [Allies.odoProudfoot]
      odo <- putAllyIntoPlay pid Allies.odoProudfoot
      chooseAbilityOf pid odo
      chooseTarget pid (CardIdTarget hobbit.id)
      elem hobbit.id <$> handIds pid `shouldReturn` True

  describe "Defiant Challenge" do
    it "lets each player ready a character he controls" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      run $ Exhaust beravor
      _ <- playWith pid eowynCode 0 Events.defiantChallenge
      threat pid `shouldReturn` 2
      chooseTarget pid beravor
      isExhausted beravor `shouldReturn` False

  describe "Helm of Secrecy" do
    it "swaps a hero for one from the collection of no higher threat" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- playWith pid beravorCode 4 Events.helmOfSecrecy
      chooseTarget pid beravor
      chooseOptionMatching "swap in Thalin" pid \case
        TargetLabel _ [SwapHero _ c] -> toCardCode c == "01006"
        _ -> False
      getsGame (elem "01006" . map toCardCode . toList . (.entities.heroes)) `shouldReturn` True
      passResponses pid
