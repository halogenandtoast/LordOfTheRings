module LordOfTheRings.EredMithrin.TheGhostOfFramsburgPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotCommitToQuest, Defending, ForNextAttack, ReduceCostToAttach, AttackModifier))
import LordOfTheRings.Matcher.Types (CardMatcher (CardWithTrait))
import LordOfTheRings.Trait (Trait (Item))
import LordOfTheRings.Window (WindowType (EnemyDestroyedByAttack), mkAfter)
import TestImport

dainCode, eowynCode, theodredCode, beravorCode, legolasCode :: CardCode
dainCode = "19084"
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"
legolasCode = "01005"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

dainTest :: (PlayerId -> GameT ()) -> IO ()
dainTest = gameTestWith (deckList [dainCode, eowynCode, theodredCode] testDeck.cards)

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  existing <- getsGame (Map.keys . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . find (`notElem` existing) . Map.keys . (.entities.locations))

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  existing <- getsGame (Map.keys . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  eid <- getsGame (fromJustNote "no enemy" . find (`notElem` existing) . Map.keys . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  AttachmentTarget aid -> maybe False (attachmentExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.attachments)
  _ -> pure False

deckSize :: PlayerId -> GameT Int
deckSize pid = length . (.deck) <$> getPlayer pid

allyWithCode :: CardDef -> GameT (Maybe Target)
allyWithCode def = getsGame (fmap toTarget . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

attachmentPlacements :: CardDef -> GameT [Placement]
attachmentPlacements def = getsGame (map (.placement) . filter ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

-- | The card goes on top of the player's deck.
onTopOfDeck :: PlayerId -> CardDef -> GameT ()
onTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (card :)) =<< getGame

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card = do
  question <- getQuestion pid
  pure $ CardIdTarget card.id `elem` [t | Just q <- [question], TargetLabel t _ <- questionChoices q]

spec :: Spec
spec = describe "The Ghost of Framsburg player cards" do
  describe "Dáin Ironfoot (spirit)" do
    it "discards the top of his deck for +1 defense while defending, 3 times a phase" . dainTest $ \pid -> do
      dain <- hero dainCode
      run $ CreateEffect $ Effect GameSource dain [Defending] UntilEndOfRound
      deck <- deckSize pid
      for_ [4, 5, 6 :: Int] \n -> do
        run $ ActionWindow pid
        chooseAbilityOf pid dain
        getDefense dain `shouldReturn` n
        passActions pid
      deckSize pid `shouldReturn` deck - 3
      run $ ActionWindow pid
      offersAbilityOf pid dain `shouldReturn` False

    it "has no action while not defending" . dainTest $ \pid -> do
      dain <- hero dainCode
      run $ ActionWindow pid
      offersAbilityOf pid dain `shouldReturn` False

  describe "Soldier of Erebor" do
    it "discards the top 3 cards of the deck to ready, once a round" . gameTest $ \pid -> do
      soldier <- putAllyIntoPlay pid Allies.soldierOfErebor
      deck <- deckSize pid
      run $ Exhaust soldier
      run $ ActionWindow pid
      chooseAbilityOf pid soldier
      isExhausted soldier `shouldReturn` False
      deckSize pid `shouldReturn` deck - 3
      passActions pid
      run $ Exhaust soldier
      run $ ActionWindow pid
      offersAbilityOf pid soldier `shouldReturn` False

  describe "Man the Walls" do
    it "lowers the cost of the first ally played this phase, which cannot quest this round" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource theodred Resource 2
      guard' <- putCardIntoHand pid Allies.guardOfTheCitadel
      another <- putCardIntoHand pid Events.manTheWalls
      _ <- playWith pid theodredCode 0 Events.manTheWalls
      offersCard pid another `shouldReturn` False
      chooseTarget pid (CardIdTarget guard'.id)
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 0
      Just ally' <- allyWithCode Allies.guardOfTheCitadel
      getModifiers ally' >>= (`shouldSatisfy` elem CannotCommitToQuest)

  describe "Mirkwood Hunter" do
    it "costs 1 less for each attachment on the active location" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.forestGate
      run $ PlaceLocation lid ActiveLocation
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      _ <- playWith pid beravorCode 2 Allies.mirkwoodHunter
      allyWithCode Allies.mirkwoodHunter >>= (`shouldSatisfy` isJust)
      (getHeroResources . toId =<< getHeroWithCode beravorCode) `shouldReturn` 0

  describe "Familiar Lands" do
    it "gives each location with a player card attachment -2 threat for the phase" . gameTest $ \pid -> do
      gate <- stagedLocation Locations.forestGate
      mountains <- stagedLocation Locations.mountainsOfMirkwood
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget gate)
      card <- putCardIntoHand pid Events.familiarLands
      beravor <- hero beravorCode
      run $ PlaceTokens GameSource beravor Resource 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      getLocationThreat gate `shouldReturn` 0
      getLocationThreat mountains `shouldReturn` 2

  describe "Warrior Sword" do
    it "gives +1 attack for each enemy engaged with you, up to +3" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      _ <- putAttachmentOn pid Attachments.warriorSword theodred
      getAttack theodred `shouldReturn` 2
      _ <- engagedEnemy pid Enemies.forestSpider
      getAttack theodred `shouldReturn` 3
      for_ [1 .. 3 :: Int] \_ -> engagedEnemy pid Enemies.forestSpider
      getAttack theodred `shouldReturn` 5

  describe "Swift and Strong" do
    it "exhausts a Weapon to ready its character after it destroys an enemy, +2 attack for its next attack" . gameTestWith (deckList [legolasCode, eowynCode, theodredCode] testDeck.cards) $ \pid -> do
      theodred <- hero theodredCode
      sword <- putAttachmentOn pid Attachments.warriorSword theodred
      legolas <- hero legolasCode
      run $ PlaceTokens GameSource legolas Resource 1
      card <- putCardIntoHand pid Events.swiftAndStrong
      run $ Exhaust theodred
      eid <- engagedEnemy pid Enemies.forestSpider
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [theodred] mempty]
      chooseTarget pid (CardIdTarget card.id)
      isExhausted theodred `shouldReturn` False
      isExhausted sword `shouldReturn` True
      getModifiers theodred >>= (`shouldSatisfy` elem (ForNextAttack $ AttackModifier 2))

  describe "Descendant of Girion" do
    it "lowers the cost of Item attachments played on him by 1" . gameTest $ \pid -> do
      descendant <- putAllyIntoPlay pid Allies.descendantOfGirion
      getModifiers descendant >>= (`shouldSatisfy` elem (ReduceCostToAttach (CardWithTrait Item) 1))

  describe "Valiant Determination" do
    it "keeps its ally from exhausting to quest" . gameTest $ \pid -> do
      scout <- putAllyIntoPlay pid Allies.snowbournScout
      _ <- putAttachmentOn pid Attachments.valiantDetermination scout
      run $ CommitToQuest scout
      isExhausted scout `shouldReturn` False

  describe "Ring of Thrór" do
    it "exhausts and discards the top card to ready its hero, putting a discarded attachment into play" . dainTest $ \pid -> do
      dain <- hero dainCode
      ring <- putAttachmentOn pid Attachments.ringOfThror dain
      onTopOfDeck pid Attachments.bootsFromErebor
      run $ Exhaust dain
      run $ ActionWindow pid
      chooseAbilityOf pid ring
      isExhausted dain `shouldReturn` False
      isExhausted ring `shouldReturn` True
      attachmentPlacements Attachments.bootsFromErebor `shouldReturn` [AttachedTo dain]

    it "leaves a discarded card that isn't an attachment in the discard pile" . dainTest $ \pid -> do
      dain <- hero dainCode
      ring <- putAttachmentOn pid Attachments.ringOfThror dain
      onTopOfDeck pid Allies.guardOfTheCitadel
      run $ ActionWindow pid
      chooseAbilityOf pid ring
      any ((== toCardCode Allies.guardOfTheCitadel) . toCardCode) . (.discard) <$> getPlayer pid `shouldReturn` True
