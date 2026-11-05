-- | The player cards of The Land of Shadow, The Flame of the West and The Mountain of Fire.
module LordOfTheRings.LordOfTheRingsSaga.PlayerCardsBSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Keyword (Keyword (FirstPlayerGainsControl, Sentinel))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (Objective)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import TestImport

eowyn, theodred, beravor :: CardCode
eowyn = "01007"
theodred = "01002"
beravor = "01012"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | The modifiers on the card, once the game has read them again.
modifiersOf :: Target -> GameT [ModifierType]
modifiersOf t = run (Run []) >> getModifiers t

stat :: (Target -> GameT Int) -> Target -> GameT Int
stat f t = run (Run []) >> f t

give :: Target -> Int -> GameT ()
give h n = run (PlaceTokens GameSource h Resource n)

-- | The scenario's Fellowship hero, put into play under the player as the saga's setup
-- does, with the box's One Ring attached when there is one.
fellowship :: PlayerId -> CardDef -> Maybe CardDef -> GameT Target
fellowship pid def ring = do
  run . PutHeroIntoPlay pid =<< genPlayerCard def (Just pid)
  h <- heroOf pid (toCardCode def)
  for_ ring \r -> run . (`PutObjectiveIntoPlayAttached` h) =<< genEncounterCard r
  pure h

ringOf :: CardDef -> GameT Objective
ringOf def = getsGame (fromJustNote "no ring" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

staged :: CardDef -> GameT EnemyId
staged def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))
    <&> toId

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- staged def
  run $ EngageEnemy pid eid
  pure eid

heroCodes :: GameT [CardCode]
heroCodes = getsGame (map toCardCode . toList . (.entities.heroes))

allyCodes :: GameT [CardCode]
allyCodes = getsGame (map toCardCode . toList . (.entities.allies))

spec :: Spec
spec = describe "Land of Shadow, Flame of the West and Mountain of Fire player cards" do
  describe "The One Ring (The Land of Shadow)" do
    it "gives its hero FirstPlayerGainsControl, and loses the game as it leaves play" . gameTest $ \pid -> do
      frodo <- fellowship pid Heroes.frodoBagginsTheLandOfShadow (Just Objectives.theOneRingTheLandOfShadow)
      modifiersOf frodo >>= (`shouldSatisfy` elem (GainKeyword FirstPlayerGainsControl))
      ring <- ringOf Objectives.theOneRingTheLandOfShadow
      run $ Discard GameSource (toTarget ring)
      getsGame (.outcome) `shouldReturn` Just Lost

  describe "Frodo Baggins (The Land of Shadow)" do
    it "spends a fellowship resource and exhausts The One Ring for +2 willpower and +2 attack" . gameTest $ \pid -> do
      frodo <- fellowship pid Heroes.frodoBagginsTheLandOfShadow (Just Objectives.theOneRingTheLandOfShadow)
      give frodo 1
      run $ ActionWindow pid
      chooseAbilityOf pid frodo
      stat getWillpower frodo `shouldReturn` 4
      stat getAttack frodo `shouldReturn` 3
      (.exhausted) <$> ringOf Objectives.theOneRingTheLandOfShadow `shouldReturn` True
      getsGame (fmap (.resources) . Map.lookup (fromJustNote "hero" $ asHeroId frodo) . (.entities.heroes)) `shouldReturn` Just 0

    it "is not offered without The One Ring" . gameTest $ \pid -> do
      frodo <- fellowship pid Heroes.frodoBagginsTheLandOfShadow Nothing
      give frodo 1
      run $ ActionWindow pid
      offersAbilityOf pid frodo `shouldReturn` False

  describe "Frodo Baggins (The Mountain of Fire)" do
    it "exhausts The One Ring in the planning phase to add a resource to a hero's pool" . gameTest $ \pid -> do
      frodo <- fellowship pid Heroes.frodoBagginsTheMountainOfFire (Just Objectives.theOneRingTheMountainOfFire)
      th <- hero theodred
      putGame . (phaseL .~ PlanningPhase) =<< getGame
      run $ ActionWindow pid
      chooseAbilityOf pid frodo
      chooseTarget pid th
      getHeroResources (fromJustNote "hero" $ asHeroId th) `shouldReturn` 1
      (.exhausted) <$> ringOf Objectives.theOneRingTheMountainOfFire `shouldReturn` True

  describe "Aragorn (The Flame of the West)" do
    it "loses the game as he leaves play" . gameTest $ \pid -> do
      aragorn <- fellowship pid Heroes.aragornTheFlameOfTheWest Nothing
      run $ Discard GameSource aragorn
      getsGame (.outcome) `shouldReturn` Just Lost

    it "with Banner of Elendil gains sentinel, and the Banner raises the others questing with him" . gameTest $ \pid -> do
      aragorn <- fellowship pid Heroes.aragornTheFlameOfTheWest Nothing
      modifiersOf aragorn >>= (`shouldSatisfy` notElem (GainKeyword Sentinel))
      _ <- putAttachmentOn pid Attachments.bannerOfElendil aragorn
      modifiersOf aragorn >>= (`shouldSatisfy` \ms -> GainKeyword Sentinel `elem` ms && DoesNotExhaustToCommit `elem` ms)
      ew <- hero eowyn
      run $ CommitToQuest aragorn
      run $ CommitToQuest ew
      stat getWillpower ew `shouldReturn` 5
      stat getWillpower aragorn `shouldReturn` 2

  describe "Aragorn (The Mountain of Fire)" do
    it "spends a resource from his pool to ready himself" . gameTest $ \pid -> do
      aragorn <- fellowship pid Heroes.aragornTheMountainOfFire Nothing
      give aragorn 1
      run $ Exhaust aragorn
      run $ ActionWindow pid
      chooseAbilityOf pid aragorn
      getsGame (fmap (heroExhausted . toAttrs) . Map.lookup (fromJustNote "hero" $ asHeroId aragorn) . (.entities.heroes)) `shouldReturn` Just False

  describe "Prince Imrahil (ally)" do
    it "becomes a hero while a hero is in your discard pile, and an ally again once none is" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.princeImrahilSpirit
      run . Discard GameSource =<< hero theodred
      heroCodes >>= (`shouldSatisfy` elem (toCardCode Allies.princeImrahilSpirit))
      putGame . (playersL . ix pid . discardL .~ []) =<< getGame
      run (Run [])
      heroCodes >>= (`shouldSatisfy` notElem (toCardCode Allies.princeImrahilSpirit))
      allyCodes >>= (`shouldSatisfy` elem (toCardCode Allies.princeImrahilSpirit))

  describe "Halbarad" do
    it "costs 1 less for each enemy engaged with you, and raises their engagement cost by 10" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Allies.halbaradLeadership
      _ <- engaged pid Enemies.forestSpider
      orcs <- engaged pid Enemies.dolGuldurOrcs
      modifiersOf (CardIdTarget card.id) >>= (`shouldSatisfy` elem (CostModifier (-2)))
      _ <- putAllyIntoPlay pid Allies.halbaradLeadership
      modifiersOf (toTarget orcs) >>= (`shouldSatisfy` elem (EngagementCostModifier 10))

  describe "Ghân-buri-Ghân" do
    it "has the active location's threat as his willpower" . gameTest $ \pid -> do
      ghan <- putAllyIntoPlay pid Allies.ghanBuriGhan
      stat getWillpower ghan `shouldReturn` 0
      run . (`PutLocationIntoPlay` ActiveLocation) =<< genEncounterCard Locations.oldForestRoad
      stat getWillpower ghan `shouldReturn` 1

  describe "Skinbark" do
    it "enters play exhausted, and attacking an Orc alone ignores its defense" . gameTest $ \pid -> do
      skinbark <- putAllyIntoPlay pid Allies.skinbark
      getsGame (any (\a -> toTarget a == skinbark && allyExhausted (toAttrs a)) . toList . (.entities.allies)) `shouldReturn` True
      run $ Ready skinbark
      orcs <- engaged pid Enemies.dolGuldurOrcs
      run $ CreateEffect $ Effect GameSource (toTarget orcs) [DefenseModifier 2] UntilEndOfPhase
      -- the one enemy engaged is attacked without asking which
      run $ PlayerAttacks pid
      chooseTarget pid skinbark
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      -- 4 attack against 3 hit points, its 2 defense not counted
      getsGame (Map.member orcs . (.entities.enemies)) `shouldReturn` False

  describe "Gamling" do
    it "exhausts to return a Rohan ally you control discarded from play to your hand" . gameTest $ \pid -> do
      gamling <- putAllyIntoPlay pid Allies.gamling
      scout <- putAllyIntoPlay pid Allies.snowbournScout
      run $ Discard GameSource scout
      chooseAbilityOf pid gamling
      map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.snowbournScout))
      map toCardCode . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` notElem (toCardCode Allies.snowbournScout))

  describe "Friend of Friends" do
    it "raises each of two Hobbit heroes with one attached" . gameTestWith (deckList ["02025", "06056", beravor] testDeck.cards) $ \pid -> do
      frodo <- hero "02025"
      pippin <- hero "06056"
      _ <- putAttachmentOn pid Attachments.friendOfFriends frodo
      stat getWillpower frodo `shouldReturn` 2
      _ <- putAttachmentOn pid Attachments.friendOfFriends pippin
      stat getWillpower frodo `shouldReturn` 3
      stat getAttack pippin `shouldReturn` 2
      card <- putCardIntoHand pid Attachments.friendOfFriends
      modifiersOf (CardIdTarget card.id) >>= (`shouldSatisfy` elem CannotBePlayedFromHand)

  describe "Brace of Coneys" do
    it "goes to the victory display for two different effects" . gameTest $ \pid -> do
      _ <- fellowship pid Heroes.frodoBagginsTheLandOfShadow Nothing
      card <- putCardIntoHand pid Events.braceOfConeys
      run $ RaiseThreat pid 5
      threat <- (.threat) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget card.id)
      chooseLabel "$label.braceOfConeys.threat" pid
      chooseLabel "$label.braceOfConeys.draw" pid
      (.threat) <$> getPlayer pid `shouldReturn` threat - 2
      length . (.hand) <$> getPlayer pid `shouldReturn` 2
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Events.braceOfConeys]

    it "cannot be played without the Ring-bearer" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Events.braceOfConeys
      run $ BeginPhase PlanningPhase
      getQuestion pid >>= \q ->
        maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) (questionChoices <$> q) `shouldBe` False
 where
  asHeroId = \case
    HeroTarget hid -> Just hid
    _ -> Nothing
