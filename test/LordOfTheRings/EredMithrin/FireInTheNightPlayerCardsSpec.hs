module LordOfTheRings.EredMithrin.FireInTheNightPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotAttackOrDefend, GainResourceIcon, ShuffleIntoDeckAtEndOfPhase))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Sphere (Sphere (Lore, Tactics))
import LordOfTheRings.Window (WindowType (DefendedAttack, EnemyDestroyedByAttack), mkAfter)
import TestImport

thranduilCode, eowynCode, theodredCode, beravorCode, eleanorCode, thalionCode :: CardCode
thranduilCode = "19056"
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"
eleanorCode = "01008"
thalionCode = "19065"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

thranduilTest :: (PlayerId -> GameT ()) -> IO ()
thranduilTest = gameTestWith (deckList [thranduilCode, eowynCode, theodredCode] testDeck.cards)

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- enemyIds
  run . AddToStaging =<< genEncounterCard def
  fromJustNote "no enemy" . find (`notElem` existing) <$> enemyIds

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  existing <- getsGame (Map.keys . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . find (`notElem` existing) . Map.keys . (.entities.locations))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  AttachmentTarget aid -> maybe False (attachmentExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.attachments)
  _ -> pure False

inHand :: PlayerId -> CardDef -> GameT Bool
inHand pid def = any ((== toCardCode def) . toCardCode) . (.hand) <$> getPlayer pid

allyWithCode :: CardDef -> GameT (Maybe Target)
allyWithCode def = getsGame (fmap toTarget . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

defeatSideQuest :: PlayerId -> CardDef -> GameT ()
defeatSideQuest pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PutSideQuestIntoPlay pid card
  qid <- getsGame (fromJustNote "no side quest" . fmap toId . find ((== card.id) . questCardId . toAttrs) . toList . (.entities.quests))
  run $ CompleteQuest qid

passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

spec :: Spec
spec = describe "The Fire in the Night player cards" do
  describe "Thranduil" do
    it "plays a Silvan ally in the combat phase, needing no resource match" . thranduilTest $ \pid -> do
      thranduil <- hero thranduilCode
      run $ PlaceTokens GameSource thranduil Resource 3
      _ <- putCardIntoHand pid Allies.daughterOfTheNimrodel
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ ActionWindow pid
      chooseAbilityOf pid thranduil
      allyWithCode Allies.daughterOfTheNimrodel >>= (`shouldSatisfy` isJust)
      (getHeroResources . toId =<< getHeroWithCode thranduilCode) `shouldReturn` 0

    it "is not offered outside the combat phase" . thranduilTest $ \pid -> do
      thranduil <- hero thranduilCode
      run $ PlaceTokens GameSource thranduil Resource 3
      _ <- putCardIntoHand pid Allies.daughterOfTheNimrodel
      run $ ActionWindow pid
      offersAbilityOf pid thranduil `shouldReturn` False

  describe "The Elvenking" do
    it "exhausts and returns a Silvan ally to hand to ready its hero" . thranduilTest $ \pid -> do
      thranduil <- hero thranduilCode
      elvenking <- putAttachmentOn pid Attachments.theElvenking thranduil
      _ <- putAllyIntoPlay pid Allies.daughterOfTheNimrodel
      run $ Exhaust thranduil
      run $ ActionWindow pid
      chooseAbilityOf pid elvenking
      isExhausted thranduil `shouldReturn` False
      isExhausted elvenking `shouldReturn` True
      inHand pid Allies.daughterOfTheNimrodel `shouldReturn` True

  describe "Orcrist" do
    it "gives +2 attack, and a resource after its hero destroys an enemy" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      orcrist <- putAttachmentOn pid Attachments.orcrist beravor
      getAttack beravor `shouldReturn` 4
      eid <- stagedEnemy Enemies.forestSpider
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [beravor] mempty]
      chooseAbilityOf pid orcrist
      (getHeroResources . toId =<< getHeroWithCode beravorCode) `shouldReturn` 1

  describe "Forest Road Traveler" do
    it "gets +1 willpower, attack and defense for each attachment on the active location" . gameTest $ \pid -> do
      traveler <- putAllyIntoPlay pid Allies.forestRoadTraveler
      lid <- stagedLocation Locations.forestGate
      run $ PlaceLocation lid ActiveLocation
      getWillpower traveler `shouldReturn` 1
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      _ <- putAttachmentOn pid Attachments.woodmensPath (LocationTarget lid)
      getWillpower traveler `shouldReturn` 3
      getAttack traveler `shouldReturn` 3
      getDefense traveler `shouldReturn` 3

  describe "Leather Boots" do
    it "exhausts to ready its character after a location is revealed" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      boots <- putAttachmentOn pid Attachments.leatherBoots beravor
      run $ Exhaust beravor
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run RevealEncounterCard
      chooseAbilityOf pid boots
      isExhausted beravor `shouldReturn` False
      isExhausted boots `shouldReturn` True

  describe "Giant Bear" do
    it "readies itself once a phase, to be shuffled into its deck at the end of the phase" . gameTest $ \pid -> do
      bear <- putAllyIntoPlay pid Allies.giantBear
      run $ Exhaust bear
      run $ ActionWindow pid
      chooseAbilityOf pid bear
      isExhausted bear `shouldReturn` False
      getModifiers bear >>= (`shouldSatisfy` elem ShuffleIntoDeckAtEndOfPhase)
      passActions pid
      run $ Exhaust bear
      run $ ActionWindow pid
      offersAbilityOf pid bear `shouldReturn` False

  describe "Outmatched" do
    it "readies the defender after its enemy's attack resolves" . gameTest $ \pid -> do
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      outmatched <- putAttachmentOn pid Attachments.outmatched (EnemyTarget eid)
      beravor <- hero beravorCode
      run $ Exhaust beravor
      run $ CheckWindows [mkAfter $ DefendedAttack beravor eid]
      chooseAbilityOf pid outmatched
      isExhausted beravor `shouldReturn` False

  describe "Galion" do
    it "cannot attack or defend, and gets +1 willpower for the round as he enters play" . gameTest $ \pid -> do
      galion <- putAllyIntoPlay pid Allies.galion
      chooseAbilityOf pid galion
      getWillpower galion `shouldReturn` 1
      getModifiers galion >>= (`shouldSatisfy` elem CannotAttackOrDefend)

  describe "Quicker Than Sight" do
    it "returns a Silvan ally to hand to cancel a shadow effect" . gameTestWith (coreDeck "spirit") $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.lorienGuide
      passResponses pid
      card <- putCardIntoHand pid Events.quickerThanSight
      _ <- setEncounterDeck [Enemies.forestSpider]
      run RevealEncounterCard
      eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Enemies.kingSpider]
      run $ BeginPhase CombatPhase
      passActions pid
      chooseTarget pid =<< hero eleanorCode
      passActions pid
      chooseTarget pid (CardIdTarget card.id)
      inHand pid Allies.lorienGuide `shouldReturn` True
      passActions pid
      -- King Spider's shadow would have the defending player exhaust a character
      Just (QuestionLabel prompt _ _ _) <- getQuestion pid
      prompt `shouldBe` "$label.declareAttackers"

  describe "Thalion" do
    it "readies at the beginning of the combat phase with a side quest in the victory display" . gameTest $ \pid -> do
      thalion <- putAllyIntoPlay pid Allies.thalion
      run $ Exhaust thalion
      run $ BeginPhase CombatPhase
      isExhausted thalion `shouldReturn` True
      passActions pid
      defeatSideQuest pid Quests.keepWatch
      passResponses pid
      run $ BeginPhase CombatPhase
      isExhausted thalion `shouldReturn` False

    it "becomes a hero with the spheres' resource icons once 3 side quests are in the victory display" . gameTest $ \pid -> do
      thalion <- putAllyIntoPlay pid Allies.thalion
      defeatSideQuest pid Quests.keepWatch
      passResponses pid
      defeatSideQuest pid Quests.scoutAhead
      passResponses pid
      allyWithCode Allies.thalion >>= (`shouldSatisfy` isJust)
      defeatSideQuest pid Quests.delayTheEnemy
      passResponses pid
      allyWithCode Allies.thalion `shouldReturn` Nothing
      asHero <- hero thalionCode
      unAllyIdOf thalion `shouldBe` asHero
      mods <- getModifiers asHero
      mods `shouldSatisfy` elem (GainResourceIcon Tactics)
      mods `shouldSatisfy` elem (GainResourceIcon Lore)
 where
  unAllyIdOf = \case
    AllyTarget aid -> HeroTarget (HeroId $ unAllyId aid)
    t -> t
