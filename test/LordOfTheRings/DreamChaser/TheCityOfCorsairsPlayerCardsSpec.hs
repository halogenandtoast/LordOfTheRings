module LordOfTheRings.DreamChaser.TheCityOfCorsairsPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getAttackStrength, getDefense)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CollectsAdditionalResources, GainTrait, LowerNextCostOf))
import LordOfTheRings.Phase
import LordOfTheRings.Trait (Trait (Outlands))
import TestImport

imrahilCode, beravorCode, eowynCode, theodredCode, legolasCode, leadershipImrahilCode :: CardCode
imrahilCode = "12138"
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
legolasCode = "01005"
leadershipImrahilCode = "02050"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- enemyIds
  run . AddToStaging =<< genEncounterCard def
  fromJustNote "no enemy" . find (`notElem` existing) <$> enemyIds

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  eid <- stagedEnemy def
  run $ EngageEnemy pid eid
  pure eid

placementOf :: EnemyId -> GameT Placement
placementOf eid = getsGame (maybe Unplaced (.placement) . Map.lookup eid . (.entities.enemies))

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

choices :: PlayerId -> GameT [Target]
choices pid = maybe [] (\q -> [t | TargetLabel t _ <- questionChoices q]) <$> getQuestion pid

spec :: Spec
spec = describe "The City of Corsairs player cards" do
  describe "Prince Imrahil (tactics)" do
    let deck = deckList [imrahilCode, beravorCode, theodredCode] testDeck.cards
    it "puts an ally sharing a trait with him into play from the top 5, shuffled back at the end of the phase" . gameTestWith deck $ \pid -> do
      imrahil <- hero imrahilCode
      guard' <- PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid)
      putGame . (playersL . ix pid . deckL %~ (guard' :)) =<< getGame
      putGame . (phaseL .~ CombatPhase) =<< getGame
      give imrahilCode 1
      run $ ActionWindow pid
      chooseAbilityOf pid imrahil
      chooseTarget pid (CardIdTarget guard'.id)
      getsGame (any ((== guard'.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` True
      putGame . (questionL .~ mempty) =<< getGame
      run $ EndPhase CombatPhase
      getsGame (any ((== guard'.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` False
      elem guard'.id . map (.id) . (.deck) <$> getPlayer pid `shouldReturn` True

  describe "Knight of the White Tower" do
    let deck = deckList [leadershipImrahilCode, beravorCode, theodredCode] testDeck.cards
    it "cannot be paid for from two heroes' pools" . gameTestWith deck $ \pid -> do
      knight <- putCardIntoHand pid Allies.knightOfTheWhiteTower
      give theodredCode 2
      give leadershipImrahilCode 2
      run $ BeginPhase PlanningPhase
      choices pid >>= (`shouldSatisfy` notElem (CardIdTarget knight.id))

    it "is paid for from a single hero's pool" . gameTestWith deck $ \pid -> do
      give leadershipImrahilCode 2
      _ <- playWith pid theodredCode 4 Allies.knightOfTheWhiteTower
      getsGame (length . (.entities.allies)) `shouldReturn` 1
      (`shouldBe` 0) =<< getHeroResources . toId =<< getHeroWithCode theodredCode
      (`shouldBe` 2) =<< getHeroResources . toId =<< getHeroWithCode leadershipImrahilCode

  describe "Hold the Line" do
    let deck = deckList [legolasCode, beravorCode, theodredCode] testDeck.cards
    it "lets 3 characters defend, and for 2 tactics readies those that take no damage" . gameTestWith deck $ \pid -> do
      eid <- engagedEnemy pid Enemies.hillTroll
      card <- putCardIntoHand pid Events.holdTheLine
      give theodredCode 1
      give legolasCode 2
      run $ EnemyAttack pid eid
      chooseTarget pid (CardIdTarget card.id)
      chooseLabel "$label.groupSpend" pid
      chooseTarget pid =<< hero legolasCode
      chooseTarget pid =<< hero legolasCode
      passActions pid
      chooseTarget pid =<< hero beravorCode
      chooseTarget pid =<< hero theodredCode
      chooseTarget pid =<< hero legolasCode
      choices pid `shouldReturn` []
      chooseLabel "$label.doneDeclaringDefenders" pid
      passActions pid
      chooseTarget pid =<< hero beravorCode
      passActions pid
      (`shouldBe` 2) =<< getCharacterDamage =<< hero beravorCode
      exhausted beravorCode `shouldReturn` True
      exhausted theodredCode `shouldReturn` False
      exhausted legolasCode `shouldReturn` False

  describe "Soldier of Dol Amroth" do
    it "lowers the cost of your next tactics card this phase by 1" . gameTest $ \pid -> do
      soldier <- putAllyIntoPlay pid Allies.soldierOfDolAmroth
      chooseAbilityOf pid soldier
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` any (\case LowerNextCostOf _ 1 -> True; _ -> False))

  describe "Súlien" do
    it "spends 1 lore to give each location in the staging area -1 threat this phase" . gameTest $ \pid -> do
      sulien <- putAllyIntoPlay pid Allies.sulien
      lid <- do
        run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
        getsGame (fromJustNote "no location" . headMay . Map.keys . (.entities.locations))
      give beravorCode 1
      run $ ActionWindow pid
      chooseAbilityOf pid sulien
      getLocationThreat lid `shouldReturn` 0

  describe "Inspiring Presence" do
    it "gives heroes of lower threat cost +2 defense, and +2 attack for 2 leadership" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Events.inspiringPresence
      give eowynCode 2
      give theodredCode 2
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid =<< hero beravorCode
      chooseLabel "$label.groupSpend" pid
      chooseTarget pid =<< hero theodredCode
      chooseTarget pid =<< hero theodredCode
      theodred <- hero theodredCode
      getDefense theodred `shouldReturn` 3
      getAttack theodred `shouldReturn` 4
      (`shouldBe` 2) =<< getDefense =<< hero beravorCode

  describe "Guardian of Ithilien" do
    it "returns an enemy engaged with you to the staging area" . gameTest $ \pid -> do
      eid <- engagedEnemy pid Enemies.forestSpider
      guardian <- putAllyIntoPlay pid Allies.guardianOfIthilien
      chooseAbilityOf pid guardian
      placementOf eid `shouldReturn` StagingArea

  describe "Ranger Spear" do
    it "gives +1 attack, +2 against an enemy with an attachment" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.rangerSpear beravor
      getAttack beravor `shouldReturn` 3
      bare <- engagedEnemy pid Enemies.forestSpider
      snared <- engagedEnemy pid Enemies.forestSpider
      _ <- putAttachmentOn pid Attachments.forestSnare (toTarget snared)
      getAttackStrength [beravor] bare Enemies.forestSpider `shouldReturn` 3
      getAttackStrength [beravor] snared Enemies.forestSpider `shouldReturn` 4

  describe "Prince of Dol Amroth" do
    let deck = deckList [imrahilCode, beravorCode, theodredCode] testDeck.cards
    it "gives Imrahil the Outlands trait, and a resource more with Outlands allies of 4 spheres" . gameTestWith deck $ \pid -> do
      imrahil <- hero imrahilCode
      _ <- putAttachmentOn pid Attachments.princeOfDolAmroth imrahil
      getModifiers imrahil >>= (`shouldSatisfy` elem (GainTrait Outlands))
      getModifiers imrahil >>= (`shouldSatisfy` notElem (CollectsAdditionalResources 1))
      for_ [Allies.warriorOfLossarnach, Allies.knightsOfTheSwan, Allies.ethirSwordsman, Allies.anfalasHerdsman] (putAllyIntoPlay pid)
      getModifiers imrahil >>= (`shouldSatisfy` elem (CollectsAdditionalResources 1))
