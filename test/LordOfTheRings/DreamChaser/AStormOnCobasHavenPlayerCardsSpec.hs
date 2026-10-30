module LordOfTheRings.DreamChaser.AStormOnCobasHavenPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Modifier (ModifierType (CannotAttackOrDefend, DoesNotExhaustToCommit, DrawsOnDestroyingEnemy))
import LordOfTheRings.Phase
import TestImport

naasiyahCode, beravorCode, eowynCode, theodredCode, dunhereCode :: CardCode
naasiyahCode = "12109"
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
dunhereCode = "01009"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

resources :: CardCode -> GameT Int
resources code = getHeroResources . toId =<< getHeroWithCode code

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  existing <- enemyIds
  run . AddToStaging =<< genEncounterCard def
  eid <- fromJustNote "no enemy" . find (`notElem` existing) <$> enemyIds
  run $ EngageEnemy pid eid
  pure eid

placementOf :: EnemyId -> GameT Placement
placementOf eid = getsGame (maybe Unplaced (.placement) . Map.lookup eid . (.entities.enemies))

committed :: CardCode -> GameT Bool
committed code = heroCommitted . toAttrs <$> getHeroWithCode code

intoDiscard :: PlayerId -> CardDef -> GameT Card
intoDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . discardL %~ (card :)) =<< getGame
  pure card

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

choices :: PlayerId -> GameT [Target]
choices pid = maybe [] (\q -> [t | TargetLabel t _ <- questionChoices q]) <$> getQuestion pid

spec :: Spec
spec = describe "A Storm on Cobas Haven player cards" do
  describe "Na'asiyah" do
    let deck = deckList [naasiyahCode, beravorCode, theodredCode] testDeck.cards
    it "cannot pay for allies" . gameTestWith deck $ \pid -> do
      ally <- putCardIntoHand pid Allies.veteranSwordElf
      give naasiyahCode 3
      run $ BeginPhase PlanningPhase
      choices pid >>= (`shouldSatisfy` notElem (CardIdTarget ally.id))

    it "pays 1 resource while defending for +2 defense for this attack" . gameTestWith deck $ \pid -> do
      naasiyah <- hero naasiyahCode
      eid <- engagedEnemy pid Enemies.hillTroll
      give naasiyahCode 1
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid naasiyah
      chooseAbilityOf pid naasiyah
      chooseLabel "$label.naasiyah.defense" pid
      passActions pid
      getCharacterDamage naasiyah `shouldReturn` 2
      resources naasiyahCode `shouldReturn` 0

  describe "Rune-master" do
    it "exhausts to add 1 resource to a hero a Signal attachment is attached to" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      beravor <- hero beravorCode
      runeMaster <- putAttachmentOn pid Attachments.runeMaster theodred
      _ <- playWith pid theodredCode 0 Attachments.dunedainRemedy
      chooseTarget pid beravor
      chooseAbilityOf pid runeMaster
      resources beravorCode `shouldReturn` 1

  describe "Terrible to Behold" do
    it "returns the enemy a Noble defends against to the staging area and cancels its attack" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      eid <- engagedEnemy pid Enemies.hillTroll
      card <- putCardIntoHand pid Events.terribleToBehold
      give theodredCode 1
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid theodred
      chooseTarget pid (CardIdTarget card.id)
      passActions pid
      placementOf eid `shouldReturn` StagingArea
      getCharacterDamage theodred `shouldReturn` 0

  describe "Veteran Sword-elf" do
    it "gets +1 attack and defense for each copy in your discard pile" . gameTest $ \pid -> do
      _ <- intoDiscard pid Allies.veteranSwordElf
      _ <- intoDiscard pid Allies.veteranSwordElf
      elf <- putAllyIntoPlay pid Allies.veteranSwordElf
      getAttack elf `shouldReturn` 3

  describe "Vigilant Guard" do
    it "gives +2 hit points and takes 1 of the damage another character would be dealt" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      beravor <- hero beravorCode
      guard' <- putAttachmentOn pid Attachments.vigilantGuard theodred
      getHitPoints theodred `shouldReturn` 6
      run $ DealDamage GameSource beravor 3
      chooseAbilityOf pid guard'
      getCharacterDamage beravor `shouldReturn` 2
      getCharacterDamage theodred `shouldReturn` 1

  describe "Knife-work" do
    let deck = deckList [naasiyahCode, beravorCode, theodredCode] testDeck.cards
    it "gives engaged enemies -1 defense, and for 2 lore draws a card for each enemy destroyed" . gameTestWith deck $ \pid -> do
      naasiyah <- hero naasiyahCode
      beravor <- hero beravorCode
      eid <- engagedEnemy pid Enemies.forestSpider
      card <- putCardIntoHand pid Events.knifeWork
      give naasiyahCode 1
      give beravorCode 2
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseLabel "$label.groupSpend" pid
      chooseTarget pid beravor
      chooseTarget pid beravor
      getEnemyDefense eid `shouldReturn` 0
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem DrawsOnDestroyingEnemy)
      passActions pid
      run $ PlaceTokens GameSource (toTarget eid) Damage 3
      hand <- handSize pid
      run $ AttackWith naasiyah
      run $ ResolveAttack pid eid
      handSize pid `shouldReturn` hand + 1

  describe "Linhir Sea-captain" do
    it "and the hero who paid for it alone do not exhaust to quest this round" . gameTest $ \pid -> do
      _ <- playWith pid eowynCode 3 Allies.linhirSeaCaptain
      linhir <- getsGame (toTarget . fromJustNote "no ally" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid linhir
      eowyn <- hero eowynCode
      getModifiers eowyn >>= (`shouldSatisfy` elem DoesNotExhaustToCommit)
      getModifiers linhir >>= (`shouldSatisfy` elem DoesNotExhaustToCommit)

    let spirited = deckList [eowynCode, dunhereCode, theodredCode] testDeck.cards
    it "has nothing to answer when two heroes paid for it" . gameTestWith spirited $ \pid -> do
      give dunhereCode 2
      _ <- playWith pid eowynCode 1 Allies.linhirSeaCaptain
      chooseTarget pid =<< hero dunhereCode
      chooseTarget pid =<< hero eowynCode
      linhir <- getsGame (toTarget . fromJustNote "no ally" . headMay . toList . (.entities.allies))
      offersAbilityOf pid linhir `shouldReturn` False

  describe "Windfola" do
    it "gives +1 willpower and commits its character again once removed from the quest" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      windfola <- putAttachmentOn pid Attachments.windfola eowyn
      getWillpower eowyn `shouldReturn` 5
      run $ CommitToQuest eowyn
      run $ RemoveFromQuest eowyn
      chooseAbilityOf pid windfola
      committed eowynCode `shouldReturn` True

  describe "Ioreth" do
    it "cannot attack or defend, and any player may spend 1 lore and exhaust her to heal 3" . gameTest $ \pid -> do
      ioreth <- putAllyIntoPlay pid Allies.ioreth
      getModifiers ioreth >>= (`shouldSatisfy` elem CannotAttackOrDefend)
      theodred <- hero theodredCode
      run $ PlaceTokens GameSource theodred Damage 3
      give beravorCode 1
      run $ ActionWindow pid
      chooseAbilityOf pid ioreth
      getCharacterDamage theodred `shouldReturn` 0
      resources beravorCode `shouldReturn` 0

  describe "The Houses of Healing" do
    it "costs 1 less for each Healer exhausted, and puts a hero from a discard pile into play with 1 damage" . gameTest $ \pid -> do
      ioreth <- putAllyIntoPlay pid Allies.ioreth
      legolas <- intoDiscard pid Heroes.legolas
      card <- putCardIntoHand pid Events.theHousesOfHealing
      give beravorCode 4
      putGame . (phaseL .~ RefreshPhase) =<< getGame
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid ioreth
      resources beravorCode `shouldReturn` 0
      back <- getsGame (find ((== legolas.id) . heroCardId . toAttrs) . toList . (.entities.heroes))
      fmap (.damage) back `shouldBe` Just 1

  describe "Justice Shall Be Done" do
    it "draws 3 and gives each hero 3 resources, then eliminates you at the end of the round" . gameTest $ \pid -> do
      hand <- handSize pid
      card <- playWith pid theodredCode 0 Events.justiceShallBeDone
      handSize pid `shouldReturn` hand + 3
      resources beravorCode `shouldReturn` 3
      getsGame (elem card.id . map (.id) . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` True
      -- the planning window set aside, the round ends
      putGame . (questionL .~ mempty) =<< getGame
      run EndRound
      (.eliminated) <$> getPlayer pid `shouldReturn` True
