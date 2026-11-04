module LordOfTheRings.Haradrim.TheSandsOfHaradPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (PlayerCard)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (DoesNotExhaustToDefend))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (CommittedToQuest, DeclaredAsDefender), mkAfter)
import TestImport

gimliCode, legolasCode, beravorCode, theodredCode :: CardCode
gimliCode = "16001"
legolasCode = "16002"
beravorCode = "01012"
theodredCode = "01002"

-- | Gimli, Legolas and Théodred, with the test deck's cards.
friends :: DeckList
friends = deckList [gimliCode, legolasCode, theodredCode] testDeck.cards

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

resources :: CardCode -> GameT Int
resources code = getHeroResources . toId =<< getHeroWithCode code

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  existing <- getsGame (Map.keys . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  eid <- getsGame (fromJustNote "no enemy" . find (`notElem` existing) . Map.keys . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

-- | A card put straight into the victory display, its modifiers read again.
intoVictoryDisplay :: Card -> GameT ()
intoVictoryDisplay card = do
  putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (card :))) =<< getGame
  run $ CheckWindows []

encounterSideQuestInVictoryDisplay :: GameT ()
encounterSideQuestInVictoryDisplay = intoVictoryDisplay . EncounterCard =<< genEncounterCard Quests.orcRearguard

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

-- | A player side quest of the player's put into the staging area.
sideQuestInPlay :: PlayerId -> CardDef -> GameT (PlayerCard, QuestId)
sideQuestInPlay pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PutSideQuestIntoPlay pid card
  q <- getsGame (toId . fromJustNote "no side quest" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.quests))
  pure (card, q)

-- | Whether the open question offers to play the card from hand.
offered :: Card -> GameT Bool
offered card = do
  lead <- getLead
  maybe False (any \case TargetLabel (CardIdTarget cid) _ -> cid == card.id; _ -> False) . fmap questionChoices <$> getQuestion lead

spec :: Spec
spec = describe "The Sands of Harad player cards" do
  describe "Gimli" do
    it "spends a resource as he defends to ready Legolas, who gets +1 attack" . gameTestWith friends $ \pid -> do
      gimli <- hero gimliCode
      legolas <- hero legolasCode
      eid <- engagedEnemy pid Enemies.forestSpider
      run $ Exhaust legolas
      give gimliCode 1
      run $ CheckWindows [mkAfter $ DeclaredAsDefender gimli eid]
      chooseAbilityOf pid gimli
      heroExhausted . toAttrs <$> getHeroWithCode legolasCode `shouldReturn` False
      getAttack legolas `shouldReturn` 4
      resources gimliCode `shouldReturn` 0

  describe "Legolas" do
    it "discards a card as he commits to ready Gimli, who gets +1 willpower" . gameTestWith friends $ \pid -> do
      gimli <- hero gimliCode
      legolas <- hero legolasCode
      card <- putCardIntoHand pid Allies.veteranAxehand
      run $ Exhaust gimli
      run $ CheckWindows [mkAfter $ CommittedToQuest legolas]
      chooseAbilityOf pid legolas
      chooseTarget pid (CardIdTarget card.id)
      heroExhausted . toAttrs <$> getHeroWithCode gimliCode `shouldReturn` False
      getWillpower gimli `shouldReturn` 3
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [card.id]

  describe "Halfling Bounder" do
    it "cancels a revealed card's when revealed effects while a side quest is in the victory display" . gameTest $ \pid -> do
      bounder <- putAllyIntoPlay pid Allies.halflingBounder
      encounterSideQuestInVictoryDisplay
      beravor <- hero beravorCode
      run $ Exhaust beravor
      _ <- setEncounterDeck [Treacheries.theNecromancersReach]
      run RevealEncounterCard
      chooseAbilityOf pid bounder
      getCharacterDamage beravor `shouldReturn` 0
      getsGame (null . (.entities.allies)) `shouldReturn` True

    it "has no response without a side quest in the victory display" . gameTest $ \pid -> do
      bounder <- putAllyIntoPlay pid Allies.halflingBounder
      beravor <- hero beravorCode
      run $ Exhaust beravor
      _ <- setEncounterDeck [Treacheries.theNecromancersReach]
      run RevealEncounterCard
      offersAbilityOf pid bounder `shouldReturn` False

  describe "Vigilant Dúnadan" do
    it "does not exhaust to defend only while a side quest is in the victory display" . gameTest $ \pid -> do
      dunadan <- putAllyIntoPlay pid Allies.vigilantDunadan
      getModifiers dunadan >>= (`shouldSatisfy` notElem DoesNotExhaustToDefend)
      encounterSideQuestInVictoryDisplay
      getModifiers dunadan >>= (`shouldSatisfy` elem DoesNotExhaustToDefend)

  describe "Unlikely Friendship" do
    it "draws a card and adds a resource, with a unique Silvan and a unique Dwarf" . gameTestWith friends $ \pid -> do
      was <- length . (.hand) <$> getPlayer pid
      _ <- playWith pid theodredCode 0 Events.unlikelyFriendship
      chooseTarget pid =<< hero gimliCode
      resources gimliCode `shouldReturn` 1
      length . (.hand) <$> getPlayer pid `shouldReturn` was + 1

  describe "Well Warned" do
    it "lowers the engaging player's threat by the enemy's printed threat" . gameTestWith friends $ \pid -> do
      card <- putCardIntoHand pid Events.wellWarned
      putGame . (playersL . ix pid . threatL .~ 30) =<< getGame
      _ <- engagedEnemy pid Enemies.forestSpider
      chooseTarget pid (CardIdTarget card.id)
      (.threat) <$> getPlayer pid `shouldReturn` 28

  describe "Dour-handed" do
    -- Gimli (core) pays for it
    it "deals damage for each side quest in the victory display" . gameTestWith (deckList ["01004", beravorCode, theodredCode] testDeck.cards) $ \pid -> do
      eid <- engagedEnemy pid Enemies.forestSpider
      encounterSideQuestInVictoryDisplay
      intoVictoryDisplay . PlayerCard =<< genPlayerCard Quests.theStormComes (Just pid)
      card <- putCardIntoHand pid Events.dourHanded
      give "01004" 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      getCharacterDamage (toTarget eid) `shouldReturn` 2

  describe "Dwarven Shield" do
    it "gives +1 defense, and a resource once an enemy's attack damages its hero" . gameTestWith friends $ \pid -> do
      gimli <- hero gimliCode
      shield <- putAttachmentOn pid Attachments.dwarvenShield gimli
      getDefense gimli `shouldReturn` 3
      eid <- engagedEnemy pid Enemies.forestSpider
      run $ DealDamage (EnemyAttackSource eid) gimli 1
      chooseAbilityOf pid shield
      resources gimliCode `shouldReturn` 1

    it "has no response to damage from anything but an attack" . gameTestWith friends $ \pid -> do
      gimli <- hero gimliCode
      shield <- putAttachmentOn pid Attachments.dwarvenShield gimli
      run $ DealDamage GameSource gimli 1
      offersAbilityOf pid shield `shouldReturn` False

  describe "The Road Goes Ever On" do
    it "has a player search his deck for a side quest when its quest is defeated" . gameTest $ \pid -> do
      (_, q) <- sideQuestInPlay pid Quests.theStormComes
      road <- putAttachmentOn pid Attachments.theRoadGoesEverOn (QuestTarget q)
      wanted <- PlayerCard <$> genPlayerCard Quests.theStormComes (Just pid)
      putGame . (playersL . ix pid . deckL %~ (wanted :)) =<< getGame
      run $ CompleteQuest q
      chooseAbilityOf pid road
      chooseTarget pid (CardIdTarget wanted.id)
      elem wanted.id . map (.id) . (.hand) <$> getPlayer pid `shouldReturn` True

  describe "The Storm Comes" do
    it "lets the first ally each round be paid for without a resource match" . gameTest $ \pid -> do
      intoVictoryDisplay . PlayerCard =<< genPlayerCard Quests.theStormComes (Just pid)
      secondAlly <- putCardIntoHand pid Allies.veteranAxehand
      leadershipAlly <- putCardIntoHand pid Allies.guardOfTheCitadel
      give theodredCode 2
      firstAlly <- playWith pid theodredCode 2 Allies.veteranAxehand
      getsGame (any ((== firstAlly.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` True
      -- the planning window, asked again: a second tactics ally needs a tactics resource
      offered secondAlly `shouldReturn` False
      offered leadershipAlly `shouldReturn` True

    it "goes to its owner's discard pile when a copy is already in the victory display" . gameTest $ \pid -> do
      intoVictoryDisplay . PlayerCard =<< genPlayerCard Quests.theStormComes (Just pid)
      (card, q) <- sideQuestInPlay pid Quests.theStormComes
      run $ CompleteQuest q
      victoryDisplay `shouldReturn` [toCardCode Quests.theStormComes]
      map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [card.id]
