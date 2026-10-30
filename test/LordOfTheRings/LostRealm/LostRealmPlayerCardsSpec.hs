module LordOfTheRings.LostRealm.LostRealmPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense, getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (DoesNotExhaustToCommit, LowerNextCostOf, ResolvingEnemyAttacks, WillpowerModifier))
import LordOfTheRings.Phase
import LordOfTheRings.Scenario.Types (encounterDeckL)
import LordOfTheRings.Window (WindowType (EnemyDestroyedByAttack), mkAfter)
import TestImport

aragornCode, beravorCode, theodredCode, halbaradCode :: CardCode
aragornCode = "09001"
beravorCode = "01012"
theodredCode = "01002"
halbaradCode = "09002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

-- | Aragorn (tactics), Beravor (lore, Dúnedain Ranger) and Théodred (leadership).
aragornTest :: (PlayerId -> GameT ()) -> IO ()
aragornTest = gameTestWith (deckList [aragornCode, beravorCode, theodredCode] testDeck.cards)

-- | Halbarad (leadership), Beravor and Théodred.
halbaradTest :: (PlayerId -> GameT ()) -> IO ()
halbaradTest = gameTestWith (deckList [halbaradCode, beravorCode, theodredCode] testDeck.cards)

enemyIds :: GameT [EnemyId]
enemyIds = getsGame (Map.keys . (.entities.enemies))

-- | The enemy card added to the staging area.
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

engagedWith :: EnemyId -> GameT Placement
engagedWith eid = getsGame (maybe Unplaced (.placement) . Map.lookup eid . (.entities.enemies))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  _ -> pure False

inHand :: PlayerId -> CardId -> GameT Bool
inHand pid cid = elem cid . map (.id) . (.hand) <$> getPlayer pid

allyWithCard :: CardId -> GameT (Maybe Target)
allyWithCard cid = getsGame (fmap toTarget . find ((== cid) . allyCardId . toAttrs) . toList . (.entities.allies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . headMay . Map.keys . (.entities.locations))

spec :: Spec
spec = describe "The Lost Realm player cards" do
  describe "Aragorn (tactics)" do
    it "gives each enemy engaged with you -1 defense" . aragornTest $ \pid -> do
      eid <- engagedEnemy pid Enemies.forestSpider
      getEnemyDefense eid `shouldReturn` 0

    it "engages an enemy not engaged with you after his attack destroys one" . aragornTest $ \pid -> do
      aragorn <- hero aragornCode
      destroyed <- engagedEnemy pid Enemies.blackForestBats
      other <- stagedEnemy Enemies.forestSpider
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack destroyed [aragorn] mempty]
      chooseAbilityOf pid aragorn
      engagedWith other `shouldReturn` EngagedWith pid

  describe "Halbarad" do
    it "does not exhaust to commit while you are engaged with an enemy" . halbaradTest $ \pid -> do
      halbarad <- hero halbaradCode
      getModifiers halbarad >>= (`shouldSatisfy` notElem DoesNotExhaustToCommit)
      _ <- engagedEnemy pid Enemies.blackForestBats
      getModifiers halbarad >>= (`shouldSatisfy` elem DoesNotExhaustToCommit)

    it "lets you optionally engage 1 additional enemy" . halbaradTest $ \pid -> do
      one <- stagedEnemy Enemies.forestSpider
      two <- stagedEnemy Enemies.dolGuldurOrcs
      three <- stagedEnemy Enemies.blackForestBats
      run $ OptionalEngagement pid
      chooseTarget pid one
      chooseTarget pid two
      engagedWith one `shouldReturn` EngagedWith pid
      engagedWith two `shouldReturn` EngagedWith pid
      -- no third question
      engagedWith three `shouldReturn` StagingArea
      getQuestion pid >>= (`shouldSatisfy` maybe True (notElem (toTarget three) . targetsOf))

  describe "Weather Hills Watchman" do
    it "adds a Signal card from the top 5 of your deck to your hand" . gameTest $ \pid -> do
      summons <- PlayerCard <$> genPlayerCard Events.rangerSummons (Just pid)
      putGame . (playersL . ix pid . deckL %~ (summons :)) =<< getGame
      watchman <- putAllyIntoPlay pid Allies.weatherHillsWatchman
      chooseAbilityOf pid watchman
      chooseTarget pid (CardIdTarget summons.id)
      inHand pid summons.id `shouldReturn` True

  describe "Dúnedain Hunter" do
    it "puts a non-unique enemy from the top 5 of the encounter deck into play engaged with you" . gameTest $ \pid -> do
      [louis, spider] <- setEncounterDeck [Enemies.louis, Enemies.forestSpider]
      _ <- putAllyIntoPlay pid Allies.dunedainHunter
      chooseTarget pid (CardIdTarget spider.id)
      [eid] <- enemyIds
      engagedWith eid `shouldReturn` EngagedWith pid
      map (.id) <$> getEncounterDeck `shouldReturn` [louis.id]

    it "is discarded when no enemy enters play" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.louis, Locations.oldForestRoad]
      _ <- putAllyIntoPlay pid Allies.dunedainHunter
      getsGame (length . (.entities.allies)) `shouldReturn` 0

  describe "Sarn Ford Sentry" do
    it "draws 1 card for each enemy engaged with you" . gameTest $ \pid -> do
      _ <- engagedEnemy pid Enemies.forestSpider
      _ <- engagedEnemy pid Enemies.blackForestBats
      sentry <- putAllyIntoPlay pid Allies.sarnFordSentry
      hand <- length . (.hand) <$> getPlayer pid
      chooseAbilityOf pid sentry
      length . (.hand) <$> getPlayer pid `shouldReturn` hand + 2

  describe "Warden of Annúminas" do
    it "gets +1 willpower for each enemy engaged with you" . gameTest $ \pid -> do
      warden <- putAllyIntoPlay pid Allies.wardenOfAnnuminas
      getWillpower warden `shouldReturn` 0
      _ <- engagedEnemy pid Enemies.forestSpider
      _ <- engagedEnemy pid Enemies.blackForestBats
      getWillpower warden `shouldReturn` 2

  describe "Ranger Summons and Ranger of the North" do
    let deck = deckList [aragornCode, beravorCode, theodredCode] (("09015", 3) : testDeck.cards)
    it "sets the Ranger of the North aside out of the deck" . gameTestWith deck $ \pid -> do
      player <- getPlayer pid
      length player.setAsideCards `shouldBe` 3
      filter ((== "09015") . toCardCode) player.deck `shouldBe` []

    it "shuffles one of your set-aside Rangers into the encounter deck, then leaves the game" . gameTestWith deck $ \pid -> do
      _ <- setEncounterDeck []
      card <- playWith pid theodredCode 1 Events.rangerSummons
      Just chosen <- headMay . (.setAsideCards) <$> getPlayer pid
      chooseTarget pid (CardIdTarget chosen.id)
      map (.id) <$> getEncounterDeck `shouldReturn` [chosen.id]
      elem card.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` False
      -- still its owner's until it is revealed
      length . (.setAsideCards) <$> getPlayer pid `shouldReturn` 3

    it "revealed, the Ranger of the North surges and joins a player, dealing 2 damage" . gameTestWith deck $ \pid -> do
      Just chosen <- headMay . (.setAsideCards) <$> getPlayer pid
      _ <- setEncounterDeck []
      run $ ShuffleIntoEncounterDeck (CardIdTarget chosen.id)
      [ranger] <- getEncounterDeck
      spider <- stagedEnemy Enemies.forestSpider
      road <- genEncounterCard Locations.oldForestRoad
      putGame . (scenarioL %~ overAttrs (encounterDeckL .~ [ranger, road])) =<< getGame
      run RevealEncounterCard
      resolveRevealed
      chooseTarget pid spider
      getCharacterDamage (toTarget spider) `shouldReturn` 2
      Just r <- allyWithCard ranger.id
      getsGame (fmap (\a -> (allyPlacement (toAttrs a), allyOwner (toAttrs a))) . find ((== r) . toTarget) . toList . (.entities.allies))
        `shouldReturn` Just (InPlayArea pid, pid)
      length . (.setAsideCards) <$> getPlayer pid `shouldReturn` 2
      -- its surge revealed the Old Forest Road
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "goes to its owner's discard pile when discarded from the encounter deck" . gameTestWith deck $ \pid -> do
      Just ranger <- headMay . (.setAsideCards) <$> getPlayer pid
      run $ ShuffleIntoEncounterDeck (CardIdTarget ranger.id)
      [card] <- filter ((== ranger.id) . (.id)) <$> getEncounterDeck
      run $ RemoveFromEncounterDeck card
      run $ AddToEncounterDiscard [card]
      getEncounterDiscard `shouldReturn` []
      elem ranger.id . map (.id) . (.discard) <$> getPlayer pid `shouldReturn` True
      length . (.setAsideCards) <$> getPlayer pid `shouldReturn` 2

  describe "Tireless Hunters" do
    it "engages an enemy not engaged with you before the enemy attacks, discarding a shadow card" . aragornTest $ \pid -> do
      eid <- stagedEnemy Enemies.forestSpider
      [road] <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      card <- putCardIntoHand pid Events.tirelessHunters
      hero aragornCode >>= \t -> run (PlaceTokens GameSource t Resource 1)
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      engagedWith eid `shouldReturn` EngagedWith pid
      map (.id) <$> getEncounterDiscard `shouldReturn` [road.id]

    it "cannot be played once the enemy attacks have begun" . aragornTest $ \pid -> do
      _ <- stagedEnemy Enemies.forestSpider
      card <- putCardIntoHand pid Events.tirelessHunters
      hero aragornCode >>= \t -> run (PlaceTokens GameSource t Resource 1)
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ CreateEffect $ Effect GameSource GameTarget [ResolvingEnemyAttacks] UntilEndOfPhase
      run $ ActionWindow pid
      getQuestion pid >>= (`shouldSatisfy` maybe True (notElem (CardIdTarget card.id) . targetsOf))

  describe "Expert Trackers" do
    it "exhausts a Ranger to place the engaged enemy's printed threat in progress on a location" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.mountainsOfMirkwood
      eid <- stagedEnemy Enemies.forestSpider
      card <- putCardIntoHand pid Events.expertTrackers
      run $ EngageEnemy pid eid
      chooseTarget pid (CardIdTarget card.id)
      beravor <- hero beravorCode
      isExhausted beravor `shouldReturn` True
      getLocation lid >>= \l -> l.progress `shouldBe` 2

  describe "Heir of Valandil" do
    it "lowers the next Dúnedain ally's cost by the enemies engaged with you" . aragornTest $ \pid -> do
      aragorn <- hero aragornCode
      heir <- putAttachmentOn pid Attachments.heirOfValandil aragorn
      _ <- engagedEnemy pid Enemies.forestSpider
      _ <- engagedEnemy pid Enemies.blackForestBats
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid heir
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` any (\case LowerNextCostOf _ 2 -> True; _ -> False))

  describe "Athelas" do
    it "heals all damage on a character, and may discard a Condition from it" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      theodred <- hero theodredCode
      athelas <- putAttachmentOn pid Attachments.athelas beravor
      run $ PlaceTokens GameSource theodred Damage 3
      run $ ActionWindow pid
      chooseAbilityOf pid athelas
      chooseTarget pid theodred
      getCharacterDamage theodred `shouldReturn` 0
      isExhausted beravor `shouldReturn` True
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

  describe "Secret Vigil" do
    it "gives -1 threat, and reduces each threat by the printed threat as its enemy is destroyed" . gameTest $ \pid -> do
      setThreat pid 30
      eid <- engagedEnemy pid Enemies.forestSpider
      vigil <- putAttachmentOn pid Attachments.secretVigil (toTarget eid)
      getEnemyThreat eid `shouldReturn` 1
      run $ DealDamage GameSource (toTarget eid) 4
      chooseAbilityOf pid vigil
      threat pid `shouldReturn` 28

  describe "Star Brooch" do
    it "gives +1 willpower that cannot be reduced while you are engaged" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.starBrooch beravor
      run $ CreateEffect $ Effect GameSource beravor [WillpowerModifier (-1)] UntilEndOfPhase
      getWillpower beravor `shouldReturn` 1
      _ <- engagedEnemy pid Enemies.forestSpider
      getWillpower beravor `shouldReturn` 3
 where
  targetsOf q = [t | TargetLabel t _ <- questionChoices q]
