module LordOfTheRings.Starter.RidersOfRohanSpec (spec) where

import LordOfTheRings.Ally (Ally)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy (Enemy, EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (Location, LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

eomerCode, hirgonCode, lothirielCode :: CardCode
eomerCode = "07001"
hirgonCode = "17055"
lothirielCode = "22027"

rohanTest :: (PlayerId -> GameT ()) -> IO ()
rohanTest = gameTestWith (fromJustNote "no deck" $ lookup "riders-of-rohan" starterDecks)

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

allies :: GameT [Ally]
allies = getsGame (toList . (.entities.allies))

allyCodes :: GameT [CardCode]
allyCodes = map toCardCode <$> allies

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

-- | An enemy card added to the staging area.
stageEnemy :: CardDef -> GameT Enemy
stageEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies))

engagedEnemy :: PlayerId -> CardDef -> GameT Enemy
engagedEnemy pid def = do
  e <- stageEnemy def
  run $ PlaceEnemy (toId e) (EngagedWith pid)
  pure e

enemyNow :: Enemy -> GameT (Maybe Enemy)
enemyNow e = getsGame (find ((== toId e) . toId) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT Location
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no location" . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

locationNow :: Location -> GameT Location
locationNow l = getsGame (fromJustNote "location gone" . find ((== toId l) . toId) . toList . (.entities.locations))

-- | Put cards on top of the player's deck.
onTopOfDeck :: PlayerId -> [CardDef] -> GameT [Card]
onTopOfDeck pid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (cards <>)
  pure cards

giveResources :: CardCode -> Int -> GameT ()
giveResources code n = do
  h <- hero code
  run $ PlaceTokens GameSource h Resource n

-- | Pass every response window left open.
passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

spec :: Spec
spec = describe "Riders of Rohan" do
  describe "Westfold Horse-breaker" do
    it "is discarded to ready a hero" . rohanTest $ \pid -> do
      breaker <- putAllyIntoPlay pid Allies.westfoldHorseBreaker
      eomer <- hero eomerCode
      run $ Exhaust eomer
      run $ ActionWindow pid
      chooseAbilityOf pid breaker
      chooseTarget pid eomer
      exhausted eomerCode `shouldReturn` False
      allyCodes `shouldReturn` []

  describe "Éomund" do
    it "readies all Rohan characters as he leaves play" . rohanTest $ \pid -> do
      eomund <- putAllyIntoPlay pid Allies.eomund
      run . Exhaust =<< hero eomerCode
      run $ Discard eomund
      chooseAbilityOf pid eomund
      exhausted eomerCode `shouldReturn` False

  describe "Escort from Edoras" do
    it "gets +2 willpower while committed and is discarded after the quest resolves" . rohanTest $ \pid -> do
      escort <- putAllyIntoPlay pid Allies.escortFromEdoras
      run $ CommitToQuest escort
      getWillpower escort `shouldReturn` 4
      run QuestResolution
      passResponses pid
      allyCodes `shouldReturn` []

  describe "The Riddermark's Finest" do
    it "is exhausted and discarded to place 2 progress on a location" . rohanTest $ \pid -> do
      location <- stageLocation Locations.oldForestRoad
      finest <- putAllyIntoPlay pid Allies.theRiddermarksFinest
      run $ ActionWindow pid
      chooseAbilityOf pid finest
      (.progress) <$> locationNow location `shouldReturn` 2
      allyCodes `shouldReturn` []

  describe "West Road Traveller" do
    it "switches the active location with one in the staging area after being played" . rohanTest $ \pid -> do
      active <- stageLocation Locations.oldForestRoad
      run $ PlaceLocation (toId active) ActiveLocation
      staged <- stageLocation Locations.mountainsOfMirkwood
      traveller <- playWith pid lothirielCode 2 Allies.westRoadTraveller
      chooseAbilityOf pid =<< getsGame (toTarget . fromJustNote "no ally" . find ((== traveller.id) . allyCardId . toAttrs) . toList . (.entities.allies))
      (.placement) <$> locationNow staged `shouldReturn` ActiveLocation
      (.placement) <$> locationNow active `shouldReturn` StagingArea

  describe "Guthlaf" do
    it "costs 1 less with a Rohan hero in play" . rohanTest $ \pid -> do
      _ <- playWith pid eomerCode 2 Allies.guthlaf
      (.resources) <$> getHeroWithCode eomerCode `shouldReturn` 0
      allyCodes `shouldReturn` [toCardCode Allies.guthlaf]

  describe "Westfold Outrider" do
    it "is discarded to engage an enemy" . rohanTest $ \pid -> do
      enemy <- stageEnemy Enemies.hillTroll
      outrider <- putAllyIntoPlay pid Allies.westfoldOutrider
      run $ ActionWindow pid
      chooseAbilityOf pid outrider
      fmap (enemyPlacement . toAttrs) <$> enemyNow enemy `shouldReturn` Just (EngagedWith pid)
      allyCodes `shouldReturn` []

  describe "Westfold Horse-breeder" do
    it "searches the top 10 cards for a Mount attachment after entering play" . rohanTest $ \pid -> do
      [steed] <- onTopOfDeck pid [Attachments.steedOfTheMark]
      breeder <- putAllyIntoPlay pid Allies.westfoldHorseBreeder
      chooseAbilityOf pid breeder
      chooseTarget pid (CardIdTarget steed.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem steed.id)

  describe "Riddermark Knight" do
    it "attacks with +2 attack, then is discarded" . rohanTest $ \pid -> do
      spider <- engagedEnemy pid Enemies.forestSpider
      knight <- putAllyIntoPlay pid Allies.riddermarkKnight
      run $ PlayerAttacks pid
      chooseTarget pid knight
      chooseAbilityOf pid knight
      chooseLabel "$label.doneAttacking" pid
      passResponses pid
      fmap (.damage) <$> enemyNow spider `shouldReturn` Just 3
      allyCodes `shouldReturn` []

  describe "Westfold Lancer" do
    it "is discarded after questing successfully to deal 2 damage to an enemy in the staging area" . rohanTest $ \pid -> do
      troll <- stageEnemy Enemies.hillTroll
      lancer <- putAllyIntoPlay pid Allies.westfoldLancer
      run $ CommitToQuest lancer
      run QuestResolution
      chooseAbilityOf pid lancer
      fmap (.damage) <$> enemyNow troll `shouldReturn` Just 2
      allyCodes `shouldReturn` []

  describe "Ancient Mathom" do
    it "lets the first player draw 3 cards when its location is explored" . rohanTest $ \pid -> do
      location <- stageLocation Locations.oldForestRoad
      mathom <- putAttachmentOn pid Attachments.ancientMathom (toTarget location)
      handBefore <- handSize pid
      run $ PlaceTokens GameSource (toTarget location) Progress 3
      chooseAbilityOf pid mathom
      handSize pid `shouldReturn` (handBefore + 3)

  describe "Spear of the Mark" do
    it "gives +1 attack, +2 against an enemy in the staging area" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      _ <- putAttachmentOn pid Attachments.spearOfTheMark eomer
      troll <- stageEnemy Enemies.hillTroll
      getAttack eomer `shouldReturn` 4
      getAttackStrength [eomer] (toId troll) Enemies.hillTroll `shouldReturn` 5

  describe "Forth Eorlingas!" do
    it "lets Rohan heroes attack enemies in the staging area" . rohanTest $ \pid -> do
      spider <- stageEnemy Enemies.forestSpider
      card <- putCardIntoHand pid Events.forthEorlingas
      giveResources eomerCode 2
      run $ BeginPhase CombatPhase
      passResponses pid
      chooseTarget pid card
      passActions pid
      eomer <- hero eomerCode
      chooseTarget pid eomer
      chooseLabel "$label.doneAttacking" pid
      fmap (.damage) <$> enemyNow spider `shouldReturn` Just 2

  describe "Steed of the Mark" do
    it "spends a resource to ready its hero after it commits" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      steed <- putAttachmentOn pid Attachments.steedOfTheMark eomer
      giveResources eomerCode 1
      run $ CommitToQuest eomer
      run $ CheckWindows [mkAfter $ CommittedToQuest eomer]
      chooseAbilityOf pid steed
      exhausted eomerCode `shouldReturn` False
      (.resources) <$> getHeroWithCode eomerCode `shouldReturn` 0

  describe "Éomer" do
    it "gets +2 attack after a character leaves play" . rohanTest $ \pid -> do
      ally' <- putAllyIntoPlay pid Allies.westfoldOutrider
      run $ Discard ally'
      eomer <- hero eomerCode
      chooseAbilityOf pid eomer
      getAttack eomer `shouldReturn` 5

  describe "Rohan Warhorse" do
    it "readies its hero after an attack it made destroys an enemy" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      horse <- putAttachmentOn pid Attachments.rohanWarhorse eomer
      run $ Exhaust eomer
      eid <- getRandom
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [eomer]]
      chooseAbilityOf pid horse
      exhausted eomerCode `shouldReturn` False

  describe "Firefoot" do
    it "gives Éomer +2 attack and sends the excess damage of a lone attack to another enemy" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      _ <- putAttachmentOn pid Attachments.firefoot eomer
      getAttack eomer `shouldReturn` 5
      sniper <- engagedEnemy pid Enemies.goblinSniper
      troll <- engagedEnemy pid Enemies.hillTroll
      run $ AttackWith eomer
      run $ ResolveAttack pid (toId sniper)
      passResponses pid
      chooseTarget pid (toId troll)
      isNothing <$> enemyNow sniper `shouldReturn` True
      fmap (.damage) <$> enemyNow troll `shouldReturn` Just 3

  describe "Charge of the Rohirrim" do
    it "gives Rohan characters with a Mount +3 attack" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      _ <- putAttachmentOn pid Attachments.firefoot eomer
      _ <- playWith pid eomerCode 2 Events.chargeOfTheRohirrim
      getAttack eomer `shouldReturn` 8

  describe "Wait no Longer" do
    it "engages an enemy from the top of the encounter deck and reveals one less card" . rohanTest $ \pid -> do
      [_, spider, _] <- setEncounterDeck [Locations.oldForestRoad, Enemies.forestSpider, Locations.mountainsOfMirkwood]
      card <- putCardIntoHand pid Events.waitNoLonger
      giveResources eomerCode 2
      run $ BeginPhase QuestPhase
      chooseTarget pid card
      chooseTarget pid spider
      getsGame (map (enemyPlacement . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [EngagedWith pid]
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals (-1)))
      length <$> getEncounterDeck `shouldReturn` 2

  describe "Hirgon" do
    it "plays a tactics ally for 1 less after questing successfully, and may boost it" . rohanTest $ \pid -> do
      card <- putCardIntoHand pid Allies.riddermarkKnight
      giveResources eomerCode 1
      hirgon <- hero hirgonCode
      run $ CommitToQuest hirgon
      run QuestResolution
      chooseAbilityOf pid hirgon
      chooseTarget pid card
      passResponses pid
      chooseLabel "$label.hirgon.raiseThreat" pid
      (.threat) <$> getPlayer pid `shouldReturn` 1
      knight <- getsGame (toTarget . fromJustNote "no knight" . headMay . toList . (.entities.allies))
      getAttack knight `shouldReturn` 3
      (.resources) <$> getHeroWithCode eomerCode `shouldReturn` 0

  describe "The Red Arrow" do
    it "leaves play with Valour to put an ally from the top 5 cards into play" . rohanTest $ \pid -> do
      hirgon <- hero hirgonCode
      arrow <- putAttachmentOn pid Attachments.theRedArrow hirgon
      getWillpower hirgon `shouldReturn` 3
      [guthlaf] <- onTopOfDeck pid [Allies.guthlaf]
      run $ RaiseThreat pid 40
      run $ CommitToQuest hirgon
      run QuestResolution
      chooseAbilityOf pid arrow
      chooseTarget pid (CardIdTarget guthlaf.id)
      allyCodes `shouldReturn` [toCardCode Allies.guthlaf]
      getsGame (length . (.entities.attachments)) `shouldReturn` 0
      (.discard) <$> getPlayer pid `shouldReturn` []

  describe "Fastred" do
    it "returns the enemy it defended to the staging area to lower threat by 2" . rohanTest $ \pid -> do
      run . PutHeroIntoPlay pid =<< genPlayerCard Heroes.fastred (Just pid)
      fastred <- hero "17081"
      troll <- engagedEnemy pid Enemies.hillTroll
      run $ RaiseThreat pid 10
      run $ CheckWindows [mkAfter $ DefendedAttack fastred (toId troll)]
      chooseAbilityOf pid fastred
      fmap (enemyPlacement . toAttrs) <$> enemyNow troll `shouldReturn` Just StagingArea
      (.threat) <$> getPlayer pid `shouldReturn` 8

  describe "Oath of Eorl" do
    it "moves the player's attacks ahead of the enemy attacks" . rohanTest $ \pid -> do
      _ <- engagedEnemy pid Enemies.hillTroll
      card <- putCardIntoHand pid Events.oathOfEorl
      giveResources eomerCode 3
      run $ BeginPhase CombatPhase
      chooseTarget pid card
      queue <- peekQueue
      let indexOf m = fromJustNote "missing" $ findIndex (== m) queue
      (indexOf (PlayerAttacks pid) < indexOf (EnemyAttacks pid)) `shouldBe` True

  describe "Lothíriel" do
    it "gains Rohan with Éomer and puts an ally sharing a trait into play committed" . rohanTest $ \pid -> do
      lothiriel <- hero lothirielCode
      card <- putCardIntoHand pid Allies.westfoldHorseBreaker
      run $ CommitToQuest lothiriel
      run $ CheckWindows [mkAfter $ CommittedToQuest lothiriel]
      chooseAbilityOf pid lothiriel
      chooseTarget pid card
      map (\a -> (allyCommitted (toAttrs a), allyExhausted (toAttrs a))) <$> allies `shouldReturn` [(True, True)]
      [ally'] <- allies
      getModifiers ally' >>= (`shouldSatisfy` elem ShuffleIntoDeckAtEndOfPhase)

  describe "War Axe" do
    it "gives +1 attack for each restricted attachment" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      _ <- putAttachmentOn pid Attachments.firefoot eomer
      _ <- putAttachmentOn pid Attachments.warAxe eomer
      getAttack eomer `shouldReturn` 7

  describe "Horn of the Mark" do
    it "draws a card when a character sharing a trait leaves play" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      horn <- putAttachmentOn pid Attachments.hornOfTheMark eomer
      ally' <- putAllyIntoPlay pid Allies.westfoldOutrider
      handBefore <- handSize pid
      run $ Discard ally'
      chooseAbilityOf pid horn
      handSize pid `shouldReturn` (handBefore + 1)

    it "is not offered when the character shares no trait with its hero" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      horn <- putAttachmentOn pid Attachments.hornOfTheMark eomer
      ally' <- putAllyIntoPlay pid Allies.wanderingTook
      run $ Discard ally'
      offersAbilityOf pid horn `shouldReturn` False

    it "answers its own hero leaving play, as it does so" . rohanTest $ \pid -> do
      eomer <- hero eomerCode
      horn <- putAttachmentOn pid Attachments.hornOfTheMark eomer
      handBefore <- handSize pid
      run $ Discard eomer
      chooseAbilityOf pid horn
      handSize pid `shouldReturn` (handBefore + 1)

  describe "The Muster of Rohan" do
    it "is paid by Rohan heroes and puts Rohan allies into play until the end of the round" . rohanTest $ \pid -> do
      [knight, lancer] <- onTopOfDeck pid [Allies.riddermarkKnight, Allies.westfoldLancer]
      _ <- playWith pid eomerCode 4 Events.theMusterOfRohan
      chooseTarget pid (CardIdTarget knight.id)
      passResponses pid
      chooseTarget pid (CardIdTarget lancer.id)
      passResponses pid
      chooseLabel "$label.done" pid
      (.resources) <$> getHeroWithCode eomerCode `shouldReturn` 0
      sort <$> allyCodes `shouldReturn` sort [toCardCode Allies.riddermarkKnight, toCardCode Allies.westfoldLancer]
      chooseLabel "$label.donePlanning" pid
      run EndRound
      passResponses pid
      passResponses pid
      allyCodes `shouldReturn` []

  describe "Astonishing Speed" do
    it "gives Rohan characters +2 willpower" . rohanTest $ \pid -> do
      _ <- playWith pid lothirielCode 3 Events.astonishingSpeed
      (getWillpower =<< hero eomerCode) `shouldReturn` 3
