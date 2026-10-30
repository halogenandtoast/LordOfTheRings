-- | The player cards of Escape from Mount Gram, Across the Ettenmoors and The Treachery of
-- Rhudaur.
module LordOfTheRings.AngmarAwakened.PlayerCardsBSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (AttackModifier, GainKeyword, GainTrait))
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), victoryDisplayL)
import LordOfTheRings.Trait (Trait (Noldor, Silvan))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (CommittedToQuest, DealtDamage, RoundEnds), mkAfter, mkWhen)
import TestImport

beravorCode, eowynCode, theodredCode, gimliCode, legolasCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
gimliCode = "01004"
legolasCode = "01005"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

-- | Read the modifiers again, after the game was changed by hand.
refresh :: GameT ()
refresh = putGame =<< preloadModifiers =<< getGame

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

deckSize :: PlayerId -> GameT Int
deckSize pid = length . (.deck) <$> getPlayer pid

inHand :: PlayerId -> CardId -> GameT Bool
inHand pid cid = elem cid . map (.id) . (.hand) <$> getPlayer pid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . lookup hid . mapToList <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

allyCount :: GameT Int
allyCount = getsGame (length . (.entities.allies))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (sort . map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

-- | An encounter card put straight into the victory display.
intoVictoryDisplay :: CardDef -> GameT ()
intoVictoryDisplay def = do
  card <- genEncounterCard def
  putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (EncounterCard card :))) =<< getGame
  refresh

firstLocation :: GameT LocationId
firstLocation = getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

-- | A player card put on top of the player's deck.
onTopOfDeck :: PlayerId -> CardDef -> GameT Card
onTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (card :)) =<< getGame
  pure card

putInDiscard :: PlayerId -> CardDef -> GameT Card
putInDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . discardL %~ (card :)) =<< getGame
  pure card

-- | A player side quest played from hand, paid by the hero, made the current quest.
sideQuestInPlay :: PlayerId -> CardCode -> Int -> CardDef -> GameT ()
sideQuestInPlay pid payer n def = do
  _ <- putQuestStage Quests.fliesAndSpiders
  card <- playWith pid payer n def
  passActions pid
  side <- getsGame (toTarget . fromJustNote "no side quest" . find ((== card.id) . questCardId . toAttrs) . toList . (.entities.quests))
  run $ SetCurrentQuest side

-- | The test deck with these heroes.
withHeroes :: [CardCode] -> (PlayerId -> GameT ()) -> IO ()
withHeroes codes = gameTestWith testDeck {heroes = codes}

spec :: Spec
spec = describe "Angmar Awakened player cards: Mount Gram, the Ettenmoors and Rhudaur" do
  describe "Rossiel" do
    it "gets +2 willpower while the active location shares a trait with one in the victory display" . withHeroes ["10028", eowynCode, theodredCode] $ \_ -> do
      rossiel <- hero "10028"
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.oldForestRoad
      getWillpower rossiel `shouldReturn` 2
      intoVictoryDisplay Locations.greatForestWeb
      getWillpower rossiel `shouldReturn` 4

    it "gets +2 defense defending an enemy that shares a trait with one in the victory display" . withHeroes ["10028", beravorCode, theodredCode] $ \pid -> do
      rossiel <- hero "10028"
      intoVictoryDisplay Enemies.kingSpider
      eid <- engagedEnemy pid Enemies.forestSpider
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ DefendWith pid eid rossiel
      getDefense rossiel `shouldReturn` 4

  describe "Veteran of Osgiliath" do
    it "gets +1 willpower, attack and defense while your threat is 40 or higher" . gameTest $ \pid -> do
      veteran <- putAllyIntoPlay pid Allies.veteranOfOsgiliath
      getAttack veteran `shouldReturn` 2
      setThreat pid 40
      refresh
      getAttack veteran `shouldReturn` 3
      getWillpower veteran `shouldReturn` 2
      getDefense veteran `shouldReturn` 2

  describe "Descendants of Kings" do
    it "readies a Dúnedain character for each enemy engaged with you" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      run $ Exhaust beravor
      _ <- engagedEnemy pid Enemies.forestSpider
      _ <- playWith pid theodredCode 1 Events.descendantsOfKings
      chooseTarget pid beravor
      isExhausted beravor `shouldReturn` False

  describe "Derndingle Warrior" do
    it "takes 1 damage for +3 defense while defending, once an attack" . gameTest $ \pid -> do
      warrior <- putAllyIntoPlay pid Allies.derndingleWarrior
      isExhausted warrior `shouldReturn` True
      eid <- engagedEnemy pid Enemies.forestSpider
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ DefendWith pid eid warrior
      chooseAbilityOf pid warrior
      getCharacterDamage warrior `shouldReturn` 1
      getDefense warrior `shouldReturn` 5
      (`shouldBe` False) =<< offersAbilityOf pid warrior

  describe "Boomed and Trumpeted" do
    it "readies an Ent dealt damage and gives it +3 attack" . withHeroes [beravorCode, gimliCode, theodredCode] $ \pid -> do
      ent <- putAllyIntoPlay pid Allies.wanderingEnt
      gimli <- hero gimliCode
      run $ PlaceTokens GameSource gimli Resource 1
      card <- putCardIntoHand pid Events.boomedAndTrumpeted
      run $ PlaceTokens GameSource ent Damage 1
      run $ CheckWindows [mkAfter $ DealtDamage ent 1]
      chooseTarget pid (CardIdTarget card.id)
      isExhausted ent `shouldReturn` False
      getAttack ent `shouldReturn` 5

  describe "Elven Jeweler" do
    it "discards 2 cards from hand to enter play from it" . gameTest $ \pid -> do
      a <- putCardIntoHand pid Events.hornsCry
      b <- putCardIntoHand pid Events.noneReturn
      jeweler <- putCardIntoHand pid Allies.elvenJeweler
      run $ UseHandAction pid jeweler.id
      chooseTarget pid (CardIdTarget a.id)
      chooseTarget pid (CardIdTarget b.id)
      allyCount `shouldReturn` 1
      inHand pid a.id `shouldReturn` False
      inHand pid jeweler.id `shouldReturn` False

  describe "Double Back" do
    it "reduces each player's threat by 5 once defeated" . gameTest $ \pid -> do
      sideQuestInPlay pid eowynCode 0 Quests.doubleBack
      setThreat pid 30
      run $ PlaceProgressOnQuest 4
      threat pid `shouldReturn` 25
      victoryDisplay `shouldReturn` [toCardCode Quests.doubleBack]

  describe "Leave No Trace" do
    it "adds itself and a non-unique explored location to the victory display" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- firstLocation
      card <- putCardIntoHand pid Events.leaveNoTrace
      beravor <- hero beravorCode
      run $ PlaceTokens GameSource beravor Resource 1
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      chooseTarget pid (CardIdTarget card.id)
      victoryDisplay `shouldReturn` sort [toCardCode Events.leaveNoTrace, toCardCode Locations.oldForestRoad]
      getEncounterDiscard `shouldReturn` []

  describe "Distant Stars" do
    it "exhausts a Ranger to swap the active location for one from the encounter deck" . gameTest $ \pid -> do
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.oldForestRoad
      [web] <- setEncounterDeck [Locations.greatForestWeb]
      _ <- playWith pid beravorCode 0 Events.distantStars
      chooseTarget pid (CardIdTarget web.id)
      (`shouldBe` True) =<< isExhausted =<< hero beravorCode
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.greatForestWeb]
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.oldForestRoad]

  describe "Keen as Lances" do
    it "costs 1 less for each card worth no victory points in the victory display, and goes there" . gameTest $ \pid -> do
      replicateM_ 3 $ intoVictoryDisplay Locations.oldForestRoad
      was <- handSize pid
      _ <- playWith pid theodredCode 2 Events.keenAsLances
      chooseLabel "$label.keenAsLances.draw" pid
      handSize pid `shouldReturn` was + 3
      elem (toCardCode Events.keenAsLances) <$> victoryDisplay `shouldReturn` True

  describe "Dori" do
    it "exhausts to add his defense to another hero defending" . withHeroes ["10054", beravorCode, theodredCode] $ \pid -> do
      beravor <- hero beravorCode
      dori <- hero "10054"
      eid <- engagedEnemy pid Enemies.forestSpider
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ DefendWith pid eid beravor
      chooseAbilityOf pid dori
      isExhausted dori `shouldReturn` True
      getDefense beravor `shouldReturn` 4

  describe "Ranger Provisions" do
    it "gives each of the first player's heroes a resource as its location is explored" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- firstLocation
      provisions <- putAttachmentOn pid Attachments.rangerProvisions (toTarget lid)
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      chooseAbilityOf pid provisions
      (getHeroResources . toId =<< getHeroWithCode theodredCode) `shouldReturn` 1
      (getHeroResources . toId =<< getHeroWithCode beravorCode) `shouldReturn` 1

  describe "Dúnedain Message" do
    it "searches the deck for a side quest" . gameTest $ \pid -> do
      quest <- onTopOfDeck pid Quests.doubleBack
      _ <- playWith pid theodredCode 1 Events.dunedainMessage
      chooseTarget pid (CardIdTarget quest.id)
      inHand pid quest.id `shouldReturn` True

  describe "Longbeard Sentry" do
    it "discards the top 2 cards of the deck for sentinel and +1 defense, once a phase" . gameTest $ \pid -> do
      sentry <- putAllyIntoPlay pid Allies.longbeardSentry
      cards <- deckSize pid
      run $ ActionWindow pid
      chooseAbilityOf pid sentry
      deckSize pid `shouldReturn` cards - 2
      getDefense sentry `shouldReturn` 3
      getModifiers sentry >>= (`shouldSatisfy` elem (GainKeyword Sentinel))
      (`shouldBe` False) =<< offersAbilityOf pid sentry

  describe "Delay the Enemy" do
    it "lets each player discard a non-unique enemy engaged with him once defeated" . withHeroes [beravorCode, gimliCode, theodredCode] $ \pid -> do
      sideQuestInPlay pid gimliCode 0 Quests.delayTheEnemy
      eid <- engagedEnemy pid Enemies.forestSpider
      run $ PlaceProgressOnQuest 8
      chooseTarget pid eid
      getsGame (length . (.entities.enemies)) `shouldReturn` 0

  describe "Steed of Imladris" do
    it "discards a card as its hero commits to place 2 progress on the active location" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      steed <- putAttachmentOn pid Attachments.steedOfImladris eowyn
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.oldForestRoad
      lid <- firstLocation
      card <- putCardIntoHand pid Events.hornsCry
      run $ CommitToQuest eowyn
      run $ CheckWindows [mkAfter $ CommittedToQuest eowyn]
      chooseAbilityOf pid steed
      chooseTarget pid (CardIdTarget card.id)
      (.progress) <$> getLocation lid `shouldReturn` 2

  describe "Fair and Perilous" do
    it "adds a Silvan character's willpower to its attack" . withHeroes [eowynCode, legolasCode, theodredCode] $ \pid -> do
      legolas <- hero legolasCode
      _ <- playWith pid eowynCode 1 Events.fairAndPerilous
      getAttack legolas `shouldReturn` 4

  describe "Wellinghall Preserver" do
    it "heals 1 damage from an Ent as it readies" . gameTest $ \pid -> do
      preserver <- putAllyIntoPlay pid Allies.wellinghallPreserver
      ent <- putAllyIntoPlay pid Allies.wanderingEnt
      run $ PlaceTokens GameSource ent Damage 1
      run $ Ready preserver
      chooseAbilityOf pid preserver
      getCharacterDamage ent `shouldReturn` 0

  describe "None Return" do
    it "adds itself and a destroyed non-unique enemy to the victory display" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
      card <- putCardIntoHand pid Events.noneReturn
      beravor <- hero beravorCode
      run $ PlaceTokens GameSource beravor Resource 1
      run $ DealDamage GameSource (toTarget eid) 4
      chooseTarget pid (CardIdTarget card.id)
      victoryDisplay `shouldReturn` sort [toCardCode Events.noneReturn, toCardCode Enemies.forestSpider]
      getEncounterDiscard `shouldReturn` []

  describe "Hope Rekindled" do
    it "lowers the next Valour event's cost by 2" . withHeroes [beravorCode, gimliCode, theodredCode] $ \pid -> do
      cry <- putCardIntoHand pid Events.hornsCry
      _ <- playWith pid theodredCode 0 Events.hopeRekindled
      chooseTarget pid (CardIdTarget cry.id)
      inHand pid cry.id `shouldReturn` False

    it "searches the top 10 cards for a Valour event at 40 threat" . gameTest $ \pid -> do
      setThreat pid 40
      cry <- onTopOfDeck pid Events.hornsCry
      _ <- playWith pid theodredCode 0 Events.hopeRekindled
      chooseLabel "$label.valour" pid
      chooseTarget pid (CardIdTarget cry.id)
      inHand pid cry.id `shouldReturn` True

  describe "Erestor" do
    it "draws 3 more cards in the resource phase and discards his hand at the end of the round" . withHeroes ["10084", eowynCode, theodredCode] $ \pid -> do
      refresh
      was <- handSize pid
      run $ BeginPhase ResourcePhase
      handSize pid `shouldReturn` was + 4
      run $ CheckWindows [mkWhen RoundEnds]
      handSize pid `shouldReturn` 0

  describe "Reinforcements" do
    it "puts 2 allies from hand into play until the end of the phase" . gameTest $ \pid -> do
      a <- putCardIntoHand pid Allies.galadhonArcher
      b <- putCardIntoHand pid Allies.wanderingEnt
      card <- putCardIntoHand pid Events.reinforcements
      case card of
        PlayerCard pc -> run $ PlayEvent pid pc []
        _ -> error "not a player card"
      chooseTarget pid (CardIdTarget a.id)
      chooseTarget pid (CardIdTarget b.id)
      allyCount `shouldReturn` 2
      run $ EndPhase PlanningPhase
      allyCount `shouldReturn` 0
      inHand pid a.id `shouldReturn` True

  describe "Send for Aid" do
    it "lets each player put an ally from the top 10 cards into play once defeated" . gameTest $ \pid -> do
      sideQuestInPlay pid theodredCode 1 Quests.sendForAid
      ally <- onTopOfDeck pid Allies.galadhonArcher
      run $ PlaceProgressOnQuest 6
      chooseTarget pid (CardIdTarget ally.id)
      allyCount `shouldReturn` 1

  describe "Elven Spear" do
    it "discards a card for +1 attack" . withHeroes [beravorCode, legolasCode, theodredCode] $ \pid -> do
      legolas <- hero legolasCode
      spear <- putAttachmentOn pid Attachments.elvenSpear legolas
      card <- putCardIntoHand pid Events.hornsCry
      run $ ActionWindow pid
      chooseAbilityOf pid spear
      chooseTarget pid (CardIdTarget card.id)
      getAttack legolas `shouldReturn` 4

  describe "Horn's Cry" do
    it "gives each enemy engaged with the chosen player -3 attack with Valour" . withHeroes [beravorCode, gimliCode, theodredCode] $ \pid -> do
      setThreat pid 40
      eid <- engagedEnemy pid Enemies.forestSpider
      _ <- playWith pid gimliCode 2 Events.hornsCry
      chooseLabel "$label.valour" pid
      getModifiers (toTarget eid) >>= (`shouldSatisfy` elem (AttackModifier (-3)))

  describe "Galadhrim Weaver" do
    it "shuffles the top card of the discard pile into the deck as it enters play" . gameTest $ \pid -> do
      discarded <- putInDiscard pid Events.hornsCry
      weaver <- putAllyIntoPlay pid Allies.galadhrimWeaver
      chooseAbilityOf pid weaver
      elem discarded.id . map (.id) . (.deck) <$> getPlayer pid `shouldReturn` True

  describe "Silver Harp" do
    it "exhausts to return a card discarded from hand" . gameTest $ \pid -> do
      harp <- putAttachmentOn pid Attachments.silverHarp =<< hero eowynCode
      card <- putCardIntoHand pid Events.hornsCry
      run $ DiscardFromHand pid card.id
      chooseAbilityOf pid harp
      inHand pid card.id `shouldReturn` True

  describe "Galdor of the Havens" do
    it "draws a card after a discard from hand, once a round" . gameTest $ \pid -> do
      galdor <- putAllyIntoPlay pid Allies.galdorOfTheHavens
      a <- putCardIntoHand pid Events.hornsCry
      b <- putCardIntoHand pid Events.noneReturn
      was <- handSize pid
      run $ DiscardFromHand pid a.id
      chooseAbilityOf pid galdor
      handSize pid `shouldReturn` was
      run $ DiscardFromHand pid b.id
      (`shouldBe` False) =<< offersAbilityOf pid galdor

  describe "The Door is Closed!" do
    it "cancels and discards a revealed card sharing a title with one in the victory display" . gameTest $ \pid -> do
      intoVictoryDisplay Treacheries.theNecromancersReach
      _ <- setEncounterDeck [Treacheries.theNecromancersReach]
      card <- putCardIntoHand pid Events.theDoorIsClosed
      beravor <- hero beravorCode
      run $ PlaceTokens GameSource beravor Resource 1
      run $ Exhaust beravor
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget card.id)
      resolveRevealed
      getCharacterDamage beravor `shouldReturn` 0
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Treacheries.theNecromancersReach]

  describe "Elf-friend" do
    it "gives its character the Noldor and Silvan traits" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.elfFriend beravor
      getModifiers beravor >>= (`shouldSatisfy` \ms -> GainTrait Noldor `elem` ms && GainTrait Silvan `elem` ms)
