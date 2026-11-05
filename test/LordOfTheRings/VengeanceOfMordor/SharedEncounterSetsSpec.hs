module LordOfTheRings.VengeanceOfMordor.SharedEncounterSetsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getEngagementCost, getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (..), cardMatches)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Condition, Mount, Weapon))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = getsGame (maybe Unplaced (.placement) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

questOf :: CardDef -> GameT Target
questOf def = getsGame (toTarget . fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

-- | The main quest in play, for a side quest to be defeated beside it.
mainQuest :: GameT Target
mainQuest = putQuestStage Quests.fliesAndSpiders

-- | The codes of the treacheries attached to the enemy.
attachedTo :: EnemyId -> GameT [CardCode]
attachedTo eid = getsGame (map toCardCode . filter ((== AttachedTo (EnemyTarget eid)) . (.placement)) . toList . (.entities.treacheries))

treacheryOf :: CardDef -> GameT Target
treacheryOf def = getsGame (toTarget . fromJustNote "no such treachery" . find ((== toCardCode def) . toCardCode) . toList . (.entities.treacheries))

-- | A card of the def in the encounter discard pile, on top.
discarded :: CardDef -> GameT EncounterCard
discarded def = do
  card <- genEncounterCard def
  run $ AddToEncounterDiscard [card]
  pure card

discardCodes :: GameT [CardCode]
discardCodes = map toCardCode <$> getEncounterDiscard

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

deckSize :: PlayerId -> GameT Int
deckSize pid = length . (.deck) <$> getPlayer pid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

alliesInPlay :: GameT Int
alliesInPlay = getsGame (length . (.entities.allies))

{- | The enemy attacks the player, the shadow card dealt to it first, and the defender
declares against it.
-}
shadowAttack :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
shadowAttack pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

-- | A version of Thane Ulchor, once a scenario of the cycle adds one: the shared sets read
-- him by title.
thaneDef :: Maybe CardDef
thaneDef = find (cardMatches $ CardWithTitle "Thane Ulchor") (toList Enemies.allEnemyCards)

withThane :: (CardDef -> PlayerId -> GameT ()) -> IO ()
withThane body = maybe (pendingWith "no version of Thane Ulchor is defined yet") (gameTest . body) thaneDef

-- | A version of Thane Ulchor a player can engage: Wrath and Ruin's cannot leave the
-- staging area.
withEngageableThane :: (CardDef -> PlayerId -> GameT ()) -> IO ()
withEngageableThane body =
  maybe (pendingWith "no version of Thane Ulchor that can be engaged is defined yet") (gameTest . body)
    $ find ((/= toCardCode Enemies.thaneUlchorWrathAndRuin) . toCardCode)
    $ filter (cardMatches $ CardWithTitle "Thane Ulchor") (toList Enemies.allEnemyCards)

spec :: Spec
spec = describe "A Shadow in the East's shared encounter sets" do
  describe "Riders of Rhûn" do
    it "attaches the first Easterling treachery discarded to Easterling Captain, which he discards in place of taking damage" . gameTest $ \_ -> do
      _ <- setEncounterDeck [Locations.oldForestRoad, Treacheries.swordOfRhun, Locations.oldForestRoad]
      captain <- stage Enemies.easterlingCaptain
      attachedTo captain `shouldReturn` [toCardCode Treacheries.swordOfRhun]
      discardCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
      getEnemyAttack captain `shouldReturn` 8
      run $ DealDamage GameSource (toTarget captain) 3
      getDamage (toTarget captain) `shouldReturn` 0
      attachedTo captain `shouldReturn` []
      -- with no Easterling attachment left he takes it
      run $ DealDamage GameSource (toTarget captain) 3
      getDamage (toTarget captain) `shouldReturn` 3

    it "has Rider of Rhûn find an Easterling Horse, -10 engagement cost for it, +2 threat from it" . gameTest $ \_ -> do
      reveal [Enemies.riderOfRhun, Locations.oldForestRoad, Treacheries.easterlingHorse]
      rider <- enemyOf Enemies.riderOfRhun
      attachedTo rider `shouldReturn` [toCardCode Treacheries.easterlingHorse]
      getEngagementCost rider `shouldReturn` 30
      getEnemyThreat rider `shouldReturn` 4
      horse <- treacheryOf Treacheries.easterlingHorse
      getModifiers horse >>= (`shouldSatisfy` elem (GainTrait Mount))

    it "attaches Easterling Horse to the lowest engagement cost Easterling enemy, returning it to the staging area, and again at the end of the round" . gameTest $ \pid -> do
      raider <- engaged pid Enemies.easterlingRaider
      warrior <- stage Enemies.warriorOfRhun
      reveal [Treacheries.easterlingHorse]
      attachedTo raider `shouldReturn` [toCardCode Treacheries.easterlingHorse]
      attachedTo warrior `shouldReturn` []
      placementOfEnemy raider `shouldReturn` StagingArea
      run $ EngageEnemy pid raider
      run $ CheckWindows [mkAfter RoundEnds]
      placementOfEnemy raider `shouldReturn` StagingArea

    it "has Easterling Horse surge with no Easterling enemy to take it" . gameTest $ \_ -> do
      reveal [Treacheries.easterlingHorse, Locations.oldForestRoad]
      locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]

  describe "Easterling Raiders" do
    it "gives Easterling Raider +1 attack and defense for each Easterling attachment" . gameTest $ \_ -> do
      raider <- stage Enemies.easterlingRaider
      reveal [Treacheries.swordOfRhun, Locations.oldForestRoad]
      -- 2, +1 for the attachment, +2 from the Sword
      getEnemyAttack raider `shouldReturn` 5
      getEnemyDefense raider `shouldReturn` 3

    it "gives the attacking enemy +1 attack with Easterling Raider's shadow, +2 with an Easterling attachment" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      shadowAttack pid spider Enemies.easterlingRaider beravor
      -- Forest Spider's 3 attack, raised as it engaged, +1, against Beravor's 2 defense
      getDamage beravor `shouldReturn` 2
      _ <- putAttachmentOn pid Attachments.citadelPlate beravor
      warrior <- engaged pid Enemies.warriorOfRhun
      _ <- discarded Treacheries.swordOfRhun
      run $ PlaceEnemy spider StagingArea
      run $ Ready beravor
      shadowAttack pid warrior Enemies.easterlingRaider beravor
      -- the Warrior takes the Sword as it attacks: 3 + 2, +2 for the shadow
      getDamage beravor `shouldReturn` 7

    it "has Warrior of Rhûn take the top Easterling treachery in the discard pile as it attacks" . gameTest $ \pid -> do
      warrior <- engaged pid Enemies.warriorOfRhun
      _ <- discarded Treacheries.recurveBow
      _ <- discarded Locations.oldForestRoad
      _ <- discarded Treacheries.swordOfRhun
      beravor <- hero "01012"
      shadowAttack pid warrior Locations.oldForestRoad beravor
      attachedTo warrior `shouldReturn` [toCardCode Treacheries.swordOfRhun]
      getDamage beravor `shouldReturn` 3

    it "attaches the top Easterling treachery in the discard pile to the attacking enemy with Warrior of Rhûn's shadow" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      _ <- discarded Treacheries.swordOfRhun
      shadowAttack pid spider Enemies.warriorOfRhun =<< hero "01012"
      attachedTo spider `shouldReturn` [toCardCode Treacheries.swordOfRhun]
      sword <- treacheryOf Treacheries.swordOfRhun
      getModifiers sword >>= (`shouldSatisfy` elem (GainTrait Weapon))

    it "attaches Sword of Rhûn to the lowest attack Easterling enemy without one" . gameTest $ \_ -> do
      raider <- stage Enemies.easterlingRaider
      warrior <- stage Enemies.warriorOfRhun
      reveal [Treacheries.swordOfRhun, Locations.oldForestRoad]
      attachedTo raider `shouldReturn` [toCardCode Treacheries.swordOfRhun]
      reveal [Treacheries.swordOfRhun, Locations.oldForestRoad]
      attachedTo warrior `shouldReturn` [toCardCode Treacheries.swordOfRhun]
      getEnemyAttack warrior `shouldReturn` 5

    it "attaches Recurve Bow to the highest engagement cost Easterling enemy without one, giving it archery 2" . gameTest $ \_ -> do
      raider <- stage Enemies.easterlingRaider
      warrior <- stage Enemies.warriorOfRhun
      reveal [Treacheries.recurveBow, Locations.oldForestRoad]
      attachedTo warrior `shouldReturn` [toCardCode Treacheries.recurveBow]
      getModifiers warrior >>= (`shouldSatisfy` elem (GainKeyword (Archery 2)))
      reveal [Treacheries.recurveBow, Locations.oldForestRoad]
      attachedTo raider `shouldReturn` [toCardCode Treacheries.recurveBow]

    it "arms each Easterling enemy entering play while Warriors of the East is in play, and has the strongest attack as it becomes the current quest" . gameTest $ \pid -> do
      _ <- mainQuest
      reveal [Quests.warriorsOfTheEast, Locations.oldForestRoad]
      locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
      _ <- discarded Treacheries.swordOfRhun
      raider <- stage Enemies.easterlingRaider
      attachedTo raider `shouldReturn` [toCardCode Treacheries.swordOfRhun]
      _ <- stage Enemies.warriorOfRhun
      q <- questOf Quests.warriorsOfTheEast
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ SetCurrentQuest q
      beravor <- hero "01012"
      passActions pid
      chooseTarget pid beravor
      passActions pid
      -- the Raider's 5 attack against Beravor's 2 defense
      getDamage beravor `shouldReturn` 3

  describe "Rolling Plains" do
    it "gives each copy of Rolling Plains +2 threat this phase as one is revealed, and raises each player's threat by 2 to travel there" . gameTest $ \pid -> do
      plains <- stageLocation Locations.rollingPlains
      reveal [Locations.rollingPlains]
      getLocationThreat plains `shouldReturn` 4
      (getLocationThreat =<< locationOf Locations.rollingPlains) `shouldReturn` 4
      run $ TravelTo plains
      threatOf pid `shouldReturn` 2

    it "keeps player cards from readying characters while Rocky Outcrop is staged, and has the first player exhaust a hero to travel there" . gameTest $ \pid -> do
      outcrop <- stageLocation Locations.rockyOutcrop
      getModifiers GameTarget >>= (`shouldSatisfy` elem CannotBeReadiedByPlayerCards)
      eowyn <- hero "01007"
      heroes <- traverse hero ["01012", "01007", "01002"]
      for_ heroes (run . Exhaust)
      getModifiers outcrop >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ Ready eowyn
      getModifiers outcrop >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo outcrop
      getQuestion pid >>= traverse_ \_ -> chooseTarget pid eowyn
      exhausted "01007" `shouldReturn` True

    it "gives each exhausted character -1 willpower this phase with Weary Lands, and deals 1 damage to the defender with its shadow" . gameTest $ \pid -> do
      eowyn <- hero "01007"
      run $ Exhaust eowyn
      reveal [Treacheries.wearyLands]
      getWillpower eowyn `shouldReturn` 3
      threatOf pid `shouldReturn` 1
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      shadowAttack pid spider Treacheries.wearyLands beravor
      getDamage beravor `shouldReturn` 2

    it "has each player exhaust and damage a character with Painful Fatigue, and its shadow return the attacker after the attack" . gameTest $ \pid -> do
      beravor <- hero "01012"
      reveal [Treacheries.painfulFatigue]
      chooseTarget pid beravor
      exhausted "01012" `shouldReturn` True
      getDamage beravor `shouldReturn` 1
      spider <- engaged pid Enemies.forestSpider
      eowyn <- hero "01007"
      _ <- setEncounterDeck [Treacheries.painfulFatigue]
      run $ DealShadowCard spider
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      chooseTarget pid eowyn
      chooseLabel "$label.painfulFatigue.return" pid
      passActions pid
      placementOfEnemy spider `shouldReturn` StagingArea

    it "lets no player ready more than 6 characters in the refresh phase while An Arduous Journey is in play, and lets each ready a hero as it is defeated" . gameTest $ \pid -> do
      _ <- mainQuest
      reveal [Quests.anArduousJourney]
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (RefreshAtMost 6))
      eowyn <- hero "01007"
      run $ Exhaust eowyn
      q <- questOf Quests.anArduousJourney
      run $ PlaceTokens GameSource q Progress 6
      chooseTarget pid eowyn
      exhausted "01007" `shouldReturn` False
      victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Quests.anArduousJourney))

  describe "Servants of Sauron" do
    it "raises each player's threat by the active location's threat for Messenger from Mordor, when chosen" . gameTest $ \pid -> do
      _ <- activeLocation Locations.rollingPlains
      reveal [Enemies.messengerFromMordor]
      chooseLabelStarting "$label.messengerFromMordor.raiseThreat" pid
      threatOf pid `shouldReturn` 2

    it "has Messenger from Mordor attack the first player when chosen, and give +1 attack with its shadow" . gameTest $ \pid -> do
      reveal [Enemies.messengerFromMordor]
      _ <- setEncounterDeck [Locations.oldForestRoad]
      chooseLabel "$label.messengerFromMordor.attack" pid
      beravor <- hero "01012"
      passActions pid
      chooseTarget pid beravor
      passActions pid
      getDamage beravor `shouldReturn` 2
      spider <- engaged pid Enemies.forestSpider
      theodred <- hero "01002"
      shadowAttack pid spider Enemies.messengerFromMordor theodred
      -- 3 + 1 against Théodred's 1 defense
      getDamage theodred `shouldReturn` 3

    it "has Servant of Sauron engage the first player and attack after his threat is raised, once a phase" . gameTest $ \pid -> do
      reveal [Enemies.servantOfSauron, Locations.oldForestRoad]
      servant <- enemyOf Enemies.servantOfSauron
      placementOfEnemy servant `shouldReturn` EngagedWith pid
      locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ RaiseThreat pid 1
      eowyn <- hero "01007"
      passActions pid
      chooseTarget pid eowyn
      passActions pid
      getDamage eowyn `shouldReturn` 1
      run $ RaiseThreat pid 1
      getQuestion pid >>= (`shouldSatisfy` isNothing)
      run $ PlaceEnemy servant StagingArea
      placementOfEnemy servant `shouldReturn` EngagedWith pid

    it "keeps damage off the enemies engaged with the first player this round with Overwhelmed, or raises threat by 2 with its shadow" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      reveal [Treacheries.overwhelmed]
      run $ DealDamage GameSource (toTarget spider) 2
      getDamage (toTarget spider) `shouldReturn` 0
      run $ PlaceEnemy spider StagingArea
      patrol <- engaged pid Enemies.eastBightPatrol
      _ <- setEncounterDeck [Treacheries.overwhelmed]
      run $ DealShadowCard patrol
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      chooseTarget pid =<< hero "01012"
      threat <- threatOf pid
      chooseLabel "$label.overwhelmed.raiseThreat" pid
      threatOf pid `shouldReturn` (threat + 2)

    it "raises the threat of a player an enemy engages by 1 while Shadow of Fear is in play, and lowers each by 3 as it is defeated" . gameTest $ \pid -> do
      _ <- mainQuest
      reveal [Quests.shadowOfFearASitE, Locations.oldForestRoad]
      locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
      -- an enemy with no Forced effect of its own as it engages
      _ <- engaged pid Enemies.easterlingRaider
      threatOf pid `shouldReturn` 1
      run $ RaiseThreat pid 5
      q <- questOf Quests.shadowOfFearASitE
      run $ PlaceTokens GameSource q Progress 6
      threatOf pid `shouldReturn` 3

  describe "City of Rhûn" do
    it "has a player Rhûn Backstabber engages discard an ally of cost 2 or more, or raise his threat by 2" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- engaged pid Enemies.rhunBackstabber
      chooseLabel "$label.rhunBackstabber.discardAlly" pid
      alliesInPlay `shouldReturn` 0
      _ <- engaged pid Enemies.rhunBackstabber
      -- no such ally left: the threat it is
      chooseLabel "$label.rhunBackstabber.raiseThreat" pid
      threatOf pid `shouldReturn` 2

    it "raises the defending player's threat by 1 for each enemy engaged with him with Rhûn Backstabber's shadow" . gameTest $ \pid -> do
      _ <- engaged pid Enemies.eastBightPatrol
      spider <- engaged pid Enemies.forestSpider
      _ <- setEncounterDeck [Enemies.rhunBackstabber]
      run $ DealShadowCard spider
      inPhase CombatPhase
      run $ EnemyAttack pid spider
      passActions pid
      chooseTarget pid =<< hero "01012"
      passActions pid
      threatOf pid `shouldReturn` 2

    it "puts a resource on Treacherous Easterling as its player raises his threat, +1 attack and defense for each, to +3" . gameTest $ \pid -> do
      easterling <- engaged pid Enemies.treacherousEasterling
      replicateM_ 4 $ run $ RaiseThreat pid 1
      getEnemyAttack easterling `shouldReturn` 6
      getEnemyDefense easterling `shouldReturn` 4

    it "puts a resource on staged City Forum as a location is explored, +1 threat for each, which each player raises his threat by to travel there" . gameTest $ \pid -> do
      forum <- stageLocation Locations.cityForum
      road <- activeLocation Locations.oldForestRoad
      run $ PlaceTokens GameSource (toTarget road) Progress 3
      getLocationThreat forum `shouldReturn` 2
      run $ TravelTo forum
      threatOf pid `shouldReturn` 1

    it "puts one of the top 5 encounter cards under Seedy Tavern as the players travel there, revealing it as it is explored" . gameTest $ \pid -> do
      [spider, _] <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad]
      tavern <- stageLocation Locations.seedyTavern
      getModifiers tavern >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ TravelTo tavern
      chooseTarget pid (CardIdTarget spider.id)
      (.underneath) . toAttrs <$> getLocation tavern `shouldReturn` [EncounterCard spider]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.oldForestRoad]
      run $ PlaceTokens GameSource (toTarget tavern) Progress 5
      (placementOfEnemy =<< enemyOf Enemies.forestSpider) `shouldReturn` StagingArea
      victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Locations.seedyTavern))

    it "raises each player's threat by the active location's threat with Darkness in the East, surging with none" . gameTest $ \pid -> do
      reveal [Treacheries.darknessInTheEast, Locations.oldForestRoad]
      locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
      _ <- activeLocation Locations.rollingPlains
      reveal [Treacheries.darknessInTheEast]
      threatOf pid `shouldReturn` 2

    it "gives the attacking enemy +1 attack with Darkness in the East's shadow, +2 once the player's threat was raised this round" . gameTest $ \pid -> do
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      _ <- putAttachmentOn pid Attachments.citadelPlate beravor
      shadowAttack pid spider Treacheries.darknessInTheEast beravor
      getDamage beravor `shouldReturn` 2
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem RaisedThreatThisRound)
      run $ RaiseThreat pid 1
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem RaisedThreatThisRound)
      run $ Ready beravor
      shadowAttack pid spider Treacheries.darknessInTheEast beravor
      -- 3 + 2 against Beravor's 2 defense
      getDamage beravor `shouldReturn` 5

  describe "Under Guard" do
    it "keeps progress off the other staged locations while Side Street is staged, and finds a location from the shuffled discard pile to travel there" . gameTest $ \_ -> do
      street <- stageLocation Locations.sideStreet
      road <- stageLocation Locations.oldForestRoad
      hid <- toId <$> getHeroWithCode "01012"
      run $ PlaceTokens (HeroSource hid) (toTarget road) Progress 2
      (.progress) . toAttrs <$> getLocation road `shouldReturn` 0
      _ <- setEncounterDeck []
      _ <- discarded Locations.rollingPlains
      run $ TravelTo street
      (.placement) <$> (getLocation =<< locationOf Locations.rollingPlains) `shouldReturn` StagingArea

    it "keeps damage off staged enemies while Catacombs is staged, and engages each player with the enemy he discards to travel there" . gameTest $ \pid -> do
      catacombs <- stageLocation Locations.catacombs
      spider <- stage Enemies.forestSpider
      run $ DealDamage GameSource (toTarget spider) 1
      getDamage (toTarget spider) `shouldReturn` 0
      _ <- setEncounterDeck [Enemies.eastBightPatrol]
      run $ TravelTo catacombs
      (placementOfEnemy =<< enemyOf Enemies.eastBightPatrol) `shouldReturn` EngagedWith pid

    it "resolves the active location's Travel effect with Under Guard, the players not traveling, or finds a location with none" . gameTest $ \pid -> do
      plains <- activeLocation Locations.rollingPlains
      reveal [Treacheries.underGuard]
      threatOf pid `shouldReturn` 2
      (.placement) <$> getLocation plains `shouldReturn` ActiveLocation
      run $ Discard GameSource (toTarget plains)
      reveal [Treacheries.underGuard, Enemies.forestSpider, Locations.oldForestRoad]
      (.placement) <$> (getLocation =<< locationOf Locations.oldForestRoad) `shouldReturn` StagingArea
      discardCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.forestSpider))

    it "resolves the active location's Travel effect with Under Guard's shadow when the attack destroys a character" . gameTest $ \pid -> do
      _ <- activeLocation Locations.rollingPlains
      guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      spider <- engaged pid Enemies.forestSpider
      shadowAttack pid spider Treacheries.underGuard guardian
      alliesInPlay `shouldReturn` 0
      threatOf pid `shouldReturn` 2

    it "gives staged locations +1 threat while Unwelcome Travelers is in play, and engages the first player with an enemy as it becomes the current quest" . gameTest $ \pid -> do
      _ <- mainQuest
      road <- stageLocation Locations.oldForestRoad
      reveal [Quests.unwelcomeTravelers]
      getLocationThreat road `shouldReturn` 2
      q <- questOf Quests.unwelcomeTravelers
      _ <- setEncounterDeck [Locations.rollingPlains, Enemies.forestSpider]
      run $ SetCurrentQuest q
      (placementOfEnemy =<< enemyOf Enemies.forestSpider) `shouldReturn` EngagedWith pid
      discardCodes `shouldReturn` [toCardCode Locations.rollingPlains]

  describe "Ulchor's Guard" do
    it "has Ulchor's Guard engage the player Thane Ulchor is engaged with" $ withEngageableThane \thane pid -> do
      t <- stage thane
      run $ PlaceEnemy t (EngagedWith pid)
      guard' <- stage Enemies.ulchorsGuard
      placementOfEnemy guard' `shouldReturn` EngagedWith pid
      run $ PlaceEnemy guard' StagingArea
      placementOfEnemy guard' `shouldReturn` EngagedWith pid

    it "leaves Ulchor's Guard in the staging area without Thane Ulchor engaged, and gives +2 defense with its shadow" . gameTest $ \pid -> do
      guard' <- stage Enemies.ulchorsGuard
      placementOfEnemy guard' `shouldReturn` StagingArea
      spider <- engaged pid Enemies.forestSpider
      shadowAttack pid spider Enemies.ulchorsGuard =<< hero "01012"
      getEnemyDefense spider `shouldReturn` 3

    it "has Thane Ulchor attack the first player with Ulchor's Rage" $ withThane \thane pid -> do
      t <- stage thane
      _ <- setEncounterDeck [Treacheries.ulchorsRage, Locations.oldForestRoad, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      -- the action window before defenders are declared, then the defender against him
      passActions pid
      Just (QuestionLabel _ source _ _) <- getQuestion pid
      source `shouldBe` Just (toTarget t)

    it "has Ulchor's Rage surge with no Thane Ulchor to attack, and deal 1 damage to the defender with its shadow" . gameTest $ \pid -> do
      reveal [Treacheries.ulchorsRage, Locations.oldForestRoad]
      locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      shadowAttack pid spider Treacheries.ulchorsRage beravor
      getDamage beravor `shouldReturn` 2

    it "attaches Fanaticism to the lowest defense Easterling enemy without one, making it stronger and immune, or surges" . gameTest $ \_ -> do
      raider <- stage Enemies.easterlingRaider
      warrior <- stage Enemies.warriorOfRhun
      reveal [Treacheries.fanaticism]
      attachedTo raider `shouldReturn` [toCardCode Treacheries.fanaticism]
      getEnemyThreat raider `shouldReturn` 3
      getModifiers raider >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      fanatic <- treacheryOf Treacheries.fanaticism
      getModifiers fanatic >>= (`shouldSatisfy` elem (GainTrait Condition))
      run $ Discard GameSource (EnemyTarget raider)
      run $ Discard GameSource (EnemyTarget warrior)
      reveal [Treacheries.fanaticism, Locations.oldForestRoad]
      locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]

  describe "Twisted Tunnels" do
    it "adds the first enemy discarded to the staging area as Twisted Tunnel is revealed, and the top card if an enemy with its shadow" . gameTest $ \pid -> do
      reveal [Locations.twistedTunnel, Locations.oldForestRoad, Enemies.forestSpider]
      (placementOfEnemy =<< enemyOf Enemies.forestSpider) `shouldReturn` StagingArea
      discardCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
      patrol <- engaged pid Enemies.eastBightPatrol
      _ <- setEncounterDeck [Locations.twistedTunnel, Enemies.forestSpider]
      run $ DealShadowCard patrol
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      chooseTarget pid =<< hero "01012"
      passActions pid
      getsGame (length . filter ((== toCardCode Enemies.forestSpider) . toCardCode) . toList . (.entities.enemies)) `shouldReturn` 2

    it "lets each player play no more than 1 card a round while Narrow Opening is active, and has each discard a random card as it leaves play" . gameTest $ \pid -> do
      opening <- activeLocation Locations.narrowOpening
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem (CannotPlay AnyCard))
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [PlayedCardThisRound (toCardCode Allies.guardOfTheCitadel) EventType] UntilEndOfRound
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (CannotPlay AnyCard))
      _ <- putCardIntoHand pid Allies.guardOfTheCitadel
      was <- handSize pid
      run $ Discard GameSource (toTarget opening)
      handSize pid `shouldReturn` (was - 1)

    it "makes Crumbling Passage the active location as it is revealed, and damages each exhausted character as it leaves play" . gameTest $ \_ -> do
      road <- activeLocation Locations.oldForestRoad
      reveal [Locations.crumblingPassage]
      passage <- locationOf Locations.crumblingPassage
      (.placement) <$> getLocation passage `shouldReturn` ActiveLocation
      (.placement) <$> getLocation road `shouldReturn` StagingArea
      eowyn <- hero "01007"
      run $ Exhaust eowyn
      run $ Discard GameSource (toTarget passage)
      getDamage eowyn `shouldReturn` 1
      (getDamage =<< hero "01012") `shouldReturn` 0

    it "blanks each character's text box this round with Fearful Shadows, and keeps the defender from readying with its shadow" . gameTest $ \pid -> do
      reveal [Treacheries.fearfulShadows]
      eowyn <- hero "01007"
      getModifiers eowyn >>= (`shouldSatisfy` elem Blank)
      spider <- engaged pid Enemies.forestSpider
      beravor <- hero "01012"
      shadowAttack pid spider Treacheries.fearfulShadows beravor
      getModifiers beravor >>= (`shouldSatisfy` elem CannotReady)

    it "puts the top 10 cards of each deck under the threat dial with Lost in the Dark, discarding one as threat is raised, and shuffling them back as it is defeated" . gameTest $ \pid -> do
      _ <- mainQuest
      deck <- deckSize pid
      reveal [Quests.lostInTheDark]
      deckSize pid `shouldReturn` (deck - 10)
      length . (.underThreatDial) <$> getPlayer pid `shouldReturn` 10
      run $ RaiseThreat pid 1
      length . (.underThreatDial) <$> getPlayer pid `shouldReturn` 9
      length . (.discard) <$> getPlayer pid `shouldReturn` 1
      q <- questOf Quests.lostInTheDark
      run $ PlaceTokens GameSource q Progress 8
      length . (.underThreatDial) <$> getPlayer pid `shouldReturn` 0
      deckSize pid `shouldReturn` (deck - 1)
