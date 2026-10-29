module LordOfTheRings.Dwarrowdelf.PlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.ActiveCost (playCard)
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Matcher.Window (WindowMatcher (EventPlayedWindow), windowMatches)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getAttackStrength, getDefense, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Matcher (getEnemy, hasKeyword, selectAny)
import LordOfTheRings.Matcher.Types (EnemyMatcher (EnemyWithEngagementCostAboveThreatOf, EnemyWithId))
import LordOfTheRings.Sphere (Sphere (Leadership, Lore))
import LordOfTheRings.Message.Lifted (cancelEvent)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase, RefreshPhase, ResourcePhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioVictoryDisplay))
import LordOfTheRings.Window
import TestImport

-- Beravor (Lore), Éowyn (Spirit) and Théodred (Leadership): the test deck's heroes.
beravor, eowyn, theodred :: CardCode
beravor = "01012"
eowyn = "01007"
theodred = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

putHero :: PlayerId -> CardDef -> GameT Target
putHero pid def = do
  run . PutHeroIntoPlay pid =<< genPlayerCard def (Just pid)
  heroOf pid (toCardCode def)

-- | A stat once the modifiers in play are counted.
stat :: (Target -> GameT Int) -> Target -> GameT Int
stat f t = run (Run []) >> f t

-- | An enemy added to the staging area.
staged :: CardDef -> GameT EnemyId
staged def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))
    <&> toId

-- | A location added to the staging area.
location :: CardDef -> GameT LocationId
location def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations))
    <&> toId

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

setPhase :: Phase -> GameT ()
setPhase p = putGame . (phaseL .~ p) =<< getGame

giveResources :: CardCode -> Int -> GameT ()
giveResources code n = do
  h <- hero code
  run $ PlaceTokens GameSource h Resource n

resourcesOf :: Target -> GameT Int
resourcesOf = \case
  HeroTarget hid -> getHeroResources hid
  _ -> pure 0

handOf :: PlayerId -> GameT [Card]
handOf pid = (.hand) <$> getPlayer pid

-- | A card of the player's put on top of their deck.
onTopOfDeck :: PlayerId -> CardDef -> GameT Card
onTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (card :)) =<< getGame
  pure card

inDiscard :: PlayerId -> CardDef -> GameT Card
inDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . discardL %~ (card :)) =<< getGame
  pure card

-- | The resource tokens on an attachment.
attachmentResources :: Target -> GameT Int
attachmentResources t =
  getsGame (maybe 0 (countTokens Resource . attachmentTokens . toAttrs) . find ((== t) . toTarget) . toList . (.entities.attachments))

-- | Open the player's action window.
actions :: PlayerId -> GameT ()
actions = run . ActionWindow

-- | Answer the open question with its nth choice.
chooseNth :: PlayerId -> Int -> GameT ()
chooseNth pid = void . answerQuestion pid

isInPlay :: Target -> GameT Bool
isInPlay t = do
  es <- getsGame (.entities)
  pure $ case t of
    AllyTarget aid -> Map.member aid es.allies
    EnemyTarget eid -> Map.member eid es.enemies
    LocationTarget lid -> Map.member lid es.locations
    AttachmentTarget aid -> Map.member aid es.attachments
    _ -> True

victoryDisplay :: GameT [CardId]
victoryDisplay = getsGame (map (.id) . scenarioVictoryDisplay . toAttrs . (.scenario))

spec :: Spec
spec = describe "Dwarrowdelf player cards" do
  it "Secrecy lowers a card's cost while its player's threat is 20 or lower" . gameTest $ \pid -> do
    card <- PlayerCard <$> genPlayerCard Allies.dunedainWanderer (Just pid)
    setThreat pid 21
    costModifier pid card `shouldReturn` 0
    setThreat pid 20
    costModifier pid card `shouldReturn` -3

  it "Elrohir gets +2 defense with Elladan, and pays 1 to ready after defending" . gameTest $ \pid -> do
    elrohir <- putHero pid Heroes.elrohir
    base <- stat getDefense elrohir
    _ <- putHero pid Heroes.elladan
    stat getDefense elrohir `shouldReturn` base + 2
    eid <- staged Enemies.hillTroll
    run $ PlaceTokens GameSource elrohir Resource 1
    run $ Exhaust elrohir
    run $ CheckWindows [mkAfter $ DeclaredAsDefender elrohir eid]
    chooseAbilityOf pid elrohir
    resourcesOf elrohir `shouldReturn` 0
    elrohirH <- getHeroWithCode (toCardCode Heroes.elrohir)
    heroExhausted (toAttrs elrohirH) `shouldBe` False

  it "Elladan gets +2 attack with Elrohir, and pays 1 to ready after attacking" . gameTest $ \pid -> do
    elladan <- putHero pid Heroes.elladan
    base <- stat getAttack elladan
    _ <- putHero pid Heroes.elrohir
    stat getAttack elladan `shouldReturn` base + 2
    eid <- staged Enemies.hillTroll
    run $ PlaceTokens GameSource elladan Resource 1
    run $ Exhaust elladan
    run $ CheckWindows [mkAfter $ DeclaredAsAttacker elladan eid]
    chooseAbilityOf pid elladan
    resourcesOf elladan `shouldReturn` 0

  it "Taking Initiative draws 2 and deals 2 damage when the discarded card costs enough" . gameTest $ \pid -> do
    eid <- staged Enemies.hillTroll
    _ <- onTopOfDeck pid Allies.bofur
    _ <- playWith pid theodred 0 Events.takingInitiative
    chooseTarget pid eid
    length <$> handOf pid `shouldReturn` 2
    getDamage (toTarget eid) `shouldReturn` 2

  it "Timely Aid puts an ally from the top 5 into play, for 1 with secrecy" . gameTest $ \pid -> do
    setThreat pid 20
    ally <- onTopOfDeck pid Allies.erestor
    _ <- playWith pid theodred 1 Events.timelyAid
    chooseTarget pid (CardIdTarget ally.id)
    getsGame (any ((== ally.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` True
    (resourcesOf =<< hero theodred) `shouldReturn` 0

  -- Gimli gives the 0 cost Tactics event its resource match
  it "Unseen Strike gives +3 attack against an enemy whose engagement cost is above your threat" . gameTestWith testDeck {heroes = [beravor, "01004", theodred]} $ \pid -> do
    setThreat pid 5
    eid <- staged Enemies.wolfRider
    b <- hero beravor
    base <- getAttack b
    _ <- playWith pid "01004" 0 Events.unseenStrike
    chooseTarget pid b
    def <- toCardDef <$> getEnemy eid
    getAttackStrength [b] eid def `shouldReturn` base + 3
    selectAny (EnemyWithId eid <> EnemyWithEngagementCostAboveThreatOf pid) `shouldReturn` True

  it "Keeping Count counts another copy's extra tokens, and gains one when its hero destroys an enemy" . gameTest $ \pid -> do
    b <- hero beravor
    e <- hero eowyn
    countB <- putAttachmentOn pid Attachments.keepingCount b
    _ <- putAttachmentOn pid Attachments.keepingCount e
    baseB <- stat getAttack b
    baseE <- stat getAttack e
    run $ PlaceTokens GameSource countB Resource 2
    stat getAttack e `shouldReturn` baseE + 2
    stat getAttack b `shouldReturn` baseB
    eid <- staged Enemies.wolfRider
    run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [b] mempty]
    attachmentResources countB `shouldReturn` 3

  it "Bofur's Quest Action puts him into play committed, and he returns to hand if the quest succeeds" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Allies.bofur
    giveResources eowyn 1
    setPhase QuestPhase
    actions pid
    chooseTarget pid (CardIdTarget card.id)
    passActions pid
    bofur <- getsGame (fromJustNote "no Bofur" . find ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies))
    allyCommitted (toAttrs bofur) `shouldBe` True
    allyExhausted (toAttrs bofur) `shouldBe` True
    (resourcesOf =<< hero eowyn) `shouldReturn` 0
    run $ CheckWindows [mkAfter QuestedSuccessfully]
    map (.id) <$> handOf pid `shouldReturn` [card.id]

  it "Bofur put into play after the players quested successfully returns to hand at once" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Allies.bofur
    giveResources eowyn 1
    setPhase QuestPhase
    run $ CreateEffect $ Effect GameSource GameTarget [QuestedSuccessfullyThisPhase] UntilEndOfPhase
    actions pid
    chooseTarget pid (CardIdTarget card.id)
    passActions pid
    map (.id) <$> handOf pid `shouldReturn` [card.id]

  it "marks the phase in which the players quest successfully, as Bofur reads it" . gameTest $ \_ -> do
    setPhase QuestPhase
    run . CommitToQuest =<< hero eowyn
    run QuestResolution
    getModifiers GameTarget >>= (`shouldSatisfy` elem QuestedSuccessfullyThisPhase)

  it "Renewed Friendship lowers the threat of a player who attached a card to your hero" . twoPlayerTest (coreDeck "spirit") $ \one two -> do
    rf <- putCardIntoHand one Events.renewedFriendship
    b <- heroOf one beravor
    att <- putAttachmentOn two Attachments.ringMail b
    attCard <- getsGame (fromJustNote "no attachment" . find ((== att) . toTarget) . toList . (.entities.attachments))
    setThreat two 10
    run
      $ CheckWindows
        [mkAfter $ PlayedFromHand (CardIdTarget $ attachmentCardId $ toAttrs attCard), mkAfter $ PlayedCardFromHand two (toCardCode Attachments.ringMail)]
    chooseTarget one (CardIdTarget rf.id)
    chooseLabel "$label.renewedFriendship.lowerThreat" one
    threat two `shouldReturn` 8

  it "Ravenhill Scout moves 2 progress from one location to another" . gameTest $ \pid -> do
    from <- location Locations.mountainsOfMirkwood
    dest <- location Locations.mountainsOfMirkwood
    run $ PlaceTokens GameSource (toTarget from) Progress 2
    scout <- putAllyIntoPlay pid Allies.ravenhillScout
    actions pid
    chooseAbilityOf pid scout
    chooseTarget pid from
    chooseTarget pid dest
    chooseNth pid 1
    (.progress) <$> getLocation from `shouldReturn` 0
    (.progress) <$> getLocation dest `shouldReturn` 2

  it "Needful to Know raises your threat by 1, then lowers it by the top encounter card's threat" . gameTest $ \pid -> do
    setThreat pid 20
    _ <- setEncounterDeck [Locations.mountainsOfMirkwood]
    _ <- playWith pid beravor 0 Events.needfulToKnow
    chooseLabel "$label.continue" pid
    threat pid `shouldReturn` 21 - 2

  it "Good Meal lowers the next event of its hero's sphere by 2, to 0" . gameTest $ \pid -> do
    setThreat pid 30
    frodo <- putHero pid Heroes.frodoBaggins
    meal <- putAttachmentOn pid Attachments.goodMeal frodo
    actions pid
    chooseAbilityOf pid meal
    isInPlay meal `shouldReturn` False
    sight <- PlayerCard <$> genPlayerCard Events.outOfSight (Just pid)
    costModifier pid sight `shouldReturn` -2
    cheap <- PlayerCard <$> genPlayerCard Events.renewedFriendship (Just pid)
    costModifier pid cheap `shouldReturn` -2

  it "Good Meal lowers an event of a sphere its hero has gained" . gameTest $ \pid -> do
    setThreat pid 30
    frodo <- putHero pid Heroes.frodoBaggins
    run $ CreateEffect $ Effect GameSource frodo [GainResourceIcon Lore] UntilEndOfPhase
    meal <- putAttachmentOn pid Attachments.goodMeal frodo
    actions pid
    chooseAbilityOf pid meal
    runes <- PlayerCard <$> genPlayerCard Events.daeronsRunes (Just pid)
    costModifier pid runes `shouldReturn` -2

  it "Longbeard Elder's progress goes on the active location before the quest" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.mountainsOfMirkwood
    _ <- setEncounterDeck [Locations.mountainsOfMirkwood]
    elder <- putAllyIntoPlay pid Allies.longbeardElder
    run $ CheckWindows [mkAfter $ CommittedToQuest elder]
    chooseAbilityOf pid elder
    chooseLabel "$label.continue" pid
    (.progress) <$> getActiveQuest `shouldReturn` 0
    getsGame (map (.progress) . filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations)) `shouldReturn` [1]

  it "Rivendell Blade gives the enemy its character attacks -2 defense" . gameTest $ \pid -> do
    elrohir <- putHero pid Heroes.elrohir
    _ <- putAttachmentOn pid Attachments.rivendellBlade elrohir
    eid <- staged Enemies.hillTroll
    run $ CheckWindows [mkAfter $ DeclaredAsAttacker elrohir eid]
    getEnemyDefense eid `shouldReturn` 1

  it "Hail of Stones exhausts X characters for X damage to an enemy in the staging area" . gameTest $ \pid -> do
    elladan <- putHero pid Heroes.elladan
    run $ PlaceTokens GameSource elladan Resource 1
    eid <- staged Enemies.hillTroll
    _ <- playWith pid (toCardCode Heroes.elladan) 0 Events.hailOfStones
    chooseTarget pid =<< hero beravor
    chooseTarget pid =<< hero eowyn
    chooseLabel "$label.hailOfStones.done" pid
    chooseTarget pid eid
    getDamage (toTarget eid) `shouldReturn` 2

  it "Rider of the Mark changes control, then discards a shadow card on its new controller's enemy" . twoPlayerTest (coreDeck "spirit") $ \one two -> do
    rider <- putAllyIntoPlay one Allies.riderOfTheMark
    e <- heroOf one eowyn
    run $ PlaceTokens GameSource e Resource 1
    eid <- staged Enemies.wolfRider
    run $ EngageEnemy two eid
    shadow <- genEncounterCard Locations.mountainsOfMirkwood
    run $ AttachShadowCard eid shadow
    actions one
    chooseAbilityOf one rider
    chooseTarget one two
    chooseAbilityOf two rider
    chooseTarget two shadow
    (.shadowCards) <$> getEnemy eid `shouldReturn` []
    map (.id) <$> getEncounterDiscard `shouldReturn` [shadow.id]

  it "Song of Eärendil draws a card, and moves another player's threat raise onto you" . twoPlayerTest (coreDeck "spirit") $ \one two -> do
    e <- heroOf one eowyn
    song <- putAttachmentOn one Attachments.songOfEarendil e
    run $ CheckWindows [mkAfter $ EntersPlay song]
    chooseAbilityOf one song
    length <$> handOf one `shouldReturn` 1
    oneWas <- threat one
    twoWas <- threat two
    run $ RaiseThreat two 3
    chooseAbilityOf one song
    threat one `shouldReturn` oneWas + 1
    threat two `shouldReturn` twoWas + 2

  it "Out of the Wild adds an encounter card and itself to the victory display" . gameTest $ \pid -> do
    cards <- setEncounterDeck [Locations.mountainsOfMirkwood, Enemies.hillTroll]
    card <- playWith pid beravor 3 Events.outOfTheWild
    chooseTarget pid (CardIdTarget $ (fromJustNote "no card" $ headMay cards).id)
    vd <- victoryDisplay
    sort vd `shouldBe` sort [card.id, (fromJustNote "no card" $ headMay cards).id]
    length <$> getEncounterDeck `shouldReturn` 1

  it "The End Comes shuffles the encounter discard pile into the deck after a Dwarf leaves play" . gameTest $ \pid -> do
    discarded <- genEncounterCard Locations.mountainsOfMirkwood
    run $ AddToEncounterDiscard [discarded]
    card <- putCardIntoHand pid Events.theEndComes
    bofur <- putAllyIntoPlay pid Allies.bofur
    run $ Discard bofur
    chooseTarget pid (CardIdTarget card.id)
    getEncounterDiscard `shouldReturn` []
    map (.id) <$> getEncounterDeck `shouldReturn` [discarded.id]

  it "Aragorn's Refresh Action reduces your threat to your starting threat" . gameTest $ \pid -> do
    aragorn <- putHero pid Heroes.aragornLore
    putGame . (playersL . ix pid . startingThreatL .~ 25) =<< getGame
    setThreat pid 33
    setPhase RefreshPhase
    actions pid
    chooseAbilityOf pid aragorn
    threat pid `shouldReturn` 25

  it "Grave Cairn adds a character's attack to another's after it leaves play" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Events.graveCairn
    giveResources theodred 1
    wanderer <- putAllyIntoPlay pid Allies.dunedainWanderer
    b <- hero beravor
    base <- stat getAttack b
    run $ Discard wanderer
    chooseTarget pid (CardIdTarget card.id)
    chooseTarget pid b
    getAttack b `shouldReturn` base + 2

  it "Sword that was Broken gives a leadership icon, and +1 willpower to your characters on Aragorn" . gameTest $ \pid -> do
    aragorn <- putHero pid Heroes.aragornLore
    b <- hero beravor
    base <- stat getWillpower b
    _ <- putAttachmentOn pid Attachments.swordThatWasBroken aragorn
    getModifiers aragorn >>= (`shouldSatisfy` elem (GainResourceIcon Leadership))
    getWillpower b `shouldReturn` base + 1

  it "Watcher of the Bruinen does not exhaust to defend, and discards a card from hand after it defends" . gameTest $ \pid -> do
    watcher <- putAllyIntoPlay pid Allies.watcherOfTheBruinen
    getModifiers watcher >>= (`shouldSatisfy` elem DoesNotExhaustToDefend)
    _ <- putCardIntoHand pid Events.unseenStrike
    eid <- staged Enemies.hillTroll
    run $ CheckWindows [mkAfter $ DefendedAttack watcher eid]
    chooseLabel "$label.discardOneFromHand" pid
    chooseNth pid 0
    handOf pid `shouldReturn` []
    isInPlay watcher `shouldReturn` True

  it "Trollshaw Scout does not exhaust to attack, and is discarded after it attacks with no cards in hand" . gameTest $ \pid -> do
    scout <- putAllyIntoPlay pid Allies.trollshawScout
    getModifiers scout >>= (`shouldSatisfy` elem DoesNotExhaustToAttack)
    eid <- staged Enemies.hillTroll
    run $ CheckWindows [mkAfter $ AttackedEnemy scout eid]
    isInPlay scout `shouldReturn` False

  it "Rivendell Bow gives ranged, and +1 attack on a ranged attack to a character with printed ranged" . gameTest $ \pid -> do
    elrohir <- putHero pid Heroes.elrohir
    _ <- putAttachmentOn pid Attachments.rivendellBow elrohir
    hasKeyword Ranged elrohir (toCardDef Heroes.elrohir) `shouldReturn` True
    scout <- putAllyIntoPlay pid Allies.trollshawScout
    _ <- putAttachmentOn pid Attachments.rivendellBow scout
    eid <- staged Enemies.hillTroll
    def <- toCardDef <$> getEnemy eid
    getAttackStrength [scout] eid def `shouldReturn` 3

  it "Arwen Undómiel gives a character sentinel and +1 defense after she exhausts" . gameTest $ \pid -> do
    arwen <- putAllyIntoPlay pid Allies.arwenUndomiel
    b <- hero beravor
    base <- stat getDefense b
    run $ CheckWindows [mkAfter $ Exhausted arwen]
    chooseAbilityOf pid arwen
    chooseTarget pid b
    getDefense b `shouldReturn` base + 1
    getModifiers b >>= (`shouldSatisfy` elem (GainKeyword Sentinel))

  it "Short Cut shuffles a location that entered play back, and reveals another" . gameTest $ \pid -> do
    frodo <- putHero pid Heroes.frodoBaggins
    card <- putCardIntoHand pid Events.shortCut
    giveResources beravor 1
    _ <- setEncounterDeck [Enemies.hillTroll]
    lid <- location Locations.mountainsOfMirkwood
    chooseTarget pid (CardIdTarget card.id)
    chooseTarget pid frodo
    isInPlay (toTarget lid) `shouldReturn` False
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    length <$> getEncounterDeck `shouldReturn` 1

  it "Resourceful's hero collects an extra resource" . gameTest $ \pid -> do
    b <- hero beravor
    _ <- putAttachmentOn pid Attachments.resourceful b
    run $ BeginPhase ResourcePhase
    resourcesOf b `shouldReturn` 2

  it "Háma returns a tactics event from the discard pile, then a card is discarded" . gameTest $ \pid -> do
    hama <- putHero pid Heroes.hama
    event <- inDiscard pid Events.hailOfStones
    other <- putCardIntoHand pid Events.unseenStrike
    eid <- staged Enemies.hillTroll
    run $ CheckWindows [mkAfter $ DeclaredAsAttacker hama eid]
    chooseAbilityOf pid hama
    chooseTarget pid (CardIdTarget event.id)
    chooseTarget pid (CardIdTarget other.id)
    map (.id) <$> handOf pid `shouldReturn` [event.id]

  it "Erestor discards a card to draw a card" . gameTest $ \pid -> do
    erestor <- putAllyIntoPlay pid Allies.erestor
    card <- putCardIntoHand pid Events.unseenStrike
    actions pid
    chooseAbilityOf pid erestor
    chooseTarget pid (CardIdTarget card.id)
    hand <- handOf pid
    length hand `shouldBe` 1
    map (.id) hand `shouldSatisfy` notElem card.id

  it "Fresh Tracks deals 1 damage to an enemy added to the staging area, which is spared engagement checks" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Events.freshTracks
    giveResources theodred 1
    eid <- staged Enemies.hillTroll
    chooseTarget pid (CardIdTarget card.id)
    getDamage (toTarget eid) `shouldReturn` 1
    getModifiers eid >>= (`shouldSatisfy` elem NoEngagementChecks)

  it "Erebor Battle Master gets +1 attack for each other Dwarf ally" . gameTest $ \pid -> do
    master <- putAllyIntoPlay pid Allies.ereborBattleMaster
    base <- stat getAttack master
    _ <- putAllyIntoPlay pid Allies.bofur
    stat getAttack master `shouldReturn` base + 1

  it "Ring Mail gives +1 hit point and +1 defense" . gameTest $ \pid -> do
    frodo <- putHero pid Heroes.frodoBaggins
    hp <- stat getHitPoints frodo
    def <- stat getDefense frodo
    _ <- putAttachmentOn pid Attachments.ringMail frodo
    getHitPoints frodo `shouldReturn` hp + 1
    getDefense frodo `shouldReturn` def + 1

  it "Out of Sight stops enemies engaged with you attacking you this phase" . gameTest $ \pid -> do
    eid <- staged Enemies.wolfRider
    run $ EngageEnemy pid eid
    _ <- playWith pid eowyn 5 Events.outOfSight
    later <- staged Enemies.hillTroll
    run $ EngageEnemy pid later
    -- one engaged after it was played is stopped too: neither attacks, so the player is
    -- not asked which attacks next and is left in the phase's action window
    run $ EnemyAttacks pid
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldBe` "$label.planningActions"

  it "Ever My Heart Rises readies its Dwarf and lowers your threat after travel to a Mountain" . gameTest $ \pid -> do
    bofur <- putAllyIntoPlay pid Allies.bofur
    heart <- putAttachmentOn pid Attachments.everMyHeartRises bofur
    run $ Exhaust bofur
    lid <- location Locations.mountainsOfMirkwood
    setThreat pid 10
    let was = 10
    run $ TravelTo lid
    chooseAbilityOf pid heart
    threat pid `shouldReturn` was - 1
    getsGame (any (\a -> toTarget a == bofur && not (allyExhausted $ toAttrs a)) . toList . (.entities.allies))
      `shouldReturn` True

  it "Warden of Healing heals 1 damage on up to 2 characters" . gameTest $ \pid -> do
    warden <- putAllyIntoPlay pid Allies.wardenOfHealing
    b <- hero beravor
    e <- hero eowyn
    run $ PlaceTokens GameSource b Damage 2
    run $ PlaceTokens GameSource e Damage 1
    actions pid
    chooseAbilityOf pid warden
    chooseTarget pid b
    chooseTarget pid e
    getDamage b `shouldReturn` 1
    getDamage e `shouldReturn` 0

  it "Word of Command exhausts an Istari to search the deck for a card" . gameTest $ \pid -> do
    gandalf <- putAllyIntoPlay pid Allies.gandalf
    -- Gandalf's own response to entering play goes unused
    chooseLabel "$label.pass" pid
    wanted <- onTopOfDeck pid Events.riskSomeLight
    _ <- playWith pid beravor 1 Events.wordOfCommand
    chooseTarget pid gandalf
    chooseTarget pid (CardIdTarget wanted.id)
    map (.id) <$> handOf pid >>= (`shouldSatisfy` elem wanted.id)

  it "Love of Tales adds a resource to its hero after a Song is played" . gameTest $ \pid -> do
    b <- hero beravor
    love <- putAttachmentOn pid Attachments.loveOfTales b
    run $ CheckWindows [mkAfter $ PlayedCard pid (toCardCode Attachments.songOfEarendil)]
    chooseAbilityOf pid love
    resourcesOf b `shouldReturn` 1

  it "Glorfindel raises your threat when he exhausts to commit, but not with Light of Valinor" . gameTest $ \pid -> do
    glorfindel <- putHero pid Heroes.glorfindelSpirit
    was <- threat pid
    run $ CommitToQuest glorfindel
    run $ CheckWindows [mkAfter $ CommittedToQuest glorfindel]
    threat pid `shouldReturn` was + 1
    run $ Ready glorfindel
    run $ RemoveFromQuest glorfindel
    _ <- putAttachmentOn pid Attachments.lightOfValinor glorfindel
    run $ CommitToQuest glorfindel
    run $ CheckWindows [mkAfter $ CommittedToQuest glorfindel]
    threat pid `shouldReturn` was + 1

  it "Heavy Stroke doubles only what the Dwarf's attack deals, the rest beating the defense" . gameTest $ \pid -> do
    elladan <- putHero pid Heroes.elladan
    card <- putCardIntoHand pid Events.heavyStroke
    run $ PlaceTokens GameSource elladan Resource 1
    bofur <- putAllyIntoPlay pid Allies.bofur
    eid <- staged Enemies.hillTroll
    setPhase CombatPhase
    dwarf <- getAttack bofur
    run $ CheckWindows [mkAfter $ AttackDealtDamage eid [toTarget elladan, bofur] (dwarf + 5)]
    chooseTarget pid (CardIdTarget card.id)
    getDamage (toTarget eid) `shouldReturn` dwarf

  it "Heavy Stroke deals an attack's damage again when a Dwarf made it" . gameTest $ \pid -> do
    elladan <- putHero pid Heroes.elladan
    card <- putCardIntoHand pid Events.heavyStroke
    run $ PlaceTokens GameSource elladan Resource 1
    bofur <- putAllyIntoPlay pid Allies.bofur
    eid <- staged Enemies.hillTroll
    setPhase CombatPhase
    dwarf <- getAttack bofur
    run $ CheckWindows [mkAfter $ AttackDealtDamage eid [bofur] dwarf]
    chooseTarget pid (CardIdTarget card.id)
    getDamage (toTarget eid) `shouldReturn` dwarf

  it "Heavy Stroke: no more than 1 copy is played each phase, by the whole group" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
    elladan <- putHero one Heroes.elladan
    run $ PlaceTokens GameSource elladan Resource 1
    gimli <- heroOf two "01004"
    run $ PlaceTokens GameSource gimli Resource 1
    mine <- putCardIntoHand one Events.heavyStroke
    theirs <- putCardIntoHand two Events.heavyStroke
    eid <- staged Enemies.hillTroll
    setPhase CombatPhase
    run $ CheckWindows [mkAfter $ AttackDealtDamage eid [gimli] 2]
    chooseTarget one (CardIdTarget mine.id)
    run $ CheckWindows [mkAfter $ AttackDealtDamage eid [gimli] 2]
    question <- getQuestion two
    [() | Just q <- [question], TargetLabel (CardIdTarget cid) _ <- questionChoices q, cid == theirs.id] `shouldBe` []

  it "Imladris Stargazer lets a player put the top 5 cards of their deck back in any order" . gameTest $ \pid -> do
    star <- putAllyIntoPlay pid Allies.imladrisStargazer
    top <- take 5 . (.deck) <$> getPlayer pid
    actions pid
    chooseAbilityOf pid star
    replicateM_ 5 (chooseNth pid 0)
    map (.id) . take 5 . (.deck) <$> getPlayer pid `shouldReturn` reverse (map (.id) top)

  it "Asfaloth places 2 progress when Glorfindel rides it" . gameTest $ \pid -> do
    glorfindel <- putHero pid Heroes.glorfindelSpirit
    horse <- putAttachmentOn pid Attachments.asfaloth glorfindel
    lid <- location Locations.mountainsOfMirkwood
    actions pid
    chooseAbilityOf pid horse
    chooseTarget pid lid
    (.progress) <$> getLocation lid `shouldReturn` 2

  it "Hardy Leadership gives each Dwarf +1 hit point" . gameTest $ \pid -> do
    bofur <- putAllyIntoPlay pid Allies.bofur
    hp <- stat getHitPoints bofur
    _ <- putAttachmentOn pid Attachments.hardyLeadership =<< hero theodred
    getHitPoints bofur `shouldReturn` hp + 1

  it "A Elbereth! Gilthoniel! puts the enemy that attacked you on the bottom of the deck and raises your threat to its engagement cost" . gameTest $ \pid -> do
    setThreat pid 20
    card <- putCardIntoHand pid Events.aElberethGilthoniel
    _ <- setEncounterDeck [Locations.mountainsOfMirkwood]
    eid <- staged Enemies.wolfRider
    run $ EngageEnemy pid eid
    run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    chooseTarget pid (CardIdTarget card.id)
    isInPlay (toTarget eid) `shouldReturn` False
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.mountainsOfMirkwood, toCardCode Enemies.wolfRider]
    threat pid `shouldReturn` 20
    troll <- staged Enemies.hillTroll
    run $ EngageEnemy pid troll
    card' <- putCardIntoHand pid Events.aElberethGilthoniel
    run $ CheckWindows [mkAfter $ EnemyAttacking troll pid]
    -- the Hill Troll is unique-free and costs 30 to engage
    chooseTarget pid (CardIdTarget card'.id)
    threat pid `shouldReturn` 30

  it "Miruvor is discarded for two of its effects" . gameTest $ \pid -> do
    b <- hero beravor
    miruvor <- putAttachmentOn pid Attachments.miruvor b
    run $ Exhaust b
    actions pid
    chooseAbilityOf pid miruvor
    chooseLabel "$label.miruvor.ready" pid
    chooseLabel "$label.miruvor.resource" pid
    resourcesOf b `shouldReturn` 1
    heroExhausted . toAttrs <$> getHeroWithCode beravor `shouldReturn` False
    isInPlay miruvor `shouldReturn` False

  it "Miruvor goes on top of the deck of the player resolving it, not its owner's" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
    legolas <- heroOf two "01005"
    miruvor <- putAttachmentOn one Attachments.miruvor legolas
    case miruvor of
      AttachmentTarget aid -> run $ AttachCard aid legolas
      _ -> error "not an attachment"
    actions two
    chooseAbilityOf two miruvor
    chooseLabel "$label.miruvor.resource" two
    chooseLabel "$label.miruvor.topOfDeck" two
    map toCardCode . take 1 . (.deck) <$> getPlayer two `shouldReturn` [toCardCode Attachments.miruvor]
    map toCardCode . (.discard) <$> getPlayer one `shouldReturn` []

  it "Master of the Forge searches the top 5 for an attachment" . gameTest $ \pid -> do
    master <- putAllyIntoPlay pid Allies.masterOfTheForge
    wanted <- onTopOfDeck pid Attachments.ringMail
    actions pid
    chooseAbilityOf pid master
    chooseTarget pid (CardIdTarget wanted.id)
    map (.id) <$> handOf pid `shouldReturn` [wanted.id]

  it "Peace, and Thought exhausts 2 heroes to draw 5 in the refresh phase" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Events.peaceAndThought
    giveResources beravor 1
    setPhase RefreshPhase
    actions pid
    chooseTarget pid (CardIdTarget card.id)
    chooseTarget pid =<< hero eowyn
    chooseTarget pid =<< hero theodred
    length <$> handOf pid `shouldReturn` 5

  it "Risk Some Light moves 1 of the top 3 encounter cards to the bottom and the rest back in order" . gameTest $ \pid -> do
    cards <- setEncounterDeck [Locations.mountainsOfMirkwood, Enemies.hillTroll, Enemies.wolfRider, Locations.mountainsOfMirkwood]
    _ <- playWith pid beravor 3 Events.riskSomeLight
    case cards of
      [a, b, c, d] -> do
        chooseTarget pid (CardIdTarget a.id)
        chooseTarget pid (CardIdTarget c.id)
        chooseTarget pid (CardIdTarget b.id)
        map (.id) <$> getEncounterDeck `shouldReturn` map (.id) [b, c, d, a]
      _ -> error "four cards"

  it "an event's When window opens before its effects" . gameTest $ \pid -> do
    card <- genPlayerCard Events.daeronsRunes (Just pid)
    case playCard pid card [] of
      CheckWindows [w] : _ -> windowMatches (EventPlayedWindow When) w `shouldBe` True
      _ -> error "no When window"

  it "cancelEvent takes a played event's effects off the queue and discards it" . gameTest $ \pid -> do
    card <- genPlayerCard Events.daeronsRunes (Just pid)
    push $ PlayEvent pid card []
    runQueueT (cancelEvent card.id)
    void mainLoop
    handOf pid `shouldReturn` []
    map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [card.id]
