module LordOfTheRings.Game.Cards (module LordOfTheRings.Game.Cards) where

import Data.Map.Strict qualified as Map (
  delete,
  findWithDefault,
  insert,
  lookup,
  member,
 )
import Data.UUID qualified as UUID (nil)
import LordOfTheRings.Ally (
  Ally,
  AllyAttrs (
    allyDiscardWindow,
    allyExhausted,
    allyHandCriteria,
    allyHandWindow,
    allyOwner
  ),
  createAlly,
 )
import LordOfTheRings.Attachment (
  AttachmentAttrs (attachmentPlacement),
  newAttachment,
 )
import LordOfTheRings.Card (
  Card (..),
  CardDef (
    cdCardTraits,
    cdCardType,
    cdKeywords,
    cdQuestPoints,
    cdRestrictedSlots,
    cdThreat
  ),
  CardType (AllyType, EventType, ObjectiveAllyType),
  EncounterCard (MkEncounterCard),
  HasCardCode (toCardCode),
  HasCardDef (..),
  IsCard (..),
  PlayerCard (pcOwner),
 )
import LordOfTheRings.Classes.Entity (
  Entity (overAttrs, toAttrs, toId),
  insertEntity,
 )
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasQueue (
  HasQueue (pushAll),
  push,
  runQueueT,
 )
import LordOfTheRings.Criteria (Criterion (..))
import LordOfTheRings.Effect (
  Effect (Effect, effectModifiers, effectTarget),
  EffectDuration (
    UntilEndOfAttack,
    UntilEndOfPhase,
    UntilEndOfRound
  ),
 )
import LordOfTheRings.Enemy (
  EnemyAttrs (enemyCardId, enemyPlacement, enemyShadowCards),
 )
import LordOfTheRings.Entities (
  Entities (entitiesQuests),
  alliesL,
  attachmentsL,
  enemiesL,
  heroesL,
  locationsL,
  objectivesL,
  questsL,
  treacheriesL,
 )
import LordOfTheRings.Event (
  EventAttrs (eventCriteria, eventDiscardWindow, eventWindow),
  lookupEvent,
 )
import LordOfTheRings.Frontend (
  Frontend (send),
  FrontendMessage (FlipCard),
 )
import LordOfTheRings.Game.Base (
  Game (gameEffects, gameEntities, gameModifiers),
  effectsL,
  entitiesL,
  leftPlayL,
  playersL,
  scenarioL,
 )
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (
  actionsAndResponsesAllowed,
  passesCriteria,
  windowMatchesM,
 )
import LordOfTheRings.Hero (HeroAttrs (heroGuardedBy))
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Id (
  AllyId (AllyId),
  CardId,
  EnemyId,
  LocationId,
  PlayerId,
 )
import LordOfTheRings.Keyword (
  GrappleLocation (..),
  Keyword (
    FirstPlayerGainsControl,
    FunctionsLikeAPlayerCard,
    GuardedBy,
    Hinder,
    LastPlayerGainsControl,
    Permanent,
    Restricted
  ),
 )
import LordOfTheRings.Location (
  LocationAttrs (locationCardId, locationPlacement),
 )
import LordOfTheRings.Matcher (
  AttachMatcher (PlayIntoPlayArea, PlayIntoStagingArea),
  characterController,
  getLastPlayer,
  getLead,
  getPlayer,
  hasKeyword,
  passesUnique,
 )
import LordOfTheRings.Matcher.Window (windowMatches)
import LordOfTheRings.Message (
  Message (
    AddToEncounterDiscard,
    AttachCard,
    CheckRestricted,
    CheckWindows,
    CreateEffect,
    Discard,
    Flip,
    ForgetLeftPlay,
    PlaceEnemy,
    PlaceLocation,
    PlaceObjective,
    PlayCardInWindow,
    ReleaseGuardedPlayerCards,
    RemoveFromGame,
    RescueCharacter,
    RespondFromDiscard,
    ReturnToSackDeck,
    ReturnToTopOfEncounterDeck,
    UseHandAction
  ),
  questionLabelWith,
  uiToRun,
 )
import LordOfTheRings.Message.Lifted (
  choosePlayerM,
  shuffleIntoDeck,
 )
import LordOfTheRings.Modifier (
  Modifier (modifierType),
  ModifierType (
    AdditionalRestrictedSlots,
    AlliesEnterPlayExhausted,
    AlliesLeavePlayToHand,
    AttackDealsNoDamage,
    Defending,
    EnteredPlayThisRound,
    GainKeyword,
    GainTrait,
    LeavesPlayToAnyPlayersDeck,
    LeavesPlayToEncounterDiscard,
    LeavesPlayToSackDeck,
    LeavesPlayToTopOfSackDeck,
    LeavesPlayToTopOfEncounterDeck,
    LoseKeyword,
    MayPlayTopOfDeck,
    ReadiedByRefresh,
    ShuffledIntoEncounterDeckWhenDefeated
  ),
 )
import LordOfTheRings.Objective (
  ObjectiveAttrs (objectiveCardId, objectivePlacement),
 )
import LordOfTheRings.Placement (
  Placement (..),
  placementController,
 )
import LordOfTheRings.Player.Runner (
  canAfford,
  cannotPlay,
  discardL,
  handL,
 )
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest (
  QuestAttrs (questCardId, questFlipped),
  isSideQuest,
 )
import LordOfTheRings.Question (UI, targetLabel)
import LordOfTheRings.Scenario (
  ScenarioAttrs (scenarioGuardedPlayerCards, scenarioOrcGuards),
  encounterDeckL,
  sackDeckL,
  orcGuardsL,
 )
import LordOfTheRings.Scenario.Sailing (discardEncounterCard)
import LordOfTheRings.Source (
  Source (
    CardIdSource,
    EnemyAttackSource,
    GameSource,
    PlayerSource
  ),
 )
import LordOfTheRings.Target (
  Target (
    AllyTarget,
    AttachmentTarget,
    CardIdTarget,
    EnemyTarget,
    GameTarget,
    HeroTarget,
    LocationTarget,
    ObjectiveTarget,
    PlayerTarget,
    QuestTarget,
    TreacheryTarget
  ),
  Targetable (toTarget),
 )
import LordOfTheRings.Trait (Trait)
import LordOfTheRings.Treachery (
  TreacheryAttrs (treacheryOwner, treacheryPlacement),
 )
import LordOfTheRings.Window (
  Window,
  WindowType (
    AllyEnteredPlay,
    CharacterLeftPlay,
    CharacterRescued,
    EntersPlay,
    LeftPlay
  ),
  mkAfter,
 )

-- | The enemy's attack deals no damage, as Frodo Baggins (The Road Darkens) makes it.
dealsNoDamage :: Game -> EnemyId -> Bool
dealsNoDamage g eid = AttackDealsNoDamage `elem` map modifierType (Map.findWithDefault [] (EnemyTarget eid) (gameModifiers g))

-- | An objective ally that a player gains control of as it enters play.
gainsController :: CardDef -> Bool
gainsController def = any (`member` cdKeywords def) [FirstPlayerGainsControl, LastPlayerGainsControl]

-- | The player an objective ally goes to: the last player, when it reads so, or the first.
objectiveAllyController :: CardDef -> GameT PlayerId
objectiveAllyController def
  | LastPlayerGainsControl `member` cdKeywords def = getLastPlayer
  | otherwise = getLead

-- | Whether the enemy has the hinder keyword.
hinders :: Game -> EnemyId -> GameT Bool
hinders g eid = case Map.lookup eid g.entities.enemies of
  Just e -> hasKeyword Hinder (toTarget eid) (toCardDef e)
  Nothing -> pure False

-- | An encounter card in play goes back into the encounter deck where the function puts it,
-- the cards attached to it discarded.
returnToEncounterDeck
  :: (EncounterCard -> [EncounterCard] -> [EncounterCard]) -> Target -> Game -> GameT Game
returnToEncounterDeck place target g = do
  let toDeck card = scenarioL %~ overAttrs (encounterDeckL %~ place card)
      withAttached =
        pushAll
          $ [Discard GameSource (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
          <> freeGuarded g target
  case target of
    EnemyTarget eid | Just e <- Map.lookup eid g.entities.enemies -> do
      withAttached
      pushAll $ discardShadowsOf g target <> attachedEnemies g target
      pure
        $ g
        & entitiesL
        . enemiesL
        %~ Map.delete eid
        & toDeck (MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e))
    LocationTarget lid | Just l <- Map.lookup lid g.entities.locations -> do
      withAttached
      pure
        $ g
        & entitiesL
        . locationsL
        %~ Map.delete lid
        & toDeck (MkEncounterCard (locationCardId $ toAttrs l) (toCardCode l))
    ObjectiveTarget oid
      | Just o <- Map.lookup oid g.entities.objectives ->
          pure
            $ g
            & entitiesL
            . objectivesL
            %~ Map.delete oid
            & toDeck (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o))
    _ -> pure g

-- | Show the players what the board does as it happens: the log is written by the narrator.
announce :: Message -> GameT ()
announce = \case
  -- a stage turning over, as the setup or a click does
  Flip target -> do
    quests <- getsGame (toList . entitiesQuests . gameEntities)
    for_ quests \q -> do
      let attrs = toAttrs q
      when (not attrs.questFlipped && (target == toTarget q || target == CardIdTarget attrs.questCardId))
        $ send
        $ FlipCard attrs.questCardId
  _ -> pure ()

-- | The Guarded (X) keyword's types on a player card, as Necklace of Girion's.
guardedTypes :: PlayerCard -> Maybe [CardType]
guardedTypes card = headMay [ts | GuardedBy ts <- toList (cdKeywords $ toCardDef card)]

-- | The attachment card enters play and attaches to a host its player chooses.
playAttachment :: PlayerId -> PlayerCard -> Game -> GameT Game
playAttachment pid card g = do
  attachment <- newAttachment pid card
  let entered = CheckWindows [mkAfter $ EntersPlay $ toTarget attachment]
      attachTo host = targetLabel host [AttachCard (toId attachment) host, entered]
  case (toAttrs attachment).attachTo of
    PlayIntoStagingArea -> do
      push entered
      pure
        $ g
        & entitiesL
        . attachmentsL
        %~ insertEntity (overAttrs (\a -> a {attachmentPlacement = StagingArea}) attachment)
    PlayIntoPlayArea -> do
      push entered
      pure
        $ g
        & entitiesL
        . attachmentsL
        %~ insertEntity (overAttrs (\a -> a {attachmentPlacement = InPlayArea pid}) attachment)
    _ -> do
      hosts <- cardHosts pid (toCardCode card)
      push $ case hosts of
        [host] -> uiToRun (attachTo host)
        _ ->
          questionLabelWith
            (ikey "label.attachTo")
            (Just $ toTarget attachment)
            hosts
            pid
            (map attachTo hosts)
      pure $ g & entitiesL . attachmentsL %~ insertEntity attachment

-- | Remove an entity from play and put its card in the right discard pile.
discardTarget :: Target -> Game -> GameT Game
discardTarget target = discardFromPlay target <=< (`leftPlay` target)

discardFromPlay :: Target -> Game -> GameT Game
discardFromPlay target g = case target of
  HeroTarget hid -> fromEntity heroesL hid
  AllyTarget aid -> fromEntity alliesL aid
  EnemyTarget eid -> fromEntity enemiesL eid
  LocationTarget lid -> fromEntity locationsL lid
  TreacheryTarget tid -> fromEntity treacheriesL tid
  AttachmentTarget aid -> fromEntity attachmentsL aid
  -- a side quest discarded from the staging area, as an encounter card's effect may
  QuestTarget qid | maybe False isSideQuest (Map.lookup qid g.entities.quests) -> fromEntity questsL qid
  -- an objective that leaves its hero, or is discarded, goes back to the staging area
  ObjectiveTarget oid -> do
    mods <- getModifiers target
    case Map.lookup oid g.entities.objectives of
      Just o | LeavesPlayToEncounterDiscard `elem` mods -> do
        push $ AddToEncounterDiscard [MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o)]
        pure $ g & entitiesL . objectivesL %~ Map.delete oid
      Just o | LeavesPlayToTopOfSackDeck `elem` mods -> do
        let card = MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o)
        pure $ g & entitiesL . objectivesL %~ Map.delete oid & scenarioL %~ overAttrs (sackDeckL %~ (card :))
      Just _ | LeavesPlayToSackDeck `elem` mods -> g <$ push (ReturnToSackDeck oid)
      _ ->
        g <$ push (if LeavesPlayToTopOfEncounterDeck `elem` mods then ReturnToTopOfEncounterDeck target else PlaceObjective oid StagingArea)
  _ -> pure g
 where
  fromEntity :: (Ord k, IsCard v) => Lens' Entities (Map k v) -> k -> GameT Game
  fromEntity l k = case Map.lookup k (g.entities ^. l) of
    Nothing -> pure g
    Just entity -> do
      pushAll $ discardAttached g target
      mods <- getModifiers target
      let removed = g & entitiesL . l %~ Map.delete k
          waiting ec e = effectTarget e == CardIdTarget ec.id && LeavesPlayToTopOfEncounterDeck `elem` effectModifiers e
          orcGuards = (toAttrs g.scenario).scenarioOrcGuards
      case toCard entity of
        EncounterCard ec
          | LeavesPlayToAnyPlayersDeck `elem` mods -> removed <$ toAnyPlayersDeck g ec
        -- an Orc Guard leaves its player card in that player's discard pile
        EncounterCard ec
          | Just pc <- Map.lookup ec.id orcGuards ->
              pure
                $ addToDiscard (PlayerCard pc)
                $ removed
                & scenarioL
                %~ overAttrs (orcGuardsL %~ Map.delete ec.id)
        EncounterCard ec
          | LeavesPlayToTopOfEncounterDeck `elem` mods ->
              pure $ removed & scenarioL %~ overAttrs (encounterDeckL %~ (ec :))
          -- an effect waiting on the card itself, as A Watchful Peace's, resolves once it
          -- has left play, where what it was immune to in play no longer stops it
          | any (waiting ec) (gameEffects g) ->
              pure $ removed & effectsL %~ filter (not . waiting ec) & scenarioL %~ overAttrs (encounterDeckL %~ (ec :))
          | ShuffledIntoEncounterDeckWhenDefeated `elem` mods -> do
              deck <- shuffleM (ec : (toAttrs g.scenario).encounterDeck)
              pure $ removed & scenarioL %~ overAttrs (encounterDeckL .~ deck)
        -- an encounter card with a player card back, as Misplaced Trust, goes to its
        -- owner's discard pile, not the encounter discard pile (The Land of Shadow rules
        -- insert, on The Searching Eye: "functions like a player card")
        EncounterCard ec
          | FunctionsLikeAPlayerCard `member` cdKeywords (toCardDef ec)
          , Just owner <- encounterOwner ->
              pure $ removed & playersL . ix owner . discardL %~ (EncounterCard ec :)
        PlayerCard pc
          | AllyTarget _ <- target
          , Just owner <- pcOwner pc -> do
              toHand <- elem AlliesLeavePlayToHand <$> getModifiers GameTarget
              pure
                $ if toHand
                  then removed & playersL . ix owner . handL %~ (<> [PlayerCard pc])
                  else addToDiscard (PlayerCard pc) removed
        card -> pure $ addToDiscard card removed
  encounterOwner = case target of
    TreacheryTarget tid -> treacheryOwner . toAttrs =<< Map.lookup tid g.entities.treacheries
    _ -> Nothing
  addToDiscard card = case card of
    PlayerCard pc | Just owner <- pcOwner pc -> playersL . ix owner . discardL %~ (card :)
    -- a card of the Corsair deck to its own discard pile (Dream-chaser rules)
    EncounterCard ec -> scenarioL %~ overAttrs (discardEncounterCard ec)
    _ -> id

-- | As it leaves play, the first player shuffles the card into any player's deck, as
-- Recurring Nightmare reads.
toAnyPlayersDeck :: Game -> EncounterCard -> GameT ()
toAnyPlayersDeck g card = runQueueT $ choosePlayerM g.firstPlayer (EncounterCard card) (`shuffleIntoDeck` card)

-- | The enemies grappling the location let go of it, for the staging area (The Thing in
-- the Depths).
letGoOf :: Game -> LocationId -> [Message]
letGoOf g lid = [PlaceEnemy (toId e) StagingArea | e <- toList g.entities.enemies, e.placement == AttachedTo (LocationTarget lid)]

-- | The printed value a Grapple keyword reads, and whether it seeks the lowest.
printedFor :: GrappleLocation -> CardDef -> Maybe Int
printedFor = \case
  LowestPrintedThreat -> cdThreat
  HighestPrintedThreat -> cdThreat
  LowestPrintedQuestPoints -> cdQuestPoints
  HighestPrintedQuestPoints -> cdQuestPoints
  -- counted in play instead, not printed
  MostEnemiesGrappled -> const Nothing
  FewestEnemiesGrappled -> const Nothing

lowest :: GrappleLocation -> Bool
lowest = (`elem` [LowestPrintedThreat, LowestPrintedQuestPoints, FewestEnemiesGrappled])

-- | Attachments, and encounter cards attached as conditions, leave play with their host,
-- and an enemy's shadow cards are discarded with it (ALeP 3.068).
discardAttached :: Game -> Target -> [Message]
discardAttached g target =
  [leaves a | a <- toList g.entities.attachments, a.placement == AttachedTo target]
    <> [leaves t | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
    <> [leaves o | o <- toList g.entities.objectives, o.placement == AttachedTo target]
    <> attachedEnemies g target
    <> discardShadowsOf g target
    <> freeGuarded g target
 where
  -- a Permanent boon or burden is removed from the game as its hero leaves play
  leaves :: (HasCardDef c, Targetable c) => c -> Message
  leaves c = if Permanent `member` cdKeywords (toCardDef c) then RemoveFromGame (toTarget c) else Discard GameSource (toTarget c)

-- | Whether the card is a Permanent boon or burden on a hero in play, which cannot be
-- discarded from it.
permanentOnHero :: Game -> Target -> Bool
permanentOnHero g t = fromMaybe False do
  (def, placement) <- case t of
    AttachmentTarget aid -> (\a -> (toCardDef a, a.placement)) <$> Map.lookup aid g.entities.attachments
    ObjectiveTarget oid -> (\o -> (toCardDef o, o.placement)) <$> Map.lookup oid g.entities.objectives
    TreacheryTarget tid -> (\x -> (toCardDef x, x.placement)) <$> Map.lookup tid g.entities.treacheries
    _ -> Nothing
  pure $ Permanent `member` cdKeywords def && case placement of
    AttachedTo (HeroTarget hid) -> Map.member hid g.entities.heroes
    _ -> False

{- | An enemy attached to the host, as Grasping Tentacle to a character, is discarded as its
host leaves play. One attached to an enemy is a Mount, as Mordor Warg and Racing Warg are,
whose "Forced: When attached enemy leaves play, add it to the staging area" is resolved
here: it goes to the staging area instead.
-}
attachedEnemies :: Game -> Target -> [Message]
attachedEnemies g target =
  [ case target of
      EnemyTarget _ -> PlaceEnemy (toId e) StagingArea
      _ -> Discard GameSource (toTarget e)
  | e <- toList g.entities.enemies
  , e.placement == AttachedTo target
  ]

{- | The locations the card guards go free to the staging area as it leaves play, as an
objective-location of Across the Ettenmoors returns once its guard is dealt with.
-}
freeGuarded :: Game -> Target -> [Message]
freeGuarded g target =
  [ PlaceLocation (toId l) StagingArea
  | Just cid <- [guardCard]
  , l <- toList g.entities.locations
  , l.placement == AttachedTo (CardIdTarget cid)
  ]
    -- and the player cards it guards go free (MEC65 rules)
    <> [ReleaseGuardedPlayerCards cid | Just cid <- [guardCard], Map.member cid (toAttrs g.scenario).scenarioGuardedPlayerCards]
    -- and the characters it guards as captives are rescued, in one window, as "after any
    -- number of characters are rescued" reads (Rescue the Prisoners)
    <> map RescueCharacter captives
    <> [CheckWindows [mkAfter $ CharacterRescued c | c <- captives] | notNull captives]
 where
  captives =
    [ c
    | Just cid <- [guardCard]
    , c <-
        [toTarget h | h <- toList g.entities.heroes, heroGuardedBy (toAttrs h) == Just cid]
          <> [toTarget a | a <- toList g.entities.allies, a.placement == AttachedTo (CardIdTarget cid)]
    ]
  cardIdIn :: IsCard v => Ord k => Map k v -> k -> Maybe CardId
  cardIdIn m k = (.id) . toCard <$> Map.lookup k m
  guardCard = case target of
    EnemyTarget eid -> cardIdIn g.entities.enemies eid
    LocationTarget lid -> cardIdIn g.entities.locations lid
    QuestTarget qid -> cardIdIn g.entities.quests qid
    TreacheryTarget tid -> cardIdIn g.entities.treacheries tid
    _ -> Nothing

-- | The shadow cards dealt to an enemy leaving play, discarded with it.
-- | Whether the card is among these.
holds :: Target -> Entities -> Bool
holds target es = case target of
  EnemyTarget k -> Map.member k es.enemies
  LocationTarget k -> Map.member k es.locations
  TreacheryTarget k -> Map.member k es.treacheries
  ObjectiveTarget k -> Map.member k es.objectives
  _ -> False

-- | The card among these placed in the staging area, an enemy without its shadow cards.
toStagingArea :: Target -> Entities -> Entities
toStagingArea target es = case target of
  EnemyTarget k -> es & enemiesL . ix k %~ overAttrs (\a -> a {enemyPlacement = StagingArea, enemyShadowCards = []})
  LocationTarget k -> es & locationsL . ix k %~ overAttrs (\a -> a {locationPlacement = StagingArea})
  TreacheryTarget k -> es & treacheriesL . ix k %~ overAttrs (\a -> a {treacheryPlacement = StagingArea})
  ObjectiveTarget k -> es & objectivesL . ix k %~ overAttrs (\a -> a {objectivePlacement = StagingArea})
  _ -> es

discardShadowsOf :: Game -> Target -> [Message]
discardShadowsOf g = \case
  EnemyTarget eid | Just e <- Map.lookup eid g.entities.enemies, notNull e.shadowCards -> [AddToEncounterDiscard e.shadowCards]
  _ -> []

{- | Open the window for a character leaving play, while it can still be looked at. An
ally is kept aside until the window closes, so its own abilities can answer it.
-}
leftPlay :: Game -> Target -> GameT Game
leftPlay g target = do
  windows <- leftPlayWindows g target
  case target of
    AllyTarget aid
      | Just a <- Map.lookup aid g.entities.allies
      , notNull windows -> do
          pushAll [CheckWindows (map mkAfter windows), ForgetLeftPlay aid]
          pure $ g & leftPlayL %~ Map.insert aid a
    _ -> do
      unless (null windows) $ push $ CheckWindows (map mkAfter windows)
      pure g

-- | What a character leaving play opens windows for.
leftPlayWindows :: Game -> Target -> GameT [WindowType]
leftPlayWindows g target = case target of
  HeroTarget hid | Just h <- Map.lookup hid g.entities.heroes -> window h.owner (toCardDef h) (toCard h)
  AllyTarget aid
    | Just a <- Map.lookup aid g.entities.allies
    , Just pid <- placementController a.placement ->
        window pid (toCardDef a) (toCard a)
  _ -> pure []
 where
  window pid def card = do
    gained <- gainedTraits target
    pure [LeftPlay target pid (cdCardTraits def <> gained), CharacterLeftPlay card]

-- | The refresh phase, not a card effect, readies the character next.
refreshing :: Target -> Message
refreshing c = CreateEffect $ Effect GameSource c [ReadiedByRefresh] UntilEndOfPhase

-- | The character is defending the attack being resolved.
-- | The character defends the enemy's attack, the effect's source the attack, for what
-- reads the defender of one enemy's attack (see 'getDefendersAgainst').
defendingEffect :: EnemyId -> Target -> Message
defendingEffect eid d = CreateEffect $ Effect (EnemyAttackSource eid) d [Defending] UntilEndOfAttack

gainedTraits :: Target -> GameT (Set Trait)
gainedTraits target = setFromList . mapMaybe (\case GainTrait t -> Just t; _ -> Nothing) <$> getModifiers target

-- | The characters with the keyword, printed or gained.
withKeyword :: Keyword -> [Target] -> GameT [Target]
withKeyword k = filterM (\t -> hasKeyword k t =<< characterCardDef t)

-- | Whether the card has the restricted keyword.
isRestricted :: HasCardDef a => a -> Bool
isRestricted = member Restricted . cdKeywords . toCardDef

{- | With more than 2 restricted cards on the character, its controller discards one of
them, and again until 2 are left; a card may count as 2, as Raiment of War. A character
may be allowed more, as Forth, The Three Hunters! allows each hero 1 more, and Golden
Belt its hero.
-}
checkRestricted :: Game -> Target -> GameT ()
checkRestricted g host = do
  let slots :: HasCardDef a => a -> Int
      slots = cdRestrictedSlots . toCardDef
      restrictedOn =
        [(toTarget x, slots x) | x <- toList g.entities.attachments, x.placement == AttachedTo host, (isRestricted x && not (losesRestricted x)) || gainsRestricted x]
          <> [(toTarget o, slots o) | o <- toList g.entities.objectives, o.placement == AttachedTo host, isRestricted o]
      -- an attachment given Restricted with its text, as Wild Stallion's Mount text
      gainsRestricted x = GainKeyword Restricted `elem` map modifierType (Map.findWithDefault [] (toTarget x) (gameModifiers g))
      -- one that loses it on its host, as Snowmane on Théoden
      losesRestricted x = LoseKeyword Restricted `elem` map modifierType (Map.findWithDefault [] (toTarget x) (gameModifiers g))
  controller <- characterController host
  extra <- sum . (\ms -> [n | AdditionalRestrictedSlots n <- ms]) <$> getModifiers host
  for_ controller \pid ->
    when (sum (map snd restrictedOn) > 2 + extra)
      $ push
      $ questionLabelWith
        (ikey "label.discardRestricted")
        (Just host)
        []
        pid
        [targetLabel x [Discard GameSource x, CheckRestricted host] | (x, _) <- restrictedOn]

-- | The ally as it enters play: exhausted while allies enter play so, as Flight to Gondor
-- has them.
entering :: Ally -> GameT Ally
entering a = do
  exhausted <- elem AlliesEnterPlayExhausted <$> getModifiers GameTarget
  pure $ if exhausted then overAttrs (\x -> x {allyExhausted = True}) a else a

-- | Mark the ally as having entered play this round, under the player its source names:
-- an objective-ally is put into play by the encounter cards, so by no player.
enteredPlayThisRound :: Ally -> Message
enteredPlayThisRound a = CreateEffect $ Effect source (toTarget a) [EnteredPlayThisRound] UntilEndOfRound
 where
  source
    | cdCardType (toCardDef a) == ObjectiveAllyType = GameSource
    | otherwise = PlayerSource $ allyOwner $ toAttrs a

-- | The windows for an ally entering play under the player.
allyEntersPlay :: PlayerId -> Ally -> Message
allyEntersPlay pid a =
  CheckWindows
    [ mkAfter $ EntersPlay $ toTarget a
    , mkAfter $ AllyEnteredPlay (toTarget a) pid (cdCardTraits $ toCardDef a)
    ]

{- | Cards in a player's hand that can be played or put into play in response to the
windows: response events, and allies like Brok Ironfist. A response event on top of his
deck too, while he may play that card as if it was in his hand, as Gandalf lets him.
-}
handResponses :: PlayerId -> [Window] -> Message -> GameT [UI Message]
handResponses pid windows continue = do
  allowed <- actionsAndResponsesAllowed
  if allowed then handResponsesAllowed pid windows continue else pure []

handResponsesAllowed :: PlayerId -> [Window] -> Message -> GameT [UI Message]
handResponsesAllowed pid windows continue = do
  player <- getPlayer pid
  fromTop <- elem MayPlayTopOfDeck <$> getModifiers (PlayerTarget pid)
  let matches = maybe (pure False) (\w -> anyM (windowMatchesM w) windows)
      deckTop = [c | fromTop, c@(PlayerCard pc) <- take 1 player.deck, cdCardType (toCardDef pc) == EventType]
  fmap catMaybes $ for (player.hand <> deckTop) \case
    card@(PlayerCard pc) -> case cdCardType (toCardDef pc) of
      EventType
        | Just e <- lookupEvent (toCardCode pc) pc.id pid -> do
            ok <-
              andM
                [ matches (toAttrs e).eventWindow
                , passesCriteria (toAttrs e).eventCriteria
                , canAfford player card
                , not <$> cannotPlay pid card
                ]
            pure $ guard ok $> targetLabel (CardIdTarget pc.id) [PlayCardInWindow pid pc.id windows, continue]
      AllyType -> do
        let attrs = toAttrs $ createAlly pc (AllyId UUID.nil) pid
        ok <- andM [matches (allyHandWindow attrs), passesCriteria (allyHandCriteria attrs), passesUnique card]
        pure $ guard ok $> targetLabel (CardIdTarget pc.id) [UseHandAction pid pc.id, continue]
      _ -> pure Nothing
    EncounterCard _ -> pure Nothing

{- | Whether the card is an event the player can play in the quest phase before
characters are committed.
-}
playsBeforeCommitting :: PlayerId -> Card -> Bool
playsBeforeCommitting pid = \case
  PlayerCard pc | Just e <- lookupEvent (toCardCode pc) pc.id pid -> before (toAttrs e).eventCriteria
  _ -> False
 where
  before = \case
    BeforeCommitting -> True
    BeforeStaging -> True
    Criteria cs -> any before cs
    _ -> False

{- | Cards in a player's discard pile that respond to the windows from there: allies like
Ered Luin Miner, put into play unless their card says otherwise, as Starla is played,
and events like Hidden Cache, which resolve it. The
continuation gets the card id, to mark the card as used.
-}
discardResponses
  :: PlayerId -> [Window] -> [(Source, Int)] -> (CardId -> Message) -> GameT [UI Message]
discardResponses pid windows used continue = do
  allowed <- actionsAndResponsesAllowed
  if allowed then discardResponsesAllowed pid windows used continue else pure []

discardResponsesAllowed :: PlayerId -> [Window] -> [(Source, Int)] -> (CardId -> Message) -> GameT [UI Message]
discardResponsesAllowed pid windows used continue = do
  player <- getPlayer pid
  let matches = maybe False (\w -> any (windowMatches w) windows)
  fmap catMaybes $ for player.discard \case
    PlayerCard pc | (CardIdSource pc.id, 0) `notElem` used -> case cdCardType (toCardDef pc) of
      AllyType
        | matches (allyDiscardWindow $ toAttrs $ createAlly pc (AllyId UUID.nil) pid) -> do
            ok <- passesUnique (PlayerCard pc)
            pure $ guard ok $> targetLabel (CardIdTarget pc.id) [RespondFromDiscard pid pc windows, continue pc.id]
      EventType
        | Just e <- lookupEvent (toCardCode pc) pc.id pid
        , matches (toAttrs e).eventDiscardWindow ->
            pure $ Just $ targetLabel (CardIdTarget pc.id) [RespondFromDiscard pid pc windows, continue pc.id]
      _ -> pure Nothing
    _ -> pure Nothing

-- | The next player clockwise from the given one.
