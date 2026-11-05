module LordOfTheRings.Game.Runner (module LordOfTheRings.Game.Runner) where

import Data.Map.Strict qualified as Map (
  delete,
  difference,
  findWithDefault,
  keys,
  lookup,
  member,
  union,
 )
import LordOfTheRings.Card (
  Card (EncounterCard, PlayerCard),
  CardDef (cdCardType),
  CardType (TreacheryType),
  HasCardDef (toCardDef),
  IsCard (toCard),
 )
import LordOfTheRings.Classes.Entity (
  Entity (overAttrs, toAttrs, toId),
 )
import LordOfTheRings.Classes.HasQueue (
  HasQueue (pushAll),
  insertAfterMatching,
  peekQueue,
  push,
  pushWhen,
 )
import LordOfTheRings.Effect (
  Effect (Effect),
  EffectDuration (UntilEndOfPhase, UntilEndOfRound),
 )
import LordOfTheRings.Entities (locationsL)
import LordOfTheRings.Game.Base (
  Game (gameModifiers),
  activeCostsL,
  allyEntryBarred,
  barredFromPlay,
  entitiesL,
  leftPlayLocationsL,
  playersL,
  scenarioL,
 )
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (getModifiers, isEncounterCardSource)
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Id (EnemyId, LocationId)
import LordOfTheRings.Location (
  LocationAttrs (locationPlacement, locationTokens),
  isGrottoCard,
  isUnchartedCard,
 )
import LordOfTheRings.Matcher (
  LocationMatcher (
    IsActiveLocation,
    LocationInStagingArea,
    LocationWithId
  ),
  Query (select),
  getLead,
  getLocation,
  progressPlacedThisRound,
  selectAny,
  timeCountersIn,
 )
import LordOfTheRings.Message (
  Message (
    ArrangeIslandMap,
    BeginPhase,
    CheckWindows,
    CompleteQuest,
    CreateEffect,
    Discard,
    DoStep,
    EngageEnemy,
    Flip,
    FocusArea,
    JoinArea,
    LeavePlay,
    PlaceEnemy,
    PlaceInStagingAreaOf,
    PlaceLocation,
    PlaceTimeCounters,
    PlaceTokens,
    PutAllyIntoPlay,
    PutAllyIntoPlayUnder,
    PutInDiscard,
    PutOnBottomOfOwnersDeck,
    RemoveFromGame,
    RemoveTokens,
    ResolveDiscover,
    ResolveTravelEffect,
    ReturnToBottomOfEncounterDeck,
    ReturnToHand,
    ReturnToTopOfEncounterDeck,
    SeparatePlayers,
    SeparateStagingAreas,
    ShuffleIntoEncounterDeck,
    ShuffleIntoGrottoDeck,
    ShuffleIntoOwnersDeck,
    ShuffleIntoUnchartedDeck,
    TravelTo
  ),
  removedEnemy,
 )
import LordOfTheRings.Modifier (
  Modifier (modifierType),
  ModifierType (
    CannotBeDefeated,
    CannotBeDiscardedByEncounterCards,
    CannotEnterStagingArea,
    CannotLeavePlay,
    CannotLeaveStagingArea,
    CannotRemoveTimeCounters,
    EngagesOnly,
    NoProgressOnQuestCards,
    ProgressCanceledThisRound,
    ProgressLimit,
    ProgressPerRoundLimit,
    ProgressPlacedThisPhase,
    ProgressPlacedThisRound,
    ProgressReducedBy,
    RemoveFromGameIfLeavesPlay,
    TakesProgressBeforeQuest,
    TakesQuestProgressFirst
  ),
 )
import LordOfTheRings.Placement (
  Placement (ActiveLocation, OnIslandMap, OrcsArea, StagingArea),
 )
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest (isSideQuest)
import LordOfTheRings.Scenario (
  ScenarioAttrs (scenarioOutOfPlayResources),
  outOfPlayResourcesL,
 )
import LordOfTheRings.Source (Source (GameSource))
import LordOfTheRings.Target (
  Target (
    AllyTarget,
    EnemyTarget,
    GameTarget,
    HeroTarget,
    LocationTarget,
    QuestTarget
  ),
  Targetable (toTarget),
 )
import LordOfTheRings.Token (
  Token (Progress, Resource, TimeCounter),
  removeAllTokens,
 )
import LordOfTheRings.Treachery (createTreachery)
import LordOfTheRings.Window (
  Timing (When),
  Window (Window),
  WindowType (
    BecameActiveLocation,
    EnemyEnteredPlay,
    EnemyLeftPlay,
    Explored,
    LastTimeCounterRemoved,
    LocationLeftPlay,
    PhaseBegins,
    TimeCountersRemoved
  ),
  mkAfter,
  mkWhen,
 )

import LordOfTheRings.Game.Cards (
  announce,
  leftPlayWindows,
  letGoOf,
  permanentOnHero,
 )
import LordOfTheRings.Game.Dispatch (runEach, runEntities)
import LordOfTheRings.Game.Message (runGameMessage)
import LordOfTheRings.Game.Query (
  progressFirst,
  progressRedirect,
  takingProgressFirst,
 )

runGame :: Message -> Game -> GameT Game
runGame msg g = case msg of
  -- progress for a card that another takes instead, as Blinding Blizzard the quest's
  PlaceTokens source target Progress n
    | n > 0
    , Just other <- progressRedirect g target ->
        g <$ push (PlaceTokens source other Progress n)
  RemoveTokens _ target TimeCounter _ -> do
    barred <- elem CannotRemoveTimeCounters <$> getModifiers target
    if barred
      then pure g
      else do
        g' <- runGame' msg g
        let before = timeCountersIn g.entities target
            after = timeCountersIn g'.entities target
        pushWhen (after < before)
          $ CheckWindows
          $ mkAfter (TimeCountersRemoved target (before - after))
          : [mkAfter (LastTimeCounterRemoved target) | after == 0]
        pure g'
  -- progress for the active location goes first to a location in the staging area that
  -- takes it before, as Grim Foothills
  PlaceTokens source target@(LocationTarget lid) Progress n | n > 0 -> do
    firsts <- takingProgressFirst
    active <- selectAny (LocationWithId lid <> IsActiveLocation)
    if active && notNull firsts
      then g <$ (push =<< progressFirst source n firsts \k -> [PlaceTokens source target Progress k | k > 0])
      else runGame'' msg g
  -- a quest card takes no more progress than it may this round, as Orc Rearguard limits
  -- the current quest to, and keeps count of what it took
  -- and less is placed on one whose progress is reduced, as Orc Grunts reduce the current
  -- quest's
  PlaceTokens source target@(QuestTarget _) Progress n | n > 0 -> do
    -- the first progress each round goes to a location that takes it first, as Forbidden
    -- Descent, which counts what it took
    takers <- select LocationInStagingArea
    diverted <- flip mapMaybeM takers \lid -> do
      firsts <- (\ms -> [k | TakesQuestProgressFirst k <- ms]) <$> getModifiers lid
      taken <- progressPlacedThisRound (toTarget lid)
      l <- getLocation lid
      room <- subtract l.progress <$> getLocationQuestPoints lid
      let k = foldr min (min n room) [max 0 (f - taken) | f <- firsts]
      pure $ guard (notNull firsts && k > 0) $> (lid, k)
    case diverted of
      (lid, k) : _ ->
        g
          <$ pushAll
            ( [ CreateEffect $ Effect GameSource (toTarget lid) [ProgressPlacedThisRound k] UntilEndOfRound
              , PlaceTokens source (toTarget lid) Progress k
              ]
                <> [PlaceTokens source target Progress (n - k) | n > k]
            )
      -- and all of it to one that takes progress before the quest, as Mines of Moria
      [] -> do
        befores <- filterM (fmap (elem TakesProgressBeforeQuest) . getModifiers . fst) =<< takingProgressFirst
        if notNull befores
          then g <$ (push =<< progressFirst source n befores \k -> [PlaceTokens source target Progress k | k > 0])
          else placeOnQuest
   where
    placeOnQuest = do
        barred <- elem NoProgressOnQuestCards <$> getModifiers GameTarget
        mods <- (<> [ProgressLimit 0 | barred]) <$> getModifiers target
        placed <- progressPlacedThisRound target
        let reduced = max 0 (n - sum [k | ProgressReducedBy k <- mods])
            -- and no more than it may take now, as Stranded in Umbar's 15 tokens at most
            room = foldr min reduced $ [max 0 (k - placed) | ProgressPerRoundLimit k <- mods] <> [max 0 k | ProgressLimit k <- mods]
        -- what is canceled is kept count of, as Umbar Bridge cancels only the first X
        pushWhen (n > reduced)
          $ CreateEffect
          $ Effect GameSource target [ProgressCanceledThisRound (n - reduced)] UntilEndOfRound
        if room <= 0
          then pure g
          else do
            push $ CreateEffect $ Effect GameSource target [ProgressPlacedThisRound room] UntilEndOfRound
            push $ CreateEffect $ Effect GameSource target [ProgressPlacedThisPhase room] UntilEndOfPhase
            runGame'' (PlaceTokens source target Progress room) g
  -- a location's Travel cost, which its card pushes as it sees TravelTo, is paid before
  -- it becomes the active location, so it is still in the staging area as it is paid (as
  -- Hills of Dunland's draws are, FAQ 3.123)
  TravelTo _ -> do
    push $ DoStep 1 msg
    runGame'' msg g
  -- the location's Travel effect without traveling, as Under Guard resolves the active
  -- location's: only its own card reads the 'TravelTo' its effect answers, and no one
  -- travels, so nothing else that answers traveling does
  ResolveTravelEffect lid -> traverseOf (entitiesL . locationsL . ix lid) (runEach (TravelTo lid)) g
  -- an enemy that engages only one player, as Chief Turch the first player, engages him
  -- whoever it would have engaged
  EngageEnemy pid eid -> do
    only <- (\ms -> [p | EngagesOnly p <- ms]) <$> getModifiers (EnemyTarget eid)
    case only of
      p : _ | p /= pid -> g <$ push (EngageEnemy p eid)
      _ -> runGame'' msg g
  -- no ally enters play once one has this round, as Corsair Waters reads: one to be put
  -- into play stays where it is, or goes to its owner's discard pile if it has left it
  PutAllyIntoPlay pid _ | allyEntryBarred pid g -> pure g
  -- nor one its controller cannot put into play, as Fellowship bars non-unique allies
  PutAllyIntoPlay pid cid
    | Just card <- find ((== cid) . (.id)) . (.hand) =<< Map.lookup pid g.players
    , barredFromPlay g pid card ->
        pure g
  PutAllyIntoPlayUnder pid card | barredFromPlay g pid (PlayerCard card) -> do
    let owner = card.owner >>= (`Map.lookup` g.players)
        held = maybe False (\p -> any ((== card.id) . (.id)) (p.hand <> p.deck <> p.discard)) owner
    pushWhen (not held) $ PutInDiscard card
    pure g
  PutAllyIntoPlayUnder pid card | allyEntryBarred pid g -> do
    let owner = card.owner >>= (`Map.lookup` g.players)
        held = maybe False (\p -> any ((== card.id) . (.id)) (p.hand <> p.deck <> p.discard)) owner
    pushWhen (not held) $ PutInDiscard card
    pure g
  -- a location that cannot enter the staging area stays where it is, as Shrine to Morgoth
  PlaceLocation lid StagingArea
    | Just l <- Map.lookup lid g.entities.locations
    , CannotEnterStagingArea `elem` map modifierType (Map.findWithDefault [] (toTarget l) (gameModifiers g)) ->
        pure g
  -- a location becoming the active location, however it does, opens its windows once it is
  PlaceLocation lid ActiveLocation
    | Just l <- Map.lookup lid g.entities.locations
    , l.placement /= ActiveLocation -> do
        -- on the Island Map the location traveled from stays in its place, its progress
        -- removed, and the staging area is the locations around the new one
        let onMap = isJust l.position
            leave x
              | isJust x.position && x.placement == ActiveLocation =
                  overAttrs (\a -> a {locationPlacement = OnIslandMap, locationTokens = removeAllTokens Progress a.locationTokens}) x
              | otherwise = x
        g' <- runGame'' msg (if onMap then g & entitiesL . locationsL %~ fmap leave else g)
        -- the enemies grappling it let go first (The Thing in the Depths)
        -- the Discover keyword resolves as it becomes the active location (MEC69 rules)
        lead <- getLead
        pushAll
          $ letGoOf g lid
          <> [ArrangeIslandMap | onMap]
          <> [CheckWindows [mkWhen $ BecameActiveLocation lid], ResolveDiscover lead lid, CheckWindows [mkAfter $ BecameActiveLocation lid]]
        pure g'
  -- and as it is explored, before it leaves play
  CheckWindows ws | explored@(_ : _) <- [lid | Window When (Explored lid) <- ws] -> do
    pushAll $ concatMap (letGoOf g) explored
    runGame'' msg g
  -- a Permanent boon or burden cannot be discarded from its hero while the hero is in play
  Discard _ t | permanentOnHero g t -> pure g
  -- a location encounter card effects cannot discard stays, as Mount Gundabad's campaign
  -- card keeps its unique locations: "Unique locations cannot be discarded from play by
  -- encounter card effects." Player cards and the game's rules still discard it.
  Discard source (LocationTarget lid)
    | isEncounterCardSource g source
    , CannotBeDiscardedByEncounterCards `elem` map modifierType (Map.findWithDefault [] (LocationTarget lid) (gameModifiers g)) ->
        pure g
  -- an Uncharted location's card cannot enter the discard pile or the encounter deck: it
  -- returns to the Uncharted deck (The Grey Havens rules insert)
  Discard _ (LocationTarget lid) | unchartedCard lid -> g <$ push (ShuffleIntoUnchartedDeck lid)
  ShuffleIntoEncounterDeck (LocationTarget lid) | unchartedCard lid -> g <$ push (ShuffleIntoUnchartedDeck lid)
  -- nor can a Grotto deck card: it returns to the Grotto deck (The Drowned Ruins)
  Discard _ (LocationTarget lid) | grottoCard lid -> g <$ push (ShuffleIntoGrottoDeck lid)
  ShuffleIntoEncounterDeck (LocationTarget lid) | grottoCard lid -> g <$ push (ShuffleIntoGrottoDeck lid)
  -- a quest card that cannot be defeated now keeps its progress, to be checked again once
  -- it can be (see 'checkUnblockedQuests')
  CompleteQuest qid -> do
    blocked <- elem CannotBeDefeated <$> getModifiers (QuestTarget qid)
    if blocked then pure g else runGame'' msg g
  _ -> runGame'' msg g
 where
  unchartedCard lid = maybe False (isUnchartedCard . toAttrs) (Map.lookup lid g.entities.locations)
  grottoCard lid = maybe False (isGrottoCard . toAttrs) (Map.lookup lid g.entities.locations)

runGame'' :: Message -> Game -> GameT Game
runGame'' msg g = case removedEnemy msg of
  -- an enemy that cannot leave play unless destroyed, as The Morgul Vale's Captains, stays,
  -- and so does one that cannot leave the staging area, as The Black Serpent
  Just eid -> do
    mods <- getModifiers (EnemyTarget eid)
    if CannotLeavePlay `elem` mods || stuckInStaging eid mods then pure g else runGame' msg g
  -- nor is such an enemy engaged or moved out of it
  Nothing | Just eid <- movedEnemy g msg -> do
    mods <- getModifiers (EnemyTarget eid)
    if stuckInStaging eid mods then pure g else runGame' msg g
  Nothing -> case movedLocation g msg of
    -- a location that cannot leave the staging area, as The Crossing at Tharbad, stays
    Just lid | Just l <- Map.lookup lid g.entities.locations, l.placement `elem` [StagingArea, OrcsArea] -> do
      stays <- elem CannotLeaveStagingArea <$> getModifiers (LocationTarget lid)
      if stays then pure g else runGame' msg g
    _ -> runGame' msg g
 where
  stuckInStaging eid mods =
    CannotLeaveStagingArea `elem` mods && maybe False ((`elem` [StagingArea, OrcsArea]) . (.placement)) (Map.lookup eid g.entities.enemies)

-- | The enemy a message moves out of where it is, without taking it out of play.
movedEnemy :: Game -> Message -> Maybe EnemyId
movedEnemy g = \case
  EngageEnemy _ eid -> Just eid
  PlaceEnemy eid p | maybe False ((/= p) . (.placement)) (Map.lookup eid g.entities.enemies) -> Just eid
  _ -> Nothing

-- | The location a message moves out of where it is, other than by exploring it.
movedLocation :: Game -> Message -> Maybe LocationId
movedLocation g = \case
  PlaceLocation lid p | maybe False ((/= p) . (.placement)) (Map.lookup lid g.entities.locations) -> Just lid
  ReturnToTopOfEncounterDeck (LocationTarget lid) -> Just lid
  ReturnToBottomOfEncounterDeck (LocationTarget lid) -> Just lid
  ShuffleIntoEncounterDeck (LocationTarget lid) -> Just lid
  RemoveFromGame (LocationTarget lid) -> Just lid
  _ -> Nothing

runGame' :: Message -> Game -> GameT Game
runGame' msg g = case leavingCharacter g msg of
  -- a character leaving play opens the When window first, while it is still in play
  Just target -> do
    windows <- leftPlayWindows g target
    -- a character to be removed from the game if it leaves play is, however it leaves, as
    -- a player card with the Encounter keyword (MEC67 rules)
    removed <- case msg of
      RemoveFromGame _ -> pure False
      _ -> elem RemoveFromGameIfLeavesPlay <$> getModifiers target
    pushAll [CheckWindows (map mkWhen windows), LeavePlay $ if removed then RemoveFromGame target else msg]
    pure g
  Nothing -> case msg of
    LeavePlay inner -> runInPlay inner g
    _ -> runInPlay msg g

-- | The character a message takes out of play, while it is still in play.
leavingCharacter :: Game -> Message -> Maybe Target
leavingCharacter g = \case
  Discard _ t | character t -> Just t
  ReturnToHand t | character t -> Just t
  ShuffleIntoOwnersDeck t | character t -> Just t
  PutOnBottomOfOwnersDeck t | character t -> Just t
  RemoveFromGame t | character t -> Just t
  _ -> Nothing
 where
  character = \case
    HeroTarget hid -> Map.member hid g.entities.heroes
    AllyTarget aid -> Map.member aid g.entities.allies
    _ -> False

{- | A treachery in a player's hand is out of play, but its Forced effects there answer the
windows, as Lost in the Wild's: its card's module runs through a temporary instance of it,
as a shadow card's does.
-}
runInHand :: Message -> Game -> GameT ()
runInHand msg g = case msg of
  CheckWindows _ ->
    for_ [ec | p <- toList g.players, EncounterCard ec <- p.hand, cdCardType (toCardDef ec) == TreacheryType] \ec ->
      void . runEach msg . createTreachery ec =<< getRandom
  _ -> pure ()

runInPlay :: Message -> Game -> GameT Game
runInPlay msg g =
  announce msg
    >> traverseOf (playersL . traverse) (runEach msg) g
    >>= traverseOf (activeCostsL . traverse) (runEach msg)
    >>= traverseOf entitiesL (runEntities msg)
    >>= (\g' -> g' <$ runInHand msg g')
    >>= traverseOf scenarioL (runEach msg)
    >>= runGameMessage msg
    >>= \g' -> do
      -- an enemy, location or side quest entering play, or a stage turning to its B side,
      -- gets its Time X in time counters; the cards of another area coming into focus, or
      -- joining this one, are already in play
      let entered :: Ord k => Map k v -> Map k v -> [k]
          entered old new = Map.keys (Map.difference new old)
          wasFlipped qid = maybe False (.flipped) (Map.lookup qid g.entities.quests)
          enteredSideQuest q = isSideQuest q && not (Map.member (toId q) g.entities.quests)
          movingAreas = case msg of
            FocusArea _ -> True
            JoinArea {} -> True
            SeparatePlayers _ -> True
            SeparateStagingAreas _ -> True
            PlaceInStagingAreaOf {} -> True
            _ -> False
          -- a location leaving play is kept aside for its own abilities to answer it
          departed = if movingAreas then mempty else Map.difference g.entities.locations g'.entities.locations
          departedEnemies = if movingAreas then [] else Map.keys (Map.difference g.entities.enemies g'.entities.enemies)
      unless movingAreas
        $ pushAll
        $ map (PlaceTimeCounters . EnemyTarget) (entered g.entities.enemies g'.entities.enemies)
        <> map (PlaceTimeCounters . LocationTarget) (entered g.entities.locations g'.entities.locations)
        <> [PlaceTimeCounters (toTarget q) | q <- toList g'.entities.quests, q.flipped, not (wasFlipped $ toId q)]
        <> [PlaceTimeCounters (toTarget q) | q <- toList g'.entities.quests, enteredSideQuest q]
        <> [CheckWindows [mkAfter $ LocationLeftPlay lid] | lid <- Map.keys departed]
        <> [CheckWindows [mkAfter $ EnemyLeftPlay eid] | eid <- departedEnemies]
      -- an enemy entering play, not one turned over from its other side, which stays in play;
      -- the resources placed on its card while it was out of play come with it
      let arrived = if movingAreas || isFlip then [] else entered g.entities.enemies g'.entities.enemies
          isFlip = case msg of
            Flip _ -> True
            _ -> False
          waiting = scenarioOutOfPlayResources (toAttrs g'.scenario)
          carried = [(eid, n) | eid <- arrived, Just e <- [Map.lookup eid g'.entities.enemies], Just n <- [Map.lookup (toCard e).id waiting]]
      pushAll [PlaceTokens GameSource (EnemyTarget eid) Resource n | (eid, n) <- carried]
      -- one put into play engaged has entered play once it has engaged
      queued <- peekQueue
      for_ arrived \eid -> do
        let engaging = \case
              EngageEnemy _ eid' -> eid' == eid
              _ -> False
            window = CheckWindows [mkAfter $ EnemyEnteredPlay eid]
        if any engaging queued then insertAfterMatching [window] engaging else push window
      let gone = [(toCard e).id | (eid, _) <- carried, Just e <- [Map.lookup eid g'.entities.enemies]]
          g'' =
            g'
              & leftPlayLocationsL %~ Map.union departed
              & scenarioL %~ overAttrs (outOfPlayResourcesL %~ \m -> foldr Map.delete m gone)
      case msg of
        -- the window goes in front of the phase's own steps
        BeginPhase phase -> g'' <$ push (CheckWindows [mkAfter $ PhaseBegins phase])
        _ -> pure g''

