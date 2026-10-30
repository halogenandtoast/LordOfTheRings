{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module X) where

import Control.Monad.Writer.Strict (execWriterT)
import Data.List (mapAccumL)
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import LordOfTheRings.Ability.Types
import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally
import LordOfTheRings.Attachment hiding (attachment, attachTo)
import LordOfTheRings.Card hiding (attachment, enemy, location, treachery)
import LordOfTheRings.Card.Synthetic (orcGuard)
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Criteria (Criterion (..))
import LordOfTheRings.Effect
import LordOfTheRings.Enemy hiding (enemy)
import LordOfTheRings.Entities
import LordOfTheRings.Event
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base as X
import LordOfTheRings.GameT
import LordOfTheRings.Helpers.Attachment hiding (hosts)
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Helpers.Window
import LordOfTheRings.Hero
import LordOfTheRings.I18n
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Keyword
import LordOfTheRings.Location hiding (location)
import LordOfTheRings.Log.Block
import LordOfTheRings.Log.Narrator
import LordOfTheRings.Log.Refs (sendLogInOpenBlock)
import LordOfTheRings.Matcher
import LordOfTheRings.Matcher.Window (windowMatches)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (raiseThreatFromDoomed)
import LordOfTheRings.Modifier
import LordOfTheRings.Objective
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (..), placementController)
import LordOfTheRings.Player.Runner as X
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest
import LordOfTheRings.Question
import LordOfTheRings.Scenario
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Trait (Trait)
import LordOfTheRings.Treachery hiding (treachery)
import LordOfTheRings.Window

newGame :: Player -> [Hero] -> Game
newGame = newGameIn passageThroughMirkwood

newGameIn :: Scenario -> Player -> [Hero] -> Game
newGameIn gameScenario player heroes = newTableIn gameScenario ((player, heroes) :| [])

-- | A game with a seat for each player, in the order given.
newTableIn :: Scenario -> NonEmpty (Player, [Hero]) -> Game
newTableIn gameScenario seats@((player, _) :| _) =
  Game
    { gamePlayers = Map.fromList [(p.id, p) | (p, _) <- toList seats]
    , gamePlayerOrder = [p.id | (p, _) <- toList seats]
    , gameActivePlayer = player.id
    , gameFirstPlayer = player.id
    , gameEntities = mempty {entitiesHeroes = Map.fromList [(h.id, h) | (_, hs) <- toList seats, h <- hs]}
    , gameScenario
    , gamePhase = ResourcePhase
    , gameRound = 0
    , gameQuestion = mempty
    , gameQuestionVersion = 0
    , gameActiveCosts = mempty
    , gameModifiers = mempty
    , gameEffects = []
    , gameOutcome = Nothing
    , gameLastCommittedWillpower = Nothing
    , gameLogBlocks = []
    , gameLeftPlay = mempty
    , gameAttackDestroyed = []
    , gameSeparation = Nothing
    }

answerQuestion :: PlayerId -> Int -> GameT Game
answerQuestion pid n = do
  g <- getGame
  question <- maybe (error "no question for player") pure $ Map.lookup pid g.question
  let choices = questionChoices question
  choice <- maybe (error "invalid choice") pure $ atMay choices n
  putGame $ g & questionL %~ Map.delete pid
  let rest = take n choices <> drop (n + 1) choices
      remaining = \case
        ChooseOneAtATime _ | notNull rest -> Just $ ChooseOneAtATime rest
        QuestionLabel l s ts q -> QuestionLabel l s ts <$> remaining q
        SearchCards cs q -> SearchCards cs <$> remaining q
        _ -> Nothing
  pushAll $ uiToRun choice : [Ask pid q | Just q <- [remaining question]]
  mainLoop

-- | Run messages until the queue empties or someone has to answer a question.
mainLoop :: GameT Game
mainLoop = go (0 :: Int)
 where
  go n = do
    g <- getGame
    if
      | notNull g.question || isJust g.outcome -> pure g
      | n >= messageLimit -> error "message limit reached: the game is probably looping"
      | otherwise -> runNext >>= bool getGame (go (n + 1))

{- | Run a debug message now, even while a question waits: it and whatever it leads to go
first, then the waiting question is asked again (a window is built afresh, as what it
offers may have changed) and play carries on from there.
-}
runDebug :: Message -> GameT Game
runDebug msg = do
  asked <- getsGame (.question)
  waiting <- peekQueue
  putGame . (questionL .~ mempty) =<< getGame
  setQueue $ msg
    : [fromMaybe (Ask pid q) (questionRebuild q) | (pid, q) <- Map.toList asked] <> waiting
  mainLoop

messageLimit :: Int
messageLimit = 10000

-- | Run the next queued message, returning False when the queue is empty.
runNext :: GameT Bool
runNext =
  popMessage >>= \case
    Nothing -> pure False
    Just (Ask pid q) -> do
      g <- getGame
      putGame $ g & questionL .~ Map.singleton pid q & questionVersionL +~ 1
      pure True
    Just msg -> do
      debug 1 $ print msg
      -- the log, read off the game as the message finds it
      for_ (narrationFor msg) \build -> traverse_ sendLogInOpenBlock =<< build
      -- store the result first, so the constant abilities read the game as it now is
      g <- runGame msg =<< getGame
      putGame g
      putGame =<< withHandModifiers =<< preloadModifiers g
      discardAtNoWillpower
      endWaitingGame
      pure True

-- | A game that could not end, as Ghulat's, ends with the outcome that waited once it can.
endWaitingGame :: GameT ()
endWaitingGame = do
  g <- getGame
  let waiting = [o | e <- gameEffects g, GameEndsWith o <- effectModifiers e]
      isWaiting = any (\case GameEndsWith _ -> True; _ -> False) . effectModifiers
  for_ (headMay waiting) \outcome -> do
    cannotEnd <- elem GameCannotEnd <$> getModifiers GameTarget
    unless cannotEnd do
      putGame $ g & effectsL %~ filter (not . isWaiting)
      push $ GameOver outcome

{- | Characters that are discarded once their willpower is 0, as The Mountains' Peaks
reads, are discarded as soon as it is, unless their discard is already on its way.
-}
discardAtNoWillpower :: GameT ()
discardAtNoWillpower = do
  over <- getsGame (isJust . (.outcome))
  unless over do
    characters <- selectCharacters AnyHero AnyAlly
    doomed <- flip filterM characters \c -> andM [elem DiscardedAtNoWillpower <$> getModifiers c, (<= 0) <$> getWillpower c]
    queued <- peekQueue
    pushAll [Discard c | c <- doomed, Discard c `notElem` queued, LeavePlay (Discard c) `notElem` queued]

{- | The modifiers cards in hand give themselves, as what they cost to play: each ally and
event in a hand, or being paid for, is built from its card to be asked.
-}
withHandModifiers :: Game -> GameT Game
withHandModifiers g = do
  MonoidalMap fromHands <- execWriterT do
    for_ g.players \player -> for_ player.hand (handCardModifiers player.id)
    for_ (gameActiveCosts g) \ac -> handCardModifiers ac.activeCostPlayer ac.activeCostCard
  pure
    $ if null fromHands then g else g {gameModifiers = Map.unionWith (<>) (gameModifiers g) fromHands}

-- | The modifiers a card gives itself from its owner's hand, or while being paid for.
handCardModifiers :: HasModifiersM m => PlayerId -> Card -> m ()
handCardModifiers pid = \case
  PlayerCard pc -> case cdCardType (toCardDef pc) of
    AllyType -> getModifiersFor $ createAlly pc (AllyId UUID.nil) pid
    EventType -> traverse_ getModifiersFor $ lookupEvent (toCardCode pc) pc.id pid
    -- an attachment's builder is unplaced, as a Record that costs less in hand reads it
    AttachmentType -> getModifiersFor $ lookupAttachment (toCardCode pc) pc.id (AttachmentId UUID.nil) pid
    _ -> pure ()
  EncounterCard _ -> pure ()

getPlayers :: HasGame m => m [Player]
getPlayers = toList <$> getsGame gamePlayers

instance FromJSON Entities where
  parseJSON = genericParseJSON (aesonOptions $ Just "entities") . withDefaults [("objectives", object [])]

instance FromJSON Game where
  parseJSON =
    genericParseJSON (aesonOptions $ Just "game")
      . withDefaults [("logBlocks", toJSON ([] :: [Value])), ("leftPlay", object []), ("attackDestroyed", toJSON ([] :: [Value])), ("separation", Null)]

instance FromJSON Separation where
  parseJSON = genericParseJSON (aesonOptions $ Just "separation")

instance FromJSON Area where
  parseJSON = genericParseJSON (aesonOptions $ Just "area")

{- | The players split into areas, one for each group: each player takes his cards in
play, those engaged with him and those attached to them, and the encounter cards no
player holds stay with the first group. The effects on a player or a card go with it,
and those on the game apply in every area. The areas are separated during a phase, so
each has begun it, and the first player's is in focus.
-}
separate :: [[PlayerId]] -> Game -> Game
separate groups g
  | isJust (gameSeparation g) || length groups < 2 = g
  | otherwise =
      focusArea g.firstPlayer
        g
          { gamePlayers = mempty
          , gameEntities = mempty
          , gameEffects = []
          , gameSeparation = Just $ Separation g.firstPlayer (zipWith area [0 ..] groups)
          }
 where
  es = g.entities
  area :: Int -> [PlayerId] -> Area
  area i pids =
    Area
      { areaPlayers = pids
      , areaHeldPlayers = Map.filterWithKey (\k _ -> k `elem` pids) g.players
      , areaHeldEntities =
          Entities
            { entitiesHeroes = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.heroes
            , entitiesAllies = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.allies
            , entitiesEnemies = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.enemies
            , entitiesLocations = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.locations
            , entitiesTreacheries = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.treacheries
            , entitiesQuests = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.quests
            , entitiesAttachments = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.attachments
            , entitiesObjectives = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.objectives
            }
      , areaHeldEffects = filter (\e -> maybe True (== i) (effectGroup e)) (gameEffects g)
      , areaStatus = AreaStarted
      , areaJoining = False
      }
   where
    inGroup t = groupOf t == i
  groupOf t = case holderOf t of
    Just pid | Just i <- findIndex (elem pid) groups -> i
    _ -> 0
  effectGroup e = case effectTarget e of
    GameTarget -> Nothing
    t -> Just (groupOf t)
  holderOf :: Target -> Maybe PlayerId
  holderOf = \case
    PlayerTarget pid -> Just pid
    HeroTarget k -> (.owner) <$> Map.lookup k es.heroes
    AllyTarget k -> placed . (.placement) =<< Map.lookup k es.allies
    EnemyTarget k -> placed . (.placement) =<< Map.lookup k es.enemies
    LocationTarget k -> placed . (.placement) =<< Map.lookup k es.locations
    TreacheryTarget k -> placed . (.placement) =<< Map.lookup k es.treacheries
    ObjectiveTarget k -> placed . (.placement) =<< Map.lookup k es.objectives
    -- a Trap in the staging area follows its owner (ALeP 3.382)
    AttachmentTarget k -> Map.lookup k es.attachments <&> \a -> fromMaybe (attachmentOwner $ toAttrs a) (placed a.placement)
    QuestTarget k -> questOwner . toAttrs =<< Map.lookup k es.quests
    _ -> Nothing
  placed = \case
    AttachedTo t -> holderOf t
    p -> placementController p

{- | While the players are separated each area resolves the step in turn, in turn order:
what follows the step in the area in focus is the step again in the next area still to
resolve it (the messages given, from how far that area is), and once every area has,
what follows it, back with the first player.
-}
eachAreaThen :: (AreaStatus -> [Message]) -> (Separation -> Separation) -> Message -> Game -> (Message, Game)
eachAreaThen again finish next g = case gameSeparation g of
  Nothing -> (next, g)
  Just s0 ->
    let s = s0 {separationAreas = [if isFocusedArea g a then a {areaStatus = AreaDone} else a | a <- separationAreas s0]}
     in case [a | a <- areasInTurnOrder g s, areaStatus a /= AreaDone] of
          (a : _) -> (Run (FocusArea (areaLead g s a) : again (areaStatus a)), g {gameSeparation = Just s})
          [] ->
            let s' = finish s {separationAreas = [a {areaStatus = AreaWaiting} | a <- separationAreas s]}
             in (Run [FocusArea (separationFirstPlayer s'), next], g {gameSeparation = Just s'})

{- | Run the message on one thing in play, putting what it pushes on the queue once it is
done.
-}
runEach :: RunMessage a => Message -> a -> GameT a
runEach msg = runQueueT . runMessage msg

runEntities :: Message -> Entities -> GameT Entities
runEntities msg entities =
  traverseOf (heroesL . traverse) (runEach msg) entities
    >>= traverseOf (alliesL . traverse) (runEach msg)
    >>= traverseOf (locationsL . traverse) (runEach msg)
    >>= traverseOf (enemiesL . traverse) (runEach msg)
    >>= traverseOf (treacheriesL . traverse) (runEach msg)
    >>= traverseOf (questsL . traverse) (runEach msg)
    >>= traverseOf (attachmentsL . traverse) (runEach msg)
    >>= traverseOf (objectivesL . traverse) (runEach msg)

runGame :: Message -> Game -> GameT Game
runGame msg g = case msg of
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
  PlaceTokens source target@(QuestTarget _) Progress n | n > 0 -> do
    mods <- getModifiers target
    placed <- progressPlacedThisRound target
    let room = foldr min n [max 0 (k - placed) | ProgressPerRoundLimit k <- mods]
    if room < n
      then g <$ pushWhen (room > 0) (PlaceTokens source target Progress room)
      else do
        push $ CreateEffect $ Effect GameSource target [ProgressPlacedThisRound n] UntilEndOfRound
        runGame'' msg g
  -- a location's Travel cost, which its card pushes as it sees TravelTo, is paid before
  -- it becomes the active location, so it is still in the staging area as it is paid (as
  -- Hills of Dunland's draws are, FAQ 3.123)
  TravelTo _ -> do
    push $ DoStep 1 msg
    runGame'' msg g
  -- an enemy that engages only one player, as Chief Turch the first player, engages him
  -- whoever it would have engaged
  EngageEnemy pid eid -> do
    only <- (\ms -> [p | EngagesOnly p <- ms]) <$> getModifiers (EnemyTarget eid)
    case only of
      p : _ | p /= pid -> g <$ push (EngageEnemy p eid)
      _ -> runGame'' msg g
  _ -> runGame'' msg g

runGame'' :: Message -> Game -> GameT Game
runGame'' msg g = case removedEnemy msg of
  -- an enemy that cannot leave play unless destroyed, as The Morgul Vale's Captains, stays
  Just eid -> do
    stays <- elem CannotLeavePlay <$> getModifiers (EnemyTarget eid)
    if stays then pure g else runGame' msg g
  Nothing -> case movedLocation msg of
    -- a location that cannot leave the staging area, as The Crossing at Tharbad, stays
    Just lid | Just l <- Map.lookup lid g.entities.locations, l.placement == StagingArea -> do
      stays <- elem CannotLeaveStagingArea <$> getModifiers (LocationTarget lid)
      if stays then pure g else runGame' msg g
    _ -> runGame' msg g

-- | The location a message moves out of where it is, other than by exploring it.
movedLocation :: Message -> Maybe LocationId
movedLocation = \case
  PlaceLocation lid p | p /= StagingArea -> Just lid
  ReturnToTopOfEncounterDeck (LocationTarget lid) -> Just lid
  ReturnToBottomOfEncounterDeck (LocationTarget lid) -> Just lid
  ShuffleIntoEncounterDeck (LocationTarget lid) -> Just lid
  RemoveFromGame (LocationTarget lid) -> Just lid
  _ -> Nothing

-- | The enemy a message takes out of play other than by destroying it.
removedEnemy :: Message -> Maybe EnemyId
removedEnemy = \case
  Discard (EnemyTarget eid) -> Just eid
  ReturnToTopOfEncounterDeck (EnemyTarget eid) -> Just eid
  ReturnToBottomOfEncounterDeck (EnemyTarget eid) -> Just eid
  ShuffleIntoEncounterDeck (EnemyTarget eid) -> Just eid
  RemoveFromGame (EnemyTarget eid) -> Just eid
  AddToVictoryDisplay (EnemyTarget eid) -> Just eid
  _ -> Nothing

runGame' :: Message -> Game -> GameT Game
runGame' msg g = case leavingCharacter g msg of
  -- a character leaving play opens the When window first, while it is still in play
  Just target -> do
    windows <- leftPlayWindows g target
    pushAll [CheckWindows (map mkWhen windows), LeavePlay msg]
    pure g
  Nothing -> case msg of
    LeavePlay inner -> runInPlay inner g
    _ -> runInPlay msg g

-- | The character a message takes out of play, while it is still in play.
leavingCharacter :: Game -> Message -> Maybe Target
leavingCharacter g = \case
  Discard t | character t -> Just t
  ReturnToHand t | character t -> Just t
  ShuffleIntoOwnersDeck t | character t -> Just t
  RemoveFromGame t@(HeroTarget _) | character t -> Just t
  _ -> Nothing
 where
  character = \case
    HeroTarget hid -> Map.member hid g.entities.heroes
    AllyTarget aid -> Map.member aid g.entities.allies
    _ -> False

runInPlay :: Message -> Game -> GameT Game
runInPlay msg g =
  announce msg
    >> traverseOf (playersL . traverse) (runEach msg) g
    >>= traverseOf (activeCostsL . traverse) (runEach msg)
    >>= traverseOf entitiesL (runEntities msg)
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
            _ -> False
      unless movingAreas
        $ pushAll
        $ map (PlaceTimeCounters . EnemyTarget) (entered g.entities.enemies g'.entities.enemies)
        <> map (PlaceTimeCounters . LocationTarget) (entered g.entities.locations g'.entities.locations)
        <> [PlaceTimeCounters (toTarget q) | q <- toList g'.entities.quests, q.flipped, not (wasFlipped $ toId q)]
        <> [PlaceTimeCounters (toTarget q) | q <- toList g'.entities.quests, enteredSideQuest q]
      case msg of
        -- the window goes in front of the phase's own steps
        BeginPhase phase -> g' <$ push (CheckWindows [mkAfter $ PhaseBegins phase])
        _ -> pure g'

runGameMessage :: Message -> Game -> GameT Game
runGameMessage msg g = case msg of
  -- the rest of an attack does nothing once its enemy has left play
  _ | Just eid <- attackingEnemy msg, not (Map.member eid g.entities.enemies) -> pure g
  Run msgs -> do
    pushAll msgs
    pure g
  WhileInPlay t msgs -> do
    when (inPlay g t) $ pushAll msgs
    pure g
  IfAttackDestroyed eid msgs -> do
    whenM (attackDestroyedACharacter eid) $ pushAll msgs
    pure g
  OpenLogBlock block -> pure $ g & logBlocksL %~ (<> [block])
  -- an ally that has just left play answers its own leaving
  UseCardAbility _ (AllySource aid) _ _ | Just a <- Map.lookup aid (gameLeftPlay g) -> do
    a' <- runEach msg a
    pure $ g & leftPlayL . ix aid .~ a'
  ForgetLeftPlay aid -> pure $ g & leftPlayL %~ Map.delete aid
  CloseLogBlock bid -> pure $ g & logBlocksL %~ filter ((/= bid) . (.id))
  Setup -> do
    pushAll [DoStep n Setup | n <- [1 .. 7]]
    pure g
  DoStep 3 Setup -> do
    firstPlayer <- uniform g.playerOrder
    push $ SetFirstPlayer firstPlayer
    pure g
  BeginRound -> do
    pushAll [BeginPhase minBound, EndPhase minBound]
    pure $ g & roundL +~ 1
  EndRound -> do
    -- each area ends the round in turn
    let (step, g') = eachAreaThen (const [EndRound]) id BeginRound g
    unless (isJust g.outcome) do
      leaving <- filterM (fmap (elem DiscardAtEndOfRound) . getModifiers) (toList g.entities.allies)
      pushAll [Discard (toTarget a) | a <- leaving]
      returning <- filterM (fmap (elem ReturnToHandAtEndOfRound) . getModifiers) (toList g.entities.allies)
      pushAll [ReturnToHand (toTarget a) | a <- returning, toId a `notElem` map toId leaving]
      -- a card passed by Message from Elrond, in a hand or in play
      let shuffled :: Targetable t => t -> GameT Bool
          shuffled = fmap (elem ShuffleIntoOwnersDeckAtEndOfRound) . getModifiers
      allies <- filterM shuffled (toList g.entities.allies)
      attachments <- filterM (shuffled . CardIdTarget . attachmentCardId . toAttrs) (toList g.entities.attachments)
      inHands <- filterM (shuffled . CardIdTarget . snd) [(pid, c.id) | (pid, p) <- mapToList g.players, c <- p.hand]
      pushAll
        $ [ShuffleIntoOwnersDeck (toTarget a) | a <- allies, toId a `notElem` map toId (leaving <> returning)]
        <> [ShuffleIntoOwnersDeck (toTarget a) | a <- attachments]
        <> [ShuffleFromHandIntoOwnersDeck pid cid | (pid, cid) <- inHands]
      push $ CheckWindows [mkWhen RoundEnds]
      pushEnd step
    pure $ g' & effectsL %~ filter ((/= UntilEndOfRound) . effectDuration)
  BeginPhase RefreshPhase -> do
    -- an enemy with regenerate heals as much damage, unless Saruman holds it out of play
    present <- filterM (notOutOfPlay . toTarget) (toList g.entities.enemies)
    regenerating <- for present \e -> do
      mods <- getModifiers e
      let printed = [n | Blank `notElem` mods, Regenerate n <- toList (cdKeywords $ toCardDef e)]
      pure [HealDamage (toSource e) (toTarget e) n | n <- printed <> [n | GainKeyword (Regenerate n) <- mods]]
    heroes <- filterM (fmap (notElem DoesNotReadyDuringRefresh) . getModifiers) =<< select ExhaustedHero
    allies <- select ExhaustedAlly
    players <- getPlayerOrder
    -- a player who may refresh no more characters than some, as Heart of Fangorn reads,
    -- chooses which, if they would refresh more
    limits <- for players \pid -> (pid,) . (\ms -> minimumMay [n | RefreshAtMost n <- ms]) <$> getModifiers (PlayerTarget pid)
    let characters = map toTarget heroes <> map toTarget allies
        controller = \case
          HeroTarget hid -> (.owner) <$> Map.lookup hid g.entities.heroes
          AllyTarget aid -> placementController . (.placement) =<< Map.lookup aid g.entities.allies
          _ -> Nothing
        limited =
          [ (pid, n, cs)
          | (pid, Just n) <- limits
          , let cs = [c | c <- characters, controller c == Just pid]
          , length cs > n
          ]
        held = concat [cs | (_, _, cs) <- limited]
    -- separated, the token passes once every area has refreshed: see 'eachAreaThen'
    let passToken = case drop 1 players of
          (next : _) | isNothing (gameSeparation g) -> [SetFirstPlayer next]
          _ -> []
    -- each character readied opens its own window, for responses to it readying
    readied <- for (filter (`notElem` held) characters) \c -> do
      traits <- characterTraits c
      pure [Ready c, CheckWindows [mkAfter $ Readied c traits]]
    pushAll
      $ concat regenerating
      <> concat readied
      <> [RefreshUpTo pid n cs | (pid, n, cs) <- limited]
      <> [Ready (toTarget a) | a <- toList g.entities.attachments, attachmentExhausted (toAttrs a)]
      <> [Ready (toTarget o) | o <- toList g.entities.objectives, o.exhausted]
      <> [RaiseThreat pid 1 | pid <- players]
      <> passToken
      -- the phase ends with an action window, for Refresh Actions as Escape from Darkness gives
      <> map OptionalActionWindow players
    pure $ g & phaseL .~ RefreshPhase
  SetFirstPlayer pid -> pure $ g & firstPlayerL .~ pid & separationL %~ fmap (\s -> s {separationFirstPlayer = pid})
  SeparatePlayers groups -> pure $ separate groups g
  FocusArea pid -> pure $ focusArea pid g
  JoinAtTravel ->
    pure $ g & separationL %~ fmap (\s -> s {separationAreas = [if isFocusedArea g a then a {areaJoining = True} else a | a <- separationAreas s]})
  JoinArea joining target -> pure $ joinArea joining target g
  -- in turn order each area whose quest card is defeated joins another, its active location
  -- discarded; with no other to join, all the players continue to the next stage together
  JoinAreas | Just s <- gameSeparation g -> do
    let ordered = areasInTurnOrder g s
        lead = areaLead g s
        entitiesOf a = if isFocusedArea g a then g.entities else areaHeldEntities a
        joinTo j t =
          FocusArea (lead j)
            : [Discard (toTarget l) | l <- toList (entitiesOf j).locations, l.placement == ActiveLocation]
            <> [JoinArea (lead j) (lead t)]
    case break areaJoining ordered of
      (_, []) -> pure ()
      (before, j : after) -> case filter (not . areaJoining) (before <> after) of
        [] ->
          pushAll
            $ concat [joinTo a j | a <- before <> after]
            <> [FocusArea (lead j)]
            <> take 1 [CompleteQuest (toId q) | q <- toList (entitiesOf j).quests, not (isSideQuest q)]
        [t] -> pushAll $ joinTo j t <> [JoinAreas]
        targets ->
          pushAll
            [ FocusArea (lead j)
            , questionLabel (ikey "label.chooseAreaToJoin") (lead j)
                [targetLabel (PlayerTarget $ lead t) (joinTo j t <> [JoinAreas]) | t <- targets]
            ]
    pure g
  RefreshUpTo pid n cs | n > 0, notNull cs -> do
    choices <- for cs \c -> do
      traits <- characterTraits c
      pure $ targetLabel c [Ready c, CheckWindows [mkAfter $ Readied c traits], RefreshUpTo pid (n - 1) (filter (/= c) cs)]
    push $ questionLabel (ikeyWith "label.refreshUpTo" [intVar "count" n]) pid choices
    pure g
  CheckEliminations -> do
    emptyDeck <- elem EliminatedWithEmptyDeck <$> getModifiers GameTarget
    eliminated <- forMaybeM (toList g.players) \player -> do
      heroes <- select (HeroControlledBy $ PlayerWithId player.id)
      -- the level may move, as Double Dealings lowers it
      level <- (eliminationThreat +) . sum . mapMaybe (\case EliminationThreatModifier n -> Just n; _ -> Nothing) <$> getModifiers (PlayerTarget player.id)
      pure
        -- a hero shuffled into the deck, as Lost and Alone does, is not killed (FAQ 2.37)
        $ guard (not player.eliminated && (player.threat >= level || (null heroes && not (any ((== HeroType) . (.cardType)) player.deck)) || (emptyDeck && null player.deck)))
        $> player.id
    pushAll $ map EliminatePlayer eliminated
    pure g
  EliminatePlayer pid -> do
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    allies <- select (AllyControlledBy $ PlayerWithId pid)
    engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
    let remaining = [p | p <- toList (allPlayers g), p.id /= pid, not p.eliminated]
        -- a separated player alone in his area takes it out of the game with him: the
        -- encounter cards there are discarded (ALeP 3.030), and it joins another area
        alone = isJust (gameSeparation g) && null [p | p <- toList g.players, p.id /= pid, not p.eliminated]
        others = [areaLead g s a | Just s <- [gameSeparation g], a <- areasInTurnOrder g s, not (isFocusedArea g a)]
        encounter =
          [toTarget e | e <- toList g.entities.enemies, e.placement == StagingArea]
            <> [toTarget l | l <- toList g.entities.locations, l.placement `elem` [StagingArea, ActiveLocation]]
    pushAll
      $ map (Discard . toTarget) heroes
      <> map (Discard . toTarget) allies
      <> ( if alone
             then map (Discard . toTarget) engaged <> map Discard encounter <> [JoinArea pid other | other <- take 1 others]
             else [PlaceEnemy eid StagingArea | eid <- engaged]
         )
      <> [GameOver Lost | null remaining]
    pure g
  GameOver outcome -> do
    cannotEnd <- elem GameCannotEnd <$> getModifiers GameTarget
    -- the outcome waits while the game cannot end, unless no player is left to play on;
    -- see 'endWaitingGame'
    if cannotEnd && any (not . (.eliminated)) g.players
      then g <$ push (CreateEffect $ Effect GameSource GameTarget [GameEndsWith outcome] UntilSpent)
      else do
        clearQueue
        pure $ g & outcomeL ?~ outcome
  -- an ally put into play by its card, as Emery is, discarded by its card
  Discard (CardIdTarget cid)
    | Just a <- find ((== cid) . allyCardId . toAttrs) (toList g.entities.allies) ->
        g <$ push (Discard $ toTarget a)
  Discard target -> discardTarget target g
  DealDamage source target n | n > 0 -> do
    mods <- getModifiers target
    -- damage another character takes in its place, as Song of Mocking's
    let redirected = ordNub $ filter (inPlay g) [t | DamageRedirectedTo t <- mods]
    lead <- getLead
    -- before it is dealt, a Forced effect may cancel it, as Patrol Leader's: see 'cancelDamage'
    -- damage redirected is applied where it goes, not redirected again, as two Songs of
    -- Mocking each choosing the other's hero
    let dealTo t = do
          mods' <- getModifiers t
          -- damage reduced to at most so much, as Nazgûl of Minas Morgul's to 1
          let n' = foldr min n [k | DamageReducedTo k <- mods']
          pure
            $ if CannotTakeDamage `elem` mods'
              then []
              else [CheckWindows [mkWhen $ DealtDamage t n'], DoStep 1 (DealDamage source t n')]
    unless (CannotTakeDamage `elem` mods) $ pushAll =<< case redirected of
      [t] -> dealTo t
      -- the first player picks which takes it, as "dealt to 1 Morgul Bodyguard" reads
      _ : _ -> do
        choices <- for redirected \t -> targetLabel t <$> dealTo t
        pure [questionLabelWith (ikey "label.chooseDamageRedirect") (Just target) [] lead choices]
      [] -> dealTo target
    pure g
  DoStep 1 (DealDamage source target n) -> do
    -- the damage an enemy deals may raise a player's threat as much, as Seedy Inn's shadow
    raising <- case source of
      EnemyAttackSource eid -> (\ms -> [pid | AttackDamageRaisesThreatOf pid <- ms]) <$> getModifiers eid
      _ -> pure []
    defending <- elem Defending <$> getModifiers target
    pushAll
      $ [CreateEffect $ Effect GameSource target [DamagedWhileDefending] UntilEndOfAttack | defending]
      <> [ PlaceTokens source target Damage n
         , CheckWindows [mkAfter $ DealtDamage target n]
         , CheckDefeated source target
         ]
      <> concat [[RaiseThreat pid n, CheckWindows [mkAfter $ ThreatRaisedByEncounter pid n]] | pid <- raising]
    pure g
  HealDamage source target n | n > 0 -> do
    damage <- getDamage target
    mods <- getModifiers target
    let healed = foldr min (min n damage) [k | HealLimit k <- mods]
    when (healed > 0 && CannotBeHealed `notElem` mods)
      $ pushAll [RemoveTokens source target Damage healed, CheckWindows [mkAfter $ Healed target source]]
    pure g
  ChooseAndDiscardFromHand pid source n | n > 0 -> do
    player <- getPlayer pid
    unless (null $ discardableHand player)
      $ push
      $ questionLabelWith
        (ikey "label.chooseCardToDiscardFromHand")
        (sourceTarget source)
        []
        pid
        [ targetLabel
            (CardIdTarget c.id)
            [DiscardFromHand pid c.id, ChooseAndDiscardFromHand pid source (n - 1)]
        | c <- discardableHand player
        ]
    pure g
  EachDiscardAnyFromHand source (pid : rest) discarded -> do
    player <- getPlayer pid
    push
      $ if null (discardableHand player)
        then EachDiscardAnyFromHand source rest discarded
        else
          questionLabelWith (ikey "label.discardAnyFromHand") (sourceTarget source) [] pid
            $ [ targetLabel (CardIdTarget c.id) [DiscardFromHand pid c.id, EachDiscardAnyFromHand source (pid : rest) (discarded <> [c])]
              | c <- discardableHand player
              ]
            <> [Label (ikey "label.doneDiscarding") [EachDiscardAnyFromHand source rest discarded]]
    pure g
  CheckDefeated source target -> do
    -- damage dealt in a batch can reach a card an earlier part of it already destroyed
    destroyed <- case target of
      HeroTarget hid | Map.member hid g.entities.heroes -> (>=) . (.damage) <$> getHero hid <*> getHitPoints target
      AllyTarget aid | Map.member aid g.entities.allies -> (>=) . (.damage) <$> getAlly aid <*> getHitPoints target
      -- an indestructible enemy keeps taking damage past its hit points
      EnemyTarget eid | Just e <- Map.lookup eid g.entities.enemies, Indestructible `notMember` cdKeywords (toCardDef e) ->
        (>=) . (.damage) <$> getEnemy eid <*> getEnemyHitPoints eid
      -- an objective ally in the staging area, as Celador
      ObjectiveTarget oid | Just o <- Map.lookup oid g.entities.objectives, isJust (cdHitPoints $ toCardDef o) ->
        (>=) <$> getDamage target <*> getHitPoints target
      _ -> pure False
    when destroyed $ push (Destroy source target)
    pure g
  Destroy source target@(EnemyTarget eid) -> do
    enemy <- getEnemy eid
    push $ CheckWindows [mkAfter $ EnemyDefeated eid]
    stays <- elem StaysInPlayWhenDefeated <$> getModifiers target
    let card = EncounterCard $ MkEncounterCard (enemyCardId $ toAttrs enemy) (toCardCode enemy)
        -- what a player's attack destroyed, for the attack to read as it resolves
        recorded = attackDestroyedL %~ (<> [AttackDestroyed source target Nothing card | PlayerAttackSource _ <- [source]])
    if stays then pure g else recorded <$> case cdVictoryPoints (toCardDef enemy) of
      Just _ -> do
        -- an objective it guarded as an attachment is discarded, as a Key of The Three Trials
        pushAll $ [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target] <> discardShadowsOf g target
        pure
          $ g
          & entitiesL
          . enemiesL
          %~ Map.delete eid
          & scenarioL
          %~ overAttrs (victoryDisplayL %~ (card :))
      Nothing -> discardTarget target g
  -- an objective ally destroyed in the staging area goes to the encounter discard pile
  Destroy _ (ObjectiveTarget oid) | Just o <- Map.lookup oid g.entities.objectives -> do
    push $ AddToEncounterDiscard [MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o)]
    pure $ g & entitiesL . objectivesL %~ Map.delete oid
  Destroy source target -> do
    let (card, controller) = case target of
          HeroTarget hid | Just h <- Map.lookup hid g.entities.heroes -> (Just $ toCard h, Just h.owner)
          AllyTarget aid | Just a <- Map.lookup aid g.entities.allies -> (Just $ toCard a, placementController a.placement)
          _ -> (Nothing, Nothing)
        -- one an enemy's attack destroyed with its damage, as "if this attack destroys a
        -- character" reads: not one its shadow card's damage destroyed (see 'attackDestroyed')
        byAttack = [(eid, c, pid) | EnemyAttackSource eid <- [source], Just c <- [card], Just pid <- [controller]]
    pushAll
      $ [CheckWindows [mkWhen $ CharacterDestroyed target c] | Just c <- [card]]
      <> [Discard target, CheckEliminations]
      <> [ CheckWindows
             $ mkAfter (CharacterDestroyed target c)
             : [mkAfter $ CharacterDestroyedByAttack eid target pid | (eid, _, pid) <- byAttack]
         | Just c <- [card]
         ]
    pure $ g & attackDestroyedL %~ (<> [AttackDestroyed (EnemyAttackSource eid) target (Just pid) c | (eid, c, pid) <- byAttack])
  BeginPhase PlanningPhase -> do
    players <- getPlayers
    pushAll [WithTarget (toTarget player) (DoPhase PlanningPhase) | player <- players]
    pure $ g & phaseL .~ PlanningPhase
  BeginPhase QuestPhase -> do
    order <- getPlayerOrder
    -- the first player may choose a side quest in the staging area to quest against, but
    -- not a quest card that cannot be the current quest, as Secrets of Rhudaur
    let choosable = filterM (fmap (notElem CannotBeCurrentQuest) . getModifiers)
    mainQuests <- choosable =<< getMainQuests
    sideQuests <- choosable =<< getSideQuests
    let chooseQuest =
          [ questionLabel (ikey "label.chooseCurrentQuest") lead
            $ [Label (ikey "label.questAgainstMainQuest") [] | notNull mainQuests]
            <> [targetLabel q [SetCurrentQuest q] | q <- sideQuests]
          | notNull sideQuests
          , lead <- take 1 order
          ]
    pushAll $ chooseQuest <> map CommitCharacters order <> [Staging] <> map ActionWindow order <> [QuestResolution]
    pure $ g & phaseL .~ QuestPhase
  BeginPhase TravelPhase -> do
    -- the players' actions come first, as Thrór's Map's, then the lead may travel
    pushAll [TravelActions, DoStep 1 (BeginPhase TravelPhase)]
    pure $ g & phaseL .~ TravelPhase
  DoStep 1 (BeginPhase TravelPhase) -> do
    locations <- travelableLocations
    lead <- getLead
    forced <- filterM (fmap (elem MustBeTraveledTo) . getModifiers) locations
    unless (null locations)
      $ push
      $ if null forced
        then
          questionLabel (ikey "label.travelToLocation") lead
            $ [targetLabel lid [TravelTo lid] | lid <- locations]
            <> [Label (ikey "label.doNotTravel") []]
        else questionLabel (ikey "label.mustTravel") lead [targetLabel lid [TravelTo lid] | lid <- forced]
    pure g
  DoStep 1 (TravelTo lid) -> do
    pushAll [PlaceLocation lid ActiveLocation, CheckWindows [mkAfter $ Traveled lid]]
    pure g
  BeginPhase EncounterPhase -> do
    order <- getPlayerOrder
    -- players may act after the optional engagements and after the engagement checks
    pushAll
      $ map OptionalEngagement order
      <> map OptionalActionWindow order
      <> [EngagementCheck pid 0 | pid <- take 1 order]
      <> map OptionalActionWindow order
    pure $ g & phaseL .~ EncounterPhase
  OptionalEngagement pid -> do
    staged <- select EnemyInStagingArea
    -- some enemies may only be engaged once they are alone in the staging area
    barredBy <- (\ms -> [m | CannotBeOptionallyEngagedBy m <- ms]) <$> getModifiers GameTarget
    enemies <- flip filterM staged \eid -> do
      mods <- getModifiers eid
      barred <- anyM (\m -> selectAny (PlayerWithId pid <> m)) $ barredBy <> [m | CannotBeOptionallyEngagedBy m <- mods]
      pure
        $ not barred
        && CannotEngagePlayer pid `notElem` mods
        && (length staged <= 1 || CannotBeOptionallyEngagedWhileOthersInStaging `notElem` mods)
    unless (null enemies)
      $ push
      $ questionLabel (ikey "label.optionalEngagement") pid
      $ [ targetLabel eid [EngageEnemy pid eid, CheckWindows [mkAfter $ OptionallyEngaged eid pid]]
        | eid <- enemies
        ]
      <> [Label (ikey "label.doNotEngage") []]
    pure g
  EngagementCheck pid seatsPassed -> do
    skip <- elem NoEngagementChecks <$> getModifiers GameTarget
    order <- getPlayerOrder
    let next = seatAfter order pid
    player <- getPlayer pid
    candidates <- traverse (\eid -> (eid,) <$> getEngagementCost eid) =<< select EnemyInStagingArea
    -- an enemy can be spared engagement checks against it, as a trap does
    checked <- filterM (fmap (\ms -> NoEngagementChecks `notElem` ms && CannotEngagePlayer pid `notElem` ms) . getModifiers . fst) candidates
    case sortOn (Down . snd) [c | not skip, c@(_, cost) <- checked, cost <= player.threat] of
      ((eid, _) : _) -> pushAll [EngageEnemy pid eid, EngagementCheck next 0]
      [] -> when (seatsPassed + 1 < length order) $ push (EngagementCheck next (seatsPassed + 1))
    pure g
  AmbushEngagementCheck eid | Just enemy <- Map.lookup eid g.entities.enemies, enemy.placement == StagingArea -> do
    ambush <- hasKeyword Ambush (toTarget eid) (toCardDef enemy)
    gameMods <- getModifiers GameTarget
    mods <- getModifiers eid
    let checked = ambush && IgnoreAmbush `notElem` gameMods && NoEngagementChecks `notElem` (gameMods <> mods)
    when checked do
      cost <- getEngagementCost eid
      players <- traverse getPlayer =<< getPlayerOrder
      for_ (find (\p -> p.threat >= cost && CannotEngagePlayer p.id `notElem` mods) players) \p -> push (EngageEnemy p.id eid)
    pure g
  EngageEnemy pid eid -> do
    pushAll
      [ CheckWindows [mkWhen $ EnemyEngaged eid pid]
      , PlaceEnemy eid (EngagedWith pid)
      , CreateEffect $ Effect GameSource (PlayerTarget pid) [EngagedAnEnemyThisRound] UntilEndOfRound
      , CheckWindows [mkAfter $ EnemyEngaged eid pid]
      ]
    pure g
  BeginPhase CombatPhase -> do
    order <- getPlayerOrder
    pushAll $ ResolveArchery : ResolveHinder : DealShadowCards
      : map ActionWindow order <> map EnemyAttacks order <> map PlayerAttacks order
    pure $ g & phaseL .~ CombatPhase
  ResolveArchery -> do
    total <- getArcheryTotal
    pushWhen (total > 0) $ ArcheryDamage total []
    pure g
  -- each enemy with hinder engaged with a player removes 1 progress from the current quest,
  -- from the active location once the quest has none
  ResolveHinder -> do
    n <- length <$> (filterM (hinders g) =<< select (EnemyEngagedWith AnyPlayer))
    current <- getCurrentQuest
    let questProgress = sum [q.progress | q <- toList g.entities.quests, Just (toTarget q) == current]
        rest = n - min n questProgress
    pushWhen (min n questProgress > 0) $ RemoveProgressFromQuest GameSource n
    for_ (find ((== ActiveLocation) . (.placement)) (toList g.entities.locations)) \l ->
      pushWhen (rest > 0 && l.progress > 0) $ RemoveTokens GameSource (toTarget l) Progress (min rest l.progress)
    pure g
  -- the players divide it as they see fit, the first player having the final say, so the
  -- first player assigns it; it is all assigned, then dealt at once (ALeP 3.088)
  ArcheryDamage n assigned -> do
    lead <- getLead
    alliesOnly <- elem ArcheryDamageToAllies <$> getModifiers GameTarget
    -- the players deal it to characters they control, not to one no one does, as captured
    allies <- withHitPointsLeft assigned . map toTarget =<< select (AllyControlledBy AnyPlayer)
    characters <-
      if alliesOnly && notNull allies
        then pure allies
        else withHitPointsLeft assigned =<< selectCharacters AnyHero (AllyControlledBy AnyPlayer)
    push
      $ if n > 0 && notNull characters
        then
          questionLabel (ikeyWith "label.assignArcheryDamage" [intVar "count" n]) lead
            $ [targetLabel c [ArcheryDamage (n - 1) (c : assigned)] | c <- characters]
        else Run $ dealAssigned GameSource assigned
    pure g
  DealShadowCards -> do
    order <- getPlayerOrder
    -- one engaged with each player from the staging area is dealt one for each attack
    -- an enemy with hinder is dealt none
    enemies <- filterM (\e -> not <$> orM [engagedWithEach e, hinders g e]) =<< concatMapM engagedByCost order
    pushAll $ map DealShadowCard enemies
    pure g
  EnemyAttacks pid -> do
    push . ChooseEnemyAttack pid =<< engagedByCost pid
    pure g
  ChooseEnemyAttack pid candidates -> do
    -- checked again before each attack: a card played between attacks may stop one
    engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
    enemies <-
      filterM
        (fmap (\ms -> CannotAttack `notElem` ms && CannotAttackPlayer pid `notElem` ms) . getModifiers)
        =<< filterM (fmap not . hinders g) [e | e <- candidates, e `elem` engaged]
    -- one engaged with each player is dealt a new shadow card for each attack, discarded after
    let attack eid = do
          each <- engagedWithEach eid
          pure $ [DealShadowCard eid | each] <> [EnemyAttack pid eid] <> [DiscardShadowCards eid | each]
    case enemies of
      [] -> pure ()
      [eid] -> pushAll =<< attack eid
      _ -> do
        choices <- for enemies \eid ->
          targetLabel eid . (<> [ChooseEnemyAttack pid (filter (/= eid) enemies)]) <$> attack eid
        push $ questionLabelWith (ikey "label.chooseNextAttack") Nothing (map toTarget enemies) pid choices
    pure g
  -- players may act at the end of each step of the attack: once its enemy is chosen,
  -- its defenders declared, its shadow resolved and its damage dealt
  EnemyAttack pid eid -> do
    windows <- actionWindows
    pushAll
      =<< inLogBlock
        (EnemyAttackBlock pid eid)
        (CheckWindows [mkWhen $ EnemyAttacking eid pid] : windows <> [DeclareDefenders pid eid []])
    -- what its last attack destroyed is forgotten as it makes a new one
    pure $ g & attackDestroyedL %~ filter ((/= EnemyAttackSource eid) . attackDestroyedBy)
  DefendWith pid eid defender -> do
    windows <- actionWindows
    pushAll
      $ [ defendingEffect defender
        , CheckWindows [mkAfter $ DeclaredAsDefender defender eid]
        ]
      <> windows
      <> [ResolveShadow eid pid [defender]]
      <> windows
      <> [ AttackDamage eid defender
         , AttackDefended pid eid [defender]
         , CheckWindows [mkAfter $ EnemyAttacking eid pid, mkAfter $ DefendedAttack defender eid]
         ]
      <> windows
    pure g
  DeclareDefenders pid eid chosen -> do
    -- an attack that cannot be defended, as Striking Tentacle's may be, is undefended
    undefendable <- elem CannotBeDefended <$> getModifiers eid
    many <- elem MayDeclareMultipleDefenders <$> getModifiers (PlayerTarget pid)
    others <- filter (/= pid) <$> getPlayerOrder
    defenders <- filter (`notElem` chosen) <$> (filterM (canDefend eid) =<< readyCharacters pid)
    enemyName <- cardVar "enemy" <$> getEnemy eid
    exhausts <- for defenders \d -> (d,) <$> exhaustToDefend d eid
    let exhaust' d = [Exhaust d | Just True <- [lookup d exhausts]]
    push
      $ if
        | undefendable -> UndefendedAttack pid eid
        | many ->
          questionLabelWith (ikeyWith "label.declareDefenders" enemyName) (Just $ toTarget eid) chosen pid
            $ [ targetLabel d
                  $ exhaust' d
                  <> [ defendingEffect d
                     , CheckWindows [mkAfter $ DeclaredAsDefender d eid]
                     , DeclareDefenders pid eid (chosen <> [d])
                     ]
              | d <- defenders
              ]
            <> [ Label
                   (ikey $ if null chosen then "label.takeAttackUndefended" else "label.doneDeclaringDefenders")
                   -- sentinels may defend only once the attacked player declares no defenders
                   [DeclareSentinels pid eid chosen (if null chosen then others else [])]
               ]
        | otherwise ->
          questionLabelWith (ikeyWith "label.declareDefender" enemyName) (Just $ toTarget eid) [] pid
            $ [targetLabel d (exhaust' d <> [DefendWith pid eid d]) | d <- defenders]
            <> [Label (ikey "label.takeAttackUndefended") [DeclareSentinels pid eid [] others]]
    pure g
  -- then each other player, in turn, may defend with a sentinel of theirs: one, unless
  -- the attacked player may declare several
  DeclareSentinels pid eid chosen [] -> do
    windows <- actionWindows
    push $ case chosen of
      [] -> UndefendedAttack pid eid
      _ ->
        Run
          $ windows
          <> [ResolveShadow eid pid chosen]
          <> windows
          <> [ DefendedByMany pid eid chosen
             , AttackDefended pid eid chosen
             , CheckWindows $ mkAfter (EnemyAttacking eid pid) : [mkAfter $ DefendedAttack d eid | d <- chosen]
             ]
          <> windows
    pure g
  DeclareSentinels pid eid chosen (other : rest) -> do
    many <- elem MayDeclareMultipleDefenders <$> getModifiers (PlayerTarget pid)
    sentinels <-
      filterM (canDefend eid)
        . filter (`notElem` chosen)
        =<< withKeyword Sentinel
        =<< readyCharacters other
    enemyName <- cardVar "enemy" <$> getEnemy eid
    exhausts <- for sentinels \d -> (d,) <$> exhaustToDefend d eid
    let exhaust' d = [Exhaust d | Just True <- [lookup d exhausts]]
        defend d
          | many =
              exhaust' d
                <> [ defendingEffect d
                   , CheckWindows [mkAfter $ DeclaredAsDefender d eid]
                   , DeclareSentinels pid eid (chosen <> [d]) (other : rest)
                   ]
          | otherwise = exhaust' d <> [DefendWith pid eid d]
    push
      $ if null sentinels
        then DeclareSentinels pid eid chosen rest
        else
          questionLabelWith (ikeyWith "label.defendWithSentinel" enemyName) (Just $ toTarget eid) chosen other
            $ [targetLabel d (defend d) | d <- sentinels]
            <> [Label (ikey "label.doNotDefend") [DeclareSentinels pid eid chosen rest]]
    pure g
  DefendedByMany pid eid (filter (inPlay g) -> []) -> g <$ push (UndefendedDamage pid eid)
  DefendedByMany pid eid (filter (inPlay g) -> defenders) -> do
    attack <- getEnemyAttack eid
    defense <- sum <$> traverse getDefense defenders
    choices <- for defenders \d -> targetLabel d <$> attackDamage pid eid d (attack - defense)
    push $ chooseOrRunOne pid choices
    pure g
  UndefendedAttack pid eid -> do
    windows <- actionWindows
    pushAll
      $ CheckWindows [mkAfter $ AttackUndefended pid eid]
      : windows
      <> [ResolveShadow eid pid []]
      <> windows
      <> [UndefendedDamage pid eid, CheckWindows [mkAfter $ EnemyAttacking eid pid]]
      <> windows
    pure g
  UndefendedDamage pid eid -> do
    attack <- (+) <$> getEnemyAttack eid <*> (sum . (\ms -> [n | AttackModifierIfUndefended n <- ms]) <$> getModifiers eid)
    -- the enemy's, or the player's, as guarding Gollum makes it
    redirected <-
      filter (inPlay g)
        . mapMaybe (\case UndefendedDamageTo t -> Just t; _ -> Nothing)
        <$> ((<>) <$> getModifiers eid <*> getModifiers (PlayerTarget pid))
    -- or a character the player may give it to instead of a hero, as White Tower Watchman
    watchmen <- filter (inPlay g) . (\ms -> [t | UndefendedDamageMayGoTo t <- ms]) <$> getModifiers (PlayerTarget pid)
    heroes <-
      if null redirected
        then (<> watchmen) . map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
        else pure []
    -- with several it may go to, as tied Battlegrounds at The Defense, the player chooses
    case ordNub redirected of
      [t] -> pushAll =<< attackDamage pid eid t attack
      ts@(_ : _) -> do
        options <- for ts \t -> targetLabel t <$> attackDamage pid eid t attack
        push $ questionLabelWith (ikey "label.undefendedDamage") (Just $ toTarget eid) [] pid options
      [] -> pure ()
    choices <- for heroes \h -> targetLabel h <$> attackDamage pid eid h attack
    unless (null heroes)
      $ push
      $ questionLabelWith (ikey "label.undefendedDamage") (Just $ toTarget eid) [] pid choices
    pure g
  PlayerAttacks pid -> do
    enemies <- engagedByCost pid
    loners <-
      filterM (fmap (elem CanAttackStagingAreaAlone) . getModifiers)
        =<< select (HeroControlledBy $ PlayerWithId pid)
    ready <- readyAttackers pid
    raiders <- stagingAttackers ready
    ranged <- withKeyword Ranged ready
    -- a player card letting characters attack into the staging area, as Forth Eorlingas!,
    -- does not reach an enemy immune to player card effects (ruling)
    staged <-
      (<>)
        <$> (if notNull raiders then filterM (fmap not . immuneToPlayerCards . toTarget) =<< select EnemyInStagingArea else pure [])
        <*> (if notNull ranged then filterM (fmap (elem AttackableInStagingAreaByRanged) . getModifiers) =<< select EnemyInStagingArea else pure [])
    -- enemies anyone may attack, wherever they are, as one in an Ithilien Pit
    open' <-
      filterM (fmap (elem AttackableByAnyCharacter) . getModifiers)
        =<< select (not_ $ EnemyEngagedWith $ PlayerWithId pid)
    pushAll $ ChooseAttackTarget pid (ordNub $ enemies <> staged <> open')
      : [AttackStagingAreaAlone pid (toTarget h) | h <- loners]
    pure g
  -- the player chooses which enemy to attack next, each once, among those someone could
  ChooseAttackTarget pid candidates -> do
    targets <-
      filterM (fmap notNull . mayAttack pid) [eid | eid <- candidates, Map.member eid g.entities.enemies]
    case targets of
      [] -> pure ()
      [eid] -> push $ DeclareAttackers pid eid
      _ ->
        push
          $ questionLabelWith (ikey "label.chooseEnemyToAttack") Nothing (map toTarget targets) pid
          $ [ targetLabel eid [DeclareAttackers pid eid, ChooseAttackTarget pid (filter (/= eid) targets)]
            | eid <- targets
            ]
          <> [Label (ikey "label.doNotAttack") []]
    pure g
  AttackStagingAreaAlone pid attacker -> do
    ready <- elem attacker <$> readyAttackers pid
    staged <- filterM (fmap (notElem CannotBeAttacked) . getModifiers) =<< select EnemyInStagingArea
    when (ready && notNull staged)
      $ push
      $ questionLabelWith (ikey "label.attackStagingAlone") (Just attacker) [] pid
      $ [ targetLabel
            eid
            [ AttackWith attacker
            , CreateEffect $ Effect GameSource attacker [AttackModifier 1] UntilEndOfAttack
            , ResolveAttack pid eid
            ]
        | eid <- staged
        ]
      <> [Label (ikey "label.doNotAttack") []]
    pure g
  DeclareAttackers pid eid -> do
    -- then other players' ranged characters may join an attack on an enemy engaged with them
    engaged <- selectAny (EnemyWithId eid <> EnemyEngagedWith (PlayerWithId pid))
    others <- if engaged then filter (/= pid) <$> getPlayerOrder else pure []
    attackers <- ownAttackers pid eid
    push
      $ if null attackers
        then DeclareRangedAttackers pid eid others
        else
          questionLabelWith (ikey "label.declareAttackers") Nothing [toTarget eid] pid
            $ [ targetLabel
                  a
                  [AttackWith a, CheckWindows [mkAfter $ DeclaredAsAttacker a eid], DeclareAttackers pid eid]
              | a <- attackers
              ]
            <> [Label (ikey "label.doneAttacking") [DeclareRangedAttackers pid eid others]]
    pure g
  -- the attack resolves, or its attackers stop, once no one else is left to ask; players
  -- may act once its attackers are declared and once its damage is dealt
  DeclareRangedAttackers pid eid others | null others || not (Map.member eid g.entities.enemies) -> do
    attackers <- selectCharacters AttackingHero AttackingAlly
    windows <-
      if null attackers || not (Map.member eid g.entities.enemies) then pure [] else actionWindows
    pushAll $ windows <> [ResolveAttack pid eid] <> windows
    pure g
  DeclareRangedAttackers pid eid (other : rest) -> do
    attacking <- selectCharacters AttackingHero AttackingAlly
    ranged <-
      filterM (canFight eid)
        . filter (`notElem` attacking)
        =<< withKeyword Ranged
        =<< readyAttackers other
    push
      $ if null ranged
        then DeclareRangedAttackers pid eid rest
        else
          questionLabelWith (ikey "label.joinAttackRanged") Nothing [toTarget eid] other
            $ [ targetLabel
                  a
                  [ AttackWith a
                  , CheckWindows [mkAfter $ DeclaredAsAttacker a eid]
                  , DeclareRangedAttackers pid eid (other : rest)
                  ]
              | a <- ranged
              ]
            <> [Label (ikey "label.doneAttacking") [DeclareRangedAttackers pid eid rest]]
    pure g
  -- the enemy is attacked, for its "when attacked" effects, before the damage is dealt
  ResolveAttack _ eid -> do
    attackers <- selectCharacters AttackingHero AttackingAlly
    pushAll $ [CheckWindows [mkWhen $ EnemyAttacked eid] | notNull attackers, Map.member eid g.entities.enemies] <> [DoStep 1 msg]
    pure g
  -- what the last attack on an enemy destroyed is forgotten as the next resolves
  DoStep 1 (ResolveAttack pid eid) -> do
    -- ranged characters of other players among them
    attackers <- selectCharacters AttackingHero AttackingAlly
    let unattached e = case e.placement of
          AttachedTo _ -> Nothing
          _ -> Just e
    case unattached =<< Map.lookup eid g.entities.enemies of
      -- the enemy left play, or became an attachment, before the attack resolved: the
      -- attackers just stop
      Nothing -> pushAll (map StopAttacking attackers)
      Just enemy' -> do
        mods <- getModifiers eid
        -- an enemy immune to ranged damage counts only the attackers of the player it is
        -- engaged with
        counted <-
          if ImmuneToRangedDamage `elem` mods
            then filterM (fmap (== Just (fromMaybe pid $ placementController enemy'.placement)) . characterController) attackers
            else pure attackers
        attack <- getAttackStrength counted eid (toCardDef enemy')
        alone <- case attackers of
          [a] -> elem IgnoresDefenseWhenAttackingAlone <$> getModifiers a
          _ -> pure False
        defense <- if alone then pure 0 else getEnemyDefense eid
        hitPoints <- getEnemyHitPoints eid
        traits <- (cdCardTraits (toCardDef enemy') <>) <$> gainedTraits (toTarget eid)
        -- the attack may place progress in place of its damage, as The Passage Out reads
        asProgress <- elem ProgressInsteadOfAttackDamage <$> getModifiers GameTarget
        -- what a lone attack deals beyond the enemy's remaining hit points, as Firefoot reads
        let excess = if asProgress then 0 else max 0 $ attack - defense - (hitPoints - enemy'.damage)
            damaging = CannotTakeCombatDamage `notElem` mods && not asProgress
        if null attackers
          then pure ()
          else
            pushAll
              =<< inLogBlock
                (PlayerAttackBlock pid eid attackers)
                ( map StopAttacking attackers
                    <> [DealDamage (PlayerAttackSource pid) (toTarget eid) (attack - defense) | damaging]
                    <> [ PlaceTokens (PlayerSource pid) (toTarget eid) Progress (attack - defense)
                       | asProgress
                       , attack > defense
                       , CannotTakeCombatDamage `notElem` mods
                       ]
                    <> [ CheckWindows [mkAfter $ AttackDealtDamage eid attackers (attack - defense)]
                       | attack > defense
                       , damaging
                       ]
                    <> [ AttackResolved eid (placementController enemy'.placement) attackers traits
                       ]
                    <> [ CheckWindows
                           $ [mkAfter $ AttackedEnemy a eid | a <- attackers]
                           <> [mkAfter $ AttackedAlone a eid excess | [a] <- [attackers]]
                       ]
                )
    pure $ g & attackDestroyedL %~ filter (not . isPlayerAttack . attackDestroyedBy)
  AttackResolved eid engaged attackers traits -> do
    -- only the attack's own damage destroying the enemy counts, not its leaving play some
    -- other way during the attack
    let byThisAttack d = d.target == EnemyTarget eid && isPlayerAttack (attackDestroyedBy d)
    pushWhen (any byThisAttack $ gameAttackDestroyed g) $ CheckWindows $ mkAfter (EnemyDestroyedByAttack eid attackers traits)
      : [mkAfter $ EngagedEnemyDestroyedByAttack pid attackers | Just pid <- [engaged]]
    pure $ g & effectsL %~ filter ((/= UntilEndOfAttack) . effectDuration)
  RedirectAttack eid target -> do
    let attacking = \case
          Window After (EnemyAttacking eid' _) -> eid' == eid
          _ -> False
    withQueue_ $ map \case
      AttackDamage eid' _ | eid' == eid -> AttackDamage eid target
      DefendedByMany _ eid' _ | eid' == eid -> AttackDamage eid target
      CheckWindows ws -> CheckWindows (filter (not . attacking) ws)
      m -> m
    pure g
  -- an attack turned on another enemy, as Small Target's, deals that enemy its damage
  AttackDamage eid target@(EnemyTarget other) -> do
    when (inPlay g target) do
      damage <- (-) <$> getEnemyAttack eid <*> getEnemyDefense other
      push $ DealDamage (EnemyAttackSource eid) target (max 0 damage)
    pure g
  -- a defender that left play before the damage leaves the attack undefended
  AttackDamage eid defender | not (inPlay g defender) -> do
    lead <- getLead
    engaged <- placementController . (.placement) <$> getEnemy eid
    push $ UndefendedDamage (fromMaybe lead engaged) eid
    pure g
  AttackDamage eid defender -> do
    attack <- getEnemyAttack eid
    defense <- getDefense defender
    engaged <- placementController . (.placement) <$> getEnemy eid
    lead <- getLead
    pushAll =<< attackDamage (fromMaybe lead engaged) eid defender (attack - defense)
    pure g
  ResolveShadow eid pid defenders -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy -> do
      -- each shadow card turns face up, and one with an effect resolves it; one already face
      -- up resolved in an earlier attack, as an enemy attacking again has, unless it was
      -- dealt face up
      let resolving card =
            RevealShadowCard eid (ecId card)
              : if cdShadowEffect (toCardDef card)
                then
                  [ CheckWindows $ mkWhen (ShadowEffectTriggered (ecId card) eid)
                      : [mkWhen $ ShadowEffectAgainst d | d <- defenders]
                  , ResolveShadowCard (ecId card) eid pid defenders
                  ]
                else []
          pending =
            [ card
            | card <- enemy.shadowCards
            , card.id `notElem` (toAttrs enemy).enemyShadowRevealed || card.id `elem` (toAttrs enemy).enemyShadowFaceUp
            ]
      case pending of
        [] -> pure ()
        [card] -> pushAll (resolving card)
        -- they turn over one at a time, in the order the first player decides (ruling 3.388);
        -- this resolution goes on with the rest once that one has resolved
        _ -> do
          lead <- getLead
          push
            $ questionLabelWith (ikey "label.chooseShadowCard") (Just $ toTarget eid) [] lead
            $ [targetLabel (CardIdTarget card.id) (resolving card <> [ResolveShadow eid pid defenders]) | card <- pending]
    pure g
  RevealShadowCard eid cid -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      when (any ((== cid) . ecId) enemy.shadowCards) $ send $ FlipCard cid
    pure g
  TurnShadowCardFaceUp eid cid -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      when (any ((== cid) . ecId) enemy.shadowCards) $ send $ FlipCard cid
    pure g
  ResolveShadowCard cid eid pid defenders -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      for_ (find ((== cid) . ecId) enemy.shadowCards) \card -> do
        push $ CheckWindows [mkAfter $ ShadowEffectResolved cid eid]
        resolveShadowEffect card (ResolveShadowEffect cid eid pid defenders)
    pure g
  -- a later step of a shadow effect, as "if this attack destroys a character" reads once
  -- the attack is over, which the shadow card, built again, resolves. The effect resolved
  -- as the card was revealed, so it still finishes once the card has been discarded, with
  -- its enemy leaving play after the damage (ALeP 3.043, 3.068); an attack its enemy left
  -- before the damage ended there (FAQ 2.54) and destroyed no one
  DoStep _ (ResolveShadowEffect cid eid _ _) -> do
    let dealt = maybe [] (.shadowCards) (Map.lookup eid g.entities.enemies)
    for_ (find ((== cid) . ecId) (dealt <> (toAttrs g.scenario).encounterDiscard)) \card -> resolveShadowEffect card msg
    pure g
  ShuffleIntoEncounterDeck (ObjectiveTarget oid) | Just o <- Map.lookup oid g.entities.objectives -> do
    deck <-
      shuffleM
        (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o) : (toAttrs g.scenario).encounterDeck)
    pure
      $ g
      & entitiesL
      . objectivesL
      %~ Map.delete oid
      & scenarioL
      %~ overAttrs (encounterDeckL .~ deck)
  PutShadowTreacheryIntoPlay host cid placement -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . ecId) hostEnemy.shadowCards) \card -> do
        thisTreachery <- overAttrs (\a -> a {treacheryPlacement = placement}) <$> newTreachery card
        pushAll [RemoveShadowCard host cid]
        putGame $ g & entitiesL . treacheriesL %~ insertEntity thisTreachery
    getGame
  ChooseAndExhaust source pid n | n > 0 -> do
    ready <-
      selectCharacters
        (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
        (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
    unless (null ready)
      $ push
      $ questionLabelWith
        (ikey "label.chooseAndExhaustCharacter")
        (sourceTarget source)
        []
        pid
        [targetLabel c [Exhaust c, ChooseAndExhaust source pid (n - 1)] | c <- ready]
    pure g
  DealDamageDivided source pid n assigned -> do
    characters <- withHitPointsLeft assigned =<< charactersControlledBy pid
    if n > 0 && notNull characters
      then
        push
          $ questionLabelWith (ikeyWith "label.dealDamageDivided" [intVar "n" n]) (sourceTarget source) [] pid
            [targetLabel c [DealDamageDivided source pid (n - 1) (c : assigned)] | c <- characters]
      else pushAll $ dealAssigned source assigned
    pure g
  ChooseAndExhaustHero source pid -> do
    ready <- map toTarget <$> select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
    unless (null ready)
      $ push
      $ questionLabelWith
        (ikey "label.chooseAndExhaustHero")
        (sourceTarget source)
        []
        pid
        [targetLabel h [Exhaust h] | h <- ready]
    pure g
  PayExhaustHeroCost pid matcher rest -> do
    heroes <- asPlayer pid (exhaustableHeroes matcher)
    -- the ability paid for finds the hero in its windows
    let paidWith h = \case
          UseCardAbility p s i ws -> UseCardAbility p s i (ws <> [mkAfter $ Exhausted $ toTarget h])
          m -> m
    push
      $ questionLabelWith (ikey "label.chooseAndExhaustHero") Nothing [] pid
        [targetLabel h (Exhaust (toTarget h) : map (paidWith h) rest) | h <- heroes]
    pure g
  PayExhaustCharacterCost pid hm am rest -> do
    characters <- asPlayer pid (exhaustableCharacters hm am)
    let paidWith c = \case
          UseCardAbility p s i ws -> UseCardAbility p s i (ws <> [mkAfter $ Exhausted c])
          m -> m
    push
      $ questionLabelWith (ikey "label.chooseAndExhaustCharacter") Nothing [] pid
        [targetLabel c (Exhaust c : map (paidWith c) rest) | c <- characters]
    pure g
  -- the group pays, the player picking whose heroes' resources, 1 at a time
  SpendGroupResources pid sphere n | n > 0 -> do
    heroes <- traverse getHero =<< select AnyHero
    payers <- filterM (heroPaysFor sphere) [h | h <- heroes, h.resources > 0]
    unless (null payers)
      $ push
      $ questionLabel (ikeyWith "label.payMore" [intVar "count" n]) pid
      $ [ targetLabel h [RemoveTokens (PlayerSource pid) (toTarget h) Resource 1, SpendGroupResources pid sphere (n - 1)]
        | h <- payers
        ]
    pure g
  DiscardGroupResources pid n | n > 0 -> do
    heroes <- filter ((> 0) . (.resources)) <$> (traverse getHero =<< select AnyHero)
    unless (null heroes)
      $ push
      $ questionLabel (ikeyWith "label.discardMore" [intVar "count" n]) pid
      $ [ targetLabel h [RemoveTokens (PlayerSource pid) (toTarget h) Resource 1, DiscardGroupResources pid (n - 1)]
        | h <- heroes
        ]
    pure g
  DamageAnotherCharacter source pid damaged -> do
    others <- filter (/= damaged) <$> charactersControlledBy pid
    push $ case others of
      [] -> Run []
      [c] -> DealDamage source c 1
      _ ->
        questionLabelWith (ikey "label.damageAnotherCharacter") (sourceTarget source) [] pid
          $ [targetLabel c [DealDamage source c 1] | c <- others]
    pure g
  ChooseAndDiscard source pid targets -> do
    unless (null targets)
      $ push
      $ questionLabelWith
        (ikey "label.chooseCardToDiscard")
        (sourceTarget source)
        []
        pid
        [targetLabel t [Discard t] | t <- targets]
    pure g
  BeginPhase phase -> pure $ g & phaseL .~ phase
  CommitCharacters pid -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> not_ CommittedHero)
    allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> not_ CommittedAlly)
    playerMods <- getModifiers (PlayerTarget pid)
    player <- getPlayer pid
    -- characters already committed have paid any toll for committing
    committed <-
      selectCharacters
        (HeroControlledBy (PlayerWithId pid) <> CommittedHero)
        (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
    let toll = [DiscardRandomFromHand pid 1 | DiscardRandomToCommit `elem` playerMods, null committed]
        barred = CannotCommitCharacters `elem` playerMods || (notNull toll && null player.hand)
        committable c = do
          mods <- getModifiers c
          free <- doesNotExhaustToCommit c
          pure $ CannotCommitToQuest `notElem` mods && (free || CannotExhaust `notElem` mods)
    characters <-
      if barred
        then pure []
        else
          filterM committable
            $ map toTarget heroes
            <> map toTarget allies
    quests <- getsGame (map toTarget . toList . entitiesQuests . gameEntities)
    -- a character's action may be worth more than its willpower, as Faramir's, so the
    -- player's actions are offered alongside committing it
    actions <- getActionAbilities pid
    mustCommitAll <- elem MustCommitAll <$> getModifiers GameTarget
    early <- filterM (\c -> andM [pure $ playsBeforeCommitting pid c, canPlay player c]) player.hand
    -- a player commits their characters at once, so responses to committing, as Aragorn's
    -- and Théodred's, wait until they are done and are then taken in any order
    -- asked even with no one left to commit, so the player moves on to staging
    push
      $ questionLabelWith (ikey "label.commitCharacters") Nothing quests pid
      $ [ targetLabel c (toll <> [CommitToQuest c, CommitCharacters pid])
        | c <- characters
        ]
      <> [AbilityLabel pid ab [] [CommitCharacters pid] | ab <- actions]
      <> [targetLabel (CardIdTarget c.id) [PlayCard pid c.id, CommitCharacters pid] | c <- early]
      <> [ Label (ikey "label.doneCommitting") [CheckWindows [mkAfter $ CommittedToQuest c | c <- committed] | notNull committed]
         | not mustCommitAll || null characters
         ]
    pure g
  Staging -> do
    mods <- getModifiers GameTarget
    let extra = sum [n | AdditionalStagingReveals n <- mods]
        fewer = sum [n | FewerStagingReveals n <- mods]
        players = length [() | player <- toList g.players, not player.eliminated]
        reveals = max 0 $ max 1 (players + extra) - fewer
    unless (SkipStagingStep `elem` mods)
      $ pushAll
      $ CheckWindows [mkAfter StagingStepBegins]
      : replicate reveals RevealEncounterCard
      <> [CheckWindows [mkWhen StagingStepEnds]]
    pure g
  QuestResolution -> do
    willpower <- getCommittedWillpower
    threat <- getStagingThreat
    players <- getPlayerOrder
    if
      | willpower > threat ->
          -- what questing successfully triggers resolves before the progress is placed
          -- (FAQ 1.24), as The Hunt Begins' Forced effect
          pushAll
            [ CreateEffect $ Effect GameSource GameTarget [QuestedSuccessfullyThisPhase] UntilEndOfPhase
            , CheckWindows [mkAfter QuestedSuccessfully]
            , PlaceProgressOnQuest (willpower - threat)
            ]
      | threat > willpower ->
          -- responses before the threat is raised may stop it, as Ever Onward's: see
          -- 'doesNotRaiseThreat'
          pushAll
            $ CheckWindows [mkWhen QuestedUnsuccessfully]
            : [RaiseThreat pid (threat - willpower) | pid <- players]
            <> [CheckWindows [mkAfter QuestedUnsuccessfully]]
      | otherwise -> pure ()
    pure $ g & lastCommittedWillpowerL ?~ willpower
  PlaceTimeCounters target -> do
    x <- getTimeX target
    pushWhen (x > 0) $ PlaceTokens GameSource target TimeCounter x
    pure g
  PlaceTokens _ target Progress n | n > 0, isQuestOrLocation target -> do
    push $ CheckWindows [mkAfter $ ProgressPlaced target n]
    pure g
  -- tokens for a character by its card, as one put into play is known before it has an id
  PlaceTokens source (CardIdTarget cid) token n
    | Just h <- find ((== cid) . heroCardId . toAttrs) (toList g.entities.heroes) ->
        g <$ push (PlaceTokens source (toTarget h) token n)
    | Just a <- find ((== cid) . allyCardId . toAttrs) (toList g.entities.allies) ->
        g <$ push (PlaceTokens source (toTarget a) token n)
  -- the resource phase's resources come from the game, not a card effect
  PlaceTokens source target@(HeroTarget _) Resource n
    | n > 0
    , source /= GameSource -> do
        mods <- getModifiers target
        let barred = CannotGainResourcesFromCardEffects `elem` mods || (isPlayerCardSource source && CannotGainResourcesFromPlayerCardEffects `elem` mods)
        unless barred $ push $ CheckWindows [mkAfter $ GainedResources target]
        pure g
  -- an Orc Guard that would return to the staging area goes to its owner's discard pile
  -- instead (FAQ errata, Out of the Dungeons)
  PlaceEnemy eid StagingArea
    | Just e <- Map.lookup eid g.entities.enemies
    , toCardCode e == toCardCode orcGuard -> do
        push $ Discard (toTarget eid)
        pure g
  PlaceEnemy eid StagingArea -> do
    push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget eid)]
    pure g
  PlaceProgressOnQuest n -> do
    actives <- select IsActiveLocation
    firsts <- if null actives then pure [] else takingProgressFirst
    if notNull firsts
      then g <$ (push =<< progressFirst GameSource n firsts \k -> [PlaceProgressOnQuest k | k > 0])
      else do
        -- what each active location still takes; one that takes only so much, as Perilous
        -- Swamp, is not explored by the rest, which is lost
        rooms <- for actives \lid -> do
          l <- getLocation lid
          remaining <- subtract l.progress <$> getLocationQuestPoints lid
          limits <- (\ms -> [k | ProgressLimit k <- ms]) <$> getModifiers lid
          pure (lid, remaining, max 0 $ foldr min remaining limits)
        -- the rest goes to the side quest quested against, else to the quest cards
        quests <- maybe getMainQuests (pure . pure) =<< getCurrentSideQuest
        let place lid k = [PlaceTokens GameSource (toTarget lid) Progress k | k > 0]
        case rooms of
          -- with two active locations the first player divides the progress between them
          -- (FAQ 1.34); none passes on to the quest until both are explored
          [(a, _, roomA), (b, _, roomB)] | n < roomA + roomB -> do
            lead <- getLead
            first <- getLocation a
            let lo = max 0 (n - roomB)
                hi = min n roomA
            push
              $ if lo == hi
                then Run (place a lo <> place b (n - lo))
                else
                  Ask lead
                    $ QuestionLabel (ikeyWith "label.divideProgress" (cardVar "location" first)) Nothing [toTarget a, toTarget b]
                    $ ChooseAmount lo [Label (ikeyWith "label.amount" [intVar "n" k]) (place a k <> place b (n - k)) | k <- [lo .. hi]]
          _ -> do
            let (left, onLocations) = mapAccumL (\m (lid, _, room) -> let k = min m room in (m - k, place lid k)) n rooms
                overflow = if all (\(_, remaining, room) -> room == remaining) rooms then left else 0
            -- what would go to the quest may be damage to an enemy instead, as Ride Them Down reads
            gameMods <- getModifiers GameTarget
            let instead = filter (inPlay g) [t | ProgressAsDamageTo t <- gameMods]
                canceledBy q = find ((== Just q) . sourceTarget) [s | QuestProgressCanceledBy s <- gameMods]
            pushAll
              $ concat onLocations
              <> case instead of
                t : _ -> [DealDamage GameSource t overflow | overflow > 0]
                [] ->
                  [ maybe (PlaceTokens GameSource q Progress overflow) (`ProgressCanceled` overflow) (canceledBy q)
                  | q <- quests
                  , overflow > 0
                  ]
        pure g
  PayForCard pid card fromHand windows
    | card.cardType == AttachmentType -> do
        -- a host that lowers the cost is chosen before paying
        hosts <- cardHosts (toCardCode card)
        discounts <- traverse (attachDiscount card) hosts
        if any (> 0) discounts
          then
            g
              <$ push
                ( questionLabelWith
                    (ikey "label.attachTo")
                    (Just $ CardIdTarget card.id)
                    hosts
                    pid
                    [targetLabel h [PayToAttach pid card fromHand windows h] | h <- hosts]
                )
          else payFor g pid card fromHand windows Nothing 0
    | otherwise -> payFor g pid card fromHand windows Nothing 0
  PayToAttach pid card fromHand windows host -> payFor g pid card fromHand windows (Just host) =<< attachDiscount card host
  PaidCost costId -> do
    -- a card with Doomed, printed or gained, raises each player's threat as it is played
    for_ (Map.lookup costId (gameActiveCosts g)) \ac -> do
      gained <- getModifiers (CardIdTarget ac.activeCostCard.id)
      runQueueT
        $ raiseThreatFromDoomed
        $ printedDoomed ac.activeCostCard
        + sum [n | GainKeyword (Doomed n) <- gained]
        + ac.activeCostDoomed
    pure $ g & activeCostsL %~ Map.delete costId
  PlayAtNoCost _ card -> do
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    pure g
  CreateEffect effect -> pure $ g & effectsL %~ (effect :)
  EndPhase phase -> do
    push $ CheckWindows [mkWhen $ PhaseEnds phase]
    -- at the end of each refresh phase each card with Time X loses 1 time counter, if able
    when (phase == RefreshPhase) do
      let es = g.entities
          cards = map toTarget (toList es.quests) <> map toTarget (toList es.locations) <> map toTarget (toList es.enemies)
      -- not a card Saruman holds out of play
      timed <- flip filterM cards \t -> andM [notOutOfPlay t, (&& timeCountersIn es t > 0) . (> 0) <$> getTimeX t]
      pushAll [RemoveTokens GameSource t TimeCounter 1 | t <- timed]
    when (phase == CombatPhase) do
      returning <-
        filterM
          (fmap (elem ReturnToTopOfEncounterDeckAfterCombat) . getModifiers)
          (toList g.entities.enemies)
      pushAll [ReturnToTopOfEncounterDeck (toTarget e) | e <- returning]
    returning <- flip filterM (toList g.entities.allies) \a ->
      elem ReturnToHandAtEndOfPhase <$> getModifiers (CardIdTarget $ allyCardId $ toAttrs a)
    shuffling <- flip filterM (toList g.entities.allies) \a -> elem ShuffleIntoDeckAtEndOfPhase <$> getModifiers a
    -- an ally under both, as Beorn put into play by Sneak Attack, goes where the first
    -- player chooses (FAQ 1.02)
    let both = map toTarget $ filter ((`elem` map toId shuffling) . toId) returning
    lead <- getLead
    pushAll
      $ [ReturnToHand t | t <- map toTarget returning, t `notElem` both]
      <> [ShuffleIntoOwnersDeck t | t <- map toTarget shuffling, t `notElem` both]
      <> [ questionLabelWith
             (ikey "label.endOfPhaseAlly")
             (Just t)
             []
             lead
             [ Label (ikey "label.returnAllyToHand") [ReturnToHand t]
             , Label (ikey "label.shuffleAllyIntoDeck") [ShuffleIntoOwnersDeck t]
             ]
         | t <- both
         ]
    -- separated, each area resolves the phase in turn before the next phase begins; the
    -- areas to join another do so at the beginning of the travel phase, and the token
    -- passes once every area has refreshed
    let following = maybe EndRound (\next -> Run [BeginPhase next, EndPhase next]) (nextPhase phase)
        (step, g') =
          eachAreaThen
            (\status -> [BeginPhase phase | status == AreaWaiting] <> [EndPhase phase])
            (if phase == RefreshPhase then passToken else id)
            (if phase == QuestPhase then Run [JoinAreas, FocusArea (maybe g.firstPlayer separationFirstPlayer $ gameSeparation g), following] else following)
            g
        passToken s = s {separationFirstPlayer = fromMaybe s.separationFirstPlayer $ headMay $ drop 1 $ seatsFrom s.separationFirstPlayer g.playerOrder}
    pushEnd step
    pure $ g' & effectsL %~ filter ((/= UntilEndOfPhase) . effectDuration)
  PlayAlly pid card -> do
    thisAlly <- newAlly pid card
    limited' <- elem OneAllyPerRound <$> getModifiers GameTarget
    pushWhen limited'
      $ CreateEffect
      $ Effect GameSource GameTarget [AllyPlayedThisRound] UntilEndOfRound
    push $ enteredPlayThisRound thisAlly
    push $ allyEntersPlay pid thisAlly
    pure
      $ g
      & entitiesL
      . alliesL
      %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  PutAllyIntoPlayUnder pid card -> do
    thisAlly <- newAlly (fromMaybe pid card.owner) card
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    push $ enteredPlayThisRound thisAlly
    push $ allyEntersPlay pid thisAlly
    pure
      $ g
      & entitiesL
      . alliesL
      %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  -- attached as one played is, but not played
  PutAttachmentIntoPlay pid card -> do
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    push $ PlayAttachment pid card
    pure g
  PutSideQuestIntoPlay pid card -> do
    sideQuest <- newSideQuest pid card
    pure $ g & entitiesL . questsL %~ insertEntity sideQuest
  PutHeroIntoPlay pid card -> do
    thisHero <- newHero pid card
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisHero]
    pure $ g & entitiesL . heroesL %~ insertEntity thisHero
  CheckWindows windows -> do
    abilities <- getWindowAbilities windows
    let forcedAbilities = filter isForcedAbility abilities
    lead <- getLead
    order <- getPlayerOrder
    forcedMsgs <- case forcedAbilities of
      [] -> pure []
      [ab] -> do
        controller <- fromMaybe lead <$> abilityController ab
        pure [UseAbility controller ab windows]
      abs' -> pure [Ask lead $ ChooseOneAtATime [AbilityLabel lead ab windows [] | ab <- abs']]
    pushAll $ forcedMsgs <> [ResponseWindow windows order [] 0]
    -- after an enemy's attack has resolved, what lasted for that attack ends
    let attackOver = any (\case Window After (EnemyAttacking _ _) -> True; _ -> False) windows
        -- an effect waiting for the next enemy in the staging area passes to it
        passed = case [eid | Window After (EntersStagingArea (EnemyTarget eid)) <- windows] of
          eid : _ -> map \e -> case [m | NextEnemyInStaging m <- effectModifiers e] of
            [] -> e
            ms -> e {effectTarget = EnemyTarget eid, effectModifiers = ms}
          [] -> id
    pure $ g & effectsL %~ passed . (if attackOver then filter ((/= UntilEndOfAttack) . effectDuration) else id)
  ResponseWindow windows order used passes
    | passes < length order
    , (pid : _) <- order -> do
        abilities <- getWindowAbilities windows
        available <-
          filterM
            (\ab -> (== Just pid) <$> abilityController ab)
            [ab | ab <- abilities, isResponseAbility ab, abilityKey ab `notElem` used]
        fromHand <- handResponses pid windows (ResponseWindow windows order used 0)
        -- a card responding from the discard pile stays there, so it is marked as used
        fromDiscard <- discardResponses pid windows used \cid -> ResponseWindow windows order ((CardIdSource cid, 0) : used) 0
        let next = ResponseWindow windows (drop 1 order <> [pid]) used (passes + 1)
        push
          $ if null available && null fromHand && null fromDiscard
            then next
            else
              playerWindow (ikey "label.useResponse") msg pid
                $ [ AbilityLabel pid ab windows [ResponseWindow windows order (abilityKey ab : used) 0]
                  | ab <- available
                  ]
                <> fromHand
                <> fromDiscard
                <> [Label (ikey "label.pass") [next]]
        pure g
  UseAbility pid ab windows -> do
    pushAll
      =<< inLogBlock
        (AbilityBlock pid ab.abilitySource (isForcedAbility ab))
        (payCost pid (abilityCost ab) [UseCardAbility pid ab.abilitySource ab.abilityIndex windows])
    pure g
  PlayEvent pid card _ -> do
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runEach msg
    pure g
  -- a later step of an event's effect, which the event, built again, resolves
  DoStep _ (PlayEvent pid card _) -> do
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runEach msg
    pure g
  -- the action of an ally in hand, which the ally, built from its card, resolves
  UseHandAction pid cid -> do
    player <- getPlayer pid
    for_ [pc | PlayerCard pc <- player.hand, pc.id == cid] \pc ->
      void $ runEach msg $ createAlly pc (AllyId UUID.nil) pid
    pure g
  RespondFromDiscard pid card _ -> do
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runEach msg
    pure g
  TravelActions -> do
    order <- getPlayerOrder
    pushAll $ map ActionWindow order
    pure g
  ReturnToHand target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll
      [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    pure $ g' & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . handL %~ (<> [toCard a])
  ShuffleIntoOwnersDeck target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll
      [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    deck <- shuffleM . (toCard a :) . (.deck) =<< getPlayer a.owner
    pure $ g' & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . deckL .~ deck
  ShuffleIntoOwnersDeck (AttachmentTarget aid) | Just a <- Map.lookup aid g.entities.attachments -> do
    let owner = (toAttrs a).owner
    deck <- shuffleM . (toCard a :) . (.deck) =<< getPlayer owner
    pure $ g & entitiesL . attachmentsL %~ Map.delete aid & playersL . ix owner . deckL .~ deck
  -- just before the attack's After windows, while what lasted for it still does
  AttackDefended _ eid defenders -> do
    undamaged <- filterM (fmap (notElem DamagedWhileDefending) . getModifiers) defenders
    pushWhen (notNull undamaged) $ CheckWindows [mkAfter $ DefendedWithoutDamage d eid | d <- undamaged]
    pure g
  GiveFromHand giver cid receiver -> do
    let given = [c | c <- maybe [] (.hand) (Map.lookup giver g.players), c.id == cid]
    pure $ g & playersL . ix giver . handL %~ filter ((/= cid) . (.id)) & playersL . ix receiver . handL %~ (<> given)
  ShuffleFromHandIntoOwnersDeck holder cid -> do
    let held = [c | c <- maybe [] (.hand) (Map.lookup holder g.players), c.id == cid]
        g' = g & playersL . ix holder . handL %~ filter ((/= cid) . (.id))
    foldM
      ( \acc c -> case c of
          PlayerCard pc | Just owner <- pc.owner -> do
            deck <- shuffleM (c : maybe [] (.deck) (Map.lookup owner acc.players))
            pure $ acc & playersL . ix owner . deckL .~ deck
          _ -> pure acc
      )
      g'
      held
  -- a hero shuffled into its owner's deck, as Lost and Alone has it, is put into play as it is drawn
  ShuffleIntoOwnersDeck target@(HeroTarget hid) | Just h <- Map.lookup hid g.entities.heroes -> do
    g' <- leftPlay g target
    pushAll
      $ [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      <> [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target]
    deck <- shuffleM . (toCard h :) . (.deck) =<< getPlayer h.owner
    pure $ g' & entitiesL . heroesL %~ Map.delete hid & playersL . ix h.owner . deckL .~ deck
  ReturnToTopOfEncounterDeck target -> returnToEncounterDeck (:) target g
  ReturnToBottomOfEncounterDeck target -> returnToEncounterDeck (\card deck -> deck <> [card]) target g
  RemoveProgressFromQuest source n -> do
    current <- getCurrentQuest
    let quests = [q | q <- toList g.entities.quests, Just (toTarget q) == current]
    pushAll [RemoveTokens source (toTarget q) Progress (min n q.progress) | q <- quests, q.progress > 0]
    pure g
  PutShadowEnemyIntoPlay host cid pid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        enemy <- newEnemy card
        pushAll [RemoveShadowCard host cid, EngageEnemy pid enemy.id]
        putGame $ g & entitiesL . enemiesL %~ insertEntity enemy
    getGame
  PutShadowEnemyIntoStaging host cid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        enemy <- newEnemy card
        pushAll [RemoveShadowCard host cid, CheckWindows [mkAfter $ EntersStagingArea (toTarget enemy)]]
        putGame $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
    getGame
  PutShadowCardIntoPlay host cid pid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        attacker <- newEnemy card
        pushAll
          [ RemoveShadowCard host cid
          , -- it never engages the player (Wolf Rider rulings)
            PlaceEnemy attacker.id (AttackingFromShadow pid)
          , CreateEffect
              $ Effect
                (toSource attacker)
                (toTarget attacker)
                [ReturnToTopOfEncounterDeckAfterCombat]
                UntilEndOfRound
          , DealShadowCard attacker.id
          , EnemyAttack pid attacker.id
          ]
        putGame $ g & entitiesL . enemiesL %~ insertEntity attacker
    getGame
  EngageOrcGuard pid card -> do
    guard' <- newEnemy (MkEncounterCard card.id (toCardCode orcGuard))
    push $ CheckWindows [mkAfter $ EnemyEngaged guard'.id pid]
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ insertEntity (overAttrs (\a -> a {enemyPlacement = EngagedWith pid}) guard')
      & scenarioL
      %~ overAttrs (orcGuardsL %~ Map.insert card.id card)
  -- an enemy put into play engaged enters play, as Haradrim Elite reads
  PutEnemyIntoPlayEngaged pid card -> do
    enemy <- newEnemy card
    pushAll [EngageEnemy pid enemy.id, CheckWindows [mkAfter $ EntersPlay $ toTarget enemy]]
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
  CreateEnemy eid card ->
    pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) (createEnemy card eid))
  CreateEnemyEngaged pid eid card -> do
    pushAll [EngageEnemy pid eid, CheckWindows [mkAfter $ EntersPlay $ EnemyTarget eid]]
    pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) (createEnemy card eid))
  PutIntoPlayAsActiveLocation card -> do
    thisLocation <- newLocation card
    pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = ActiveLocation}) thisLocation)
  PutLocationIntoPlay card placement -> do
    thisLocation <- newLocation card
    pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = placement}) thisLocation)
  PutObjectiveIntoPlayAttached card host -> do
    thisObjective <- newObjective card
    pure $ g & entitiesL . objectivesL %~ insertEntity (overAttrs (\a -> a {objectivePlacement = AttachedTo host}) thisObjective)
  RemoveFromGame (ObjectiveTarget oid) -> pure $ g & entitiesL . objectivesL %~ Map.delete oid
  -- a side quest removed from the game, as Stop the Executioners! removes itself
  RemoveFromGame target@(QuestTarget qid) | Just q <- Map.lookup qid g.entities.quests, isSideQuest q -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . questsL %~ Map.delete qid
  -- a hero leaves play for good, not for its owner's discard pile, its attachments
  -- discarded as they would be were it discarded
  RemoveFromGame target@(HeroTarget hid) | Map.member hid g.entities.heroes -> do
    g' <- leftPlay g target
    pushAll $ discardAttached g target
    pure $ g' & entitiesL . heroesL %~ Map.delete hid
  -- what is attached to it leaves play with it, as it would were it discarded
  RemoveFromGame target@(LocationTarget lid) -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . locationsL %~ Map.delete lid
  -- a location placed underneath a card leaves play, its tokens and attachments discarded
  PlaceUnderneath _ cards -> do
    let gone = [l | l <- toList g.entities.locations, (toCard l).id `elem` map (.id) cards]
        on' :: HasField "placement" a Placement => a -> Bool
        on' a = a.placement `elem` map (AttachedTo . toTarget) gone
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, on' a]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, on' t]
      <> [Discard (toTarget o) | o <- toList g.entities.objectives, on' o]
    pure $ g & entitiesL . locationsL %~ (\ls -> foldr (Map.delete . toId) ls gone)
  PutObjectiveIntoPlay card -> do
    thisObjective <- newObjective card
    pushWhen (Guarded `member` cdKeywords (toCardDef card)) $ GuardObjective (toId thisObjective)
    push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget thisObjective)]
    -- an objective's When Revealed, as Durin's Greaves attaching itself, after the window
    -- that can cancel it, as an enemy's
    when (cdWhenRevealed (toCardDef card))
      $ pushAll [CheckWindows [mkWhen $ Revealed (toTarget thisObjective)], WhenRevealed (toTarget thisObjective)]
    pure
      $ g
      & entitiesL
      . objectivesL
      %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  TakeControl aid pid -> do
    push $ CheckWindows [mkAfter $ ChangedControl (AllyTarget aid)]
    pure $ g & entitiesL . alliesL . ix aid %~ overAttrs (\a -> a {allyPlacement = InPlayArea pid})
  -- a side quest defeated goes to the victory display, and the quest stays where it is
  SetCurrentQuest t -> do
    sideQuests <- getSideQuests
    let chosen = [Effect GameSource t [CurrentQuest] UntilEndOfPhase | t `elem` sideQuests]
    pure $ g & effectsL %~ ((chosen <>) . filter ((CurrentQuest `notElem`) . effectModifiers))
  CompleteQuest qid | Just q <- Map.lookup qid g.entities.quests, isSideQuest q -> do
    pushAll $ discardAttachedToQuest g qid
    pure $ g & entitiesL . questsL %~ Map.delete qid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard q :))
  CompleteQuest qid -> do
    pushAll $ discardAttachedToQuest g qid <> [AdvanceQuest]
    -- a stage with victory points goes to the victory display, as Search for an Exit's
    let won = [toCard q | q <- toList (Map.lookup qid g.entities.quests), isJust (cdVictoryPoints $ toCardDef q)]
    pure $ g & entitiesL . questsL %~ Map.delete qid & scenarioL %~ overAttrs (victoryDisplayL %~ (won <>))
  -- a stage added to the victory display leaves play; what comes next is the card's to say
  AddToVictoryDisplay (QuestTarget qid) | Just q <- Map.lookup qid g.entities.quests ->
    pure $ g & entitiesL . questsL %~ Map.delete qid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard q :))
  ShuffleIntoQuestDeck card -> pure $ g & entitiesL . questsL %~ Map.filter ((/= card.id) . (.cardId) . toAttrs)
  PutOnBottomOfQuestDeck card -> pure $ g & entitiesL . questsL %~ Map.filter ((/= card.id) . (.cardId) . toAttrs)
  AddToVictoryDisplay target@(LocationTarget lid) | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      -- an objective it guarded, as Mugash on Edge of Fangorn, goes free to the staging area
      <> [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target]
    pure
      $ g
      & entitiesL
      . locationsL
      %~ Map.delete lid
      & scenarioL
      %~ overAttrs
        (victoryDisplayL %~ (EncounterCard (MkEncounterCard (locationCardId $ toAttrs l) (toCardCode l)) :))
  -- a card from the encounter deck, or a player's discard pile, as Out of the Wild takes them
  AddToVictoryDisplay (CardIdTarget cid) -> do
    let fromDeck = [EncounterCard c | c <- (toAttrs g.scenario).encounterDeck, c.id == cid]
        -- a destroyed enemy or an explored location, as None Return and Leave No Trace add
        fromEncounterDiscard = [EncounterCard c | c <- (toAttrs g.scenario).encounterDiscard, c.id == cid]
        fromDiscards = [c | p <- toList g.players, c <- p.discard, c.id == cid]
    pure
      $ g
      & scenarioL
      %~ overAttrs
        ( (encounterDeckL %~ filter ((/= cid) . (.id)))
            . (encounterDiscardL %~ filter ((/= cid) . (.id)))
            . (victoryDisplayL %~ ((fromDeck <> fromEncounterDiscard <> fromDiscards) <>))
        )
      & playersL
      . traverse
      . discardL
      %~ filter ((/= cid) . (.id))
  -- an enemy added to the victory display leaves play, what was attached to it and its
  -- shadow cards discarded
  AddToVictoryDisplay target@(EnemyTarget eid) | Just e <- Map.lookup eid g.entities.enemies -> do
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      <> [AddToEncounterDiscard e.shadowCards | notNull e.shadowCards]
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ Map.delete eid
      & scenarioL
      %~ overAttrs (victoryDisplayL %~ (EncounterCard (MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e)) :))
  AddToVictoryDisplay (AttachmentTarget aid)
    | Just a <- Map.lookup aid g.entities.attachments ->
        pure
          $ g
          & entitiesL
          . attachmentsL
          %~ Map.delete aid
          & scenarioL
          %~ overAttrs (victoryDisplayL %~ (toCard a :))
  -- an objective claimed into the victory display, as a Clue of The Steward's Fear
  AddToVictoryDisplay (ObjectiveTarget oid)
    | Just o <- Map.lookup oid g.entities.objectives ->
        pure $ g & entitiesL . objectivesL %~ Map.delete oid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard o :))
  MakeEscapeTest t -> do
    order <- getPlayerOrder
    ready <-
      filter (`notElem` t.escapeCommitted)
        <$> selectCharacters
          (HeroControlledBy (PlayerWithId t.escapePlayer) <> ReadyHero)
          (AllyControlledBy (PlayerWithId t.escapePlayer) <> ReadyAlly)
    push
      $ questionLabelWith
        (ikey "label.escapeTest")
        (sourceTarget t.escapeSource)
        t.escapeCommitted
        t.escapePlayer
      $ [ targetLabel c [Exhaust c, MakeEscapeTest t {escapeCommitted = t.escapeCommitted <> [c]}]
        | c <- ready
        ]
      <> [ Label (ikey "label.doneCommittingToEscape") $ case t.escapeParty of
             next : rest -> [MakeEscapeTest t {escapePlayer = next, escapeParty = rest}]
             -- the players may act once the characters are committed, before the cards
             -- are dealt, the test's only action window
             [] -> map OptionalActionWindow order <> [ResolveEscapeTest t]
         ]
    pure g
  ResolveEscapeTest t -> do
    (total, escape) <- escapeTestTotals t
    let (dealt, rest) = splitAt t.escapeCards (toAttrs g.scenario).encounterDeck
    pushAll $ if escape > total then t.escapeOnFail else t.escapeOnPass
    pure $ g & scenarioL %~ overAttrs ((encounterDeckL .~ rest) . (encounterDiscardL %~ (dealt <>)))
  -- a shadow card that is a location becomes the active location
  PutShadowLocationIntoPlay host cid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        thisLocation <- newLocation card
        active <- select IsActiveLocation
        pushAll $ RemoveShadowCard host cid : [PlaceLocation l StagingArea | l <- active]
        putGame
          $ g
          & entitiesL
          . locationsL
          %~ insertEntity (overAttrs (\a -> a {locationPlacement = ActiveLocation}) thisLocation)
    getGame
  ResetToStage card _ -> do
    pushAll [PutQuestIntoPlay card, Flip (toTarget card)]
    pure $ g & entitiesL . questsL %~ Map.filter isSideQuest
  PlayAttachment pid card -> do
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
      _ -> do
        hosts <- cardHosts (toCardCode card)
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
  -- a character can have at most 2 restricted attachments, claimed objectives among
  -- them: with a third, its controller discards one of them
  AttachCard aid host
    | Just a <- Map.lookup aid g.entities.attachments
    , isRestricted a ->
        g <$ checkRestricted g host
  ClaimObjective oid host
    | Just o <- Map.lookup oid g.entities.objectives
    , isRestricted o ->
        g <$ checkRestricted g host
  PlayAttachmentOn pid card host -> do
    attachment <- newAttachment pid card
    pushAll
      [AttachCard (toId attachment) host, CheckWindows [mkAfter $ EntersPlay $ toTarget attachment]]
    pure $ g & entitiesL . attachmentsL %~ insertEntity attachment
  SwapHero hid card | Just old <- Map.lookup hid g.entities.heroes -> do
    -- it trades places with the old hero, ready or exhausted as that one was (ruling)
    new <- overAttrs (\a -> a {heroExhausted = heroExhausted (toAttrs old)}) <$> newHero old.owner card
    let on' :: HasField "placement" a Placement => a -> Bool
        on' a = a.placement == AttachedTo (toTarget old)
        newHost = AttachedTo (toTarget new)
    putGame
      $ g
      & entitiesL
      . heroesL
      %~ (insertEntity new . Map.delete hid)
      -- the encounter's conditions and claimed objectives go with it
      & entitiesL
      . treacheriesL
      . traverse
      %~ (\t -> if on' t then overAttrs (\a -> a {treacheryPlacement = newHost}) t else t)
      & entitiesL
      . objectivesL
      . traverse
      %~ (\o -> if on' o then overAttrs (\a -> a {objectivePlacement = newHost}) o else o)
    moves <- for (filter on' $ toList g.entities.attachments) \a -> do
      hosts <- attachmentHosts (toAttrs a).attachTo
      pure
        $ if toTarget new `elem` hosts then AttachCard (toId a) (toTarget new) else Discard (toTarget a)
    pushAll $ moves <> [CheckWindows [mkAfter $ EntersPlay $ toTarget new]]
    getGame
  PutQuestIntoPlay card -> do
    thisQuest <- newQuest card
    pure $ g & entitiesL . questsL %~ insertEntity thisQuest
  -- the next card revealed is discarded unresolved if it is of the type named, as Minas
  -- Tirith Lampwright reads; either way the effect is spent
  (revealedCard -> Just (_, card))
    | Just e <- find (any isDiscardNextRevealed . effectModifiers) (gameEffects g) -> do
        let g' = g & effectsL %~ filter (/= e)
        if cdCardType (toCardDef card) `elem` [t | DiscardNextRevealed t <- effectModifiers e]
          then g' <$ push (AddToEncounterDiscard [card])
          else runGameMessage msg g'
  (revealedCard -> Just (engaging, card)) -> do
    let def = toCardDef card
    players <- getPlayerOrder
    let surge = [RevealEncounterCard | Surge `member` cdKeywords def]
        doomed =
          concat
            [ [RaiseThreat p n, CheckWindows [mkAfter $ ThreatRaisedByEncounter p n, mkAfter $ ThreatRaisedByDoomed p]]
            | Doomed n <- toList (cdKeywords def)
            , n > 0
            , p <- players
            ]
        -- the players as a group discard the resources from their heroes' pools (The
        -- Drúadan Forest rules)
        prowl = [DiscardGroupResources lead n | Prowl n <- toList (cdKeywords def), n > 0, lead <- take 1 players]
        whenRevealed t = if cdWhenRevealed def then AskToResolveWhenRevealed t card else WhenRevealed t
        -- Prowl, a keyword, resolves before the When Revealed effect (ALeP 3.094)
        revealing t =
          inLogBlock (RevealBlock card)
            $ [CheckWindows [mkAfter $ EntersStagingArea t] | cdCardType def `elem` [EnemyType, LocationType, EncounterSideQuestType], isNothing engaging]
            <> [CheckWindows [mkWhen $ Revealed t]] <> [WhileInPlay t prowl | notNull prowl] <> [whenRevealed t, WhileInPlay t doomed]
            <> [CheckWindows [mkAfter $ Revealed t]]
    case cdCardType def of
      -- an enemy revealed to be put into play engaged never enters the staging area
      EnemyType | Just pid <- engaging -> do
        enemy <- newEnemy card
        pushAll . ([EngageEnemy pid (toId enemy), CheckWindows [mkAfter $ EntersPlay $ toTarget enemy]] <>) . (<> [WhileInPlay (toTarget enemy) [ResolveSurge (toTarget enemy) card]])
          =<< revealing (toTarget enemy)
        pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = Unplaced}) enemy)
      EnemyType -> do
        enemy <- newEnemy card
        pushAll . (<> [WhileInPlay (toTarget enemy) [AmbushEngagementCheck (toId enemy), ResolveSurge (toTarget enemy) card]])
          =<< revealing (toTarget enemy)
        pure
          $ g
          & entitiesL
          . enemiesL
          %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        pushAll . (<> [ResolveSurge (toTarget location) card]) =<< revealing (toTarget location)
        pure
          $ g
          & entitiesL
          . locationsL
          %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      TreacheryType -> do
        treachery <- newTreachery card
        pushAll . (<> [FinishRevelation (toId treachery), WhileInPlay (toTarget treachery) surge])
          =<< revealing (toTarget treachery)
        pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
      t | t `elem` [ObjectiveType, ObjectiveAllyType] -> do
        pushAll . (<> surge) =<< inLogBlock (RevealBlock card) [PutObjectiveIntoPlay card]
        pure g
      -- an encounter side quest goes to the staging area, where the players may quest
      -- against it
      EncounterSideQuestType -> do
        sideQuest <- newQuest card
        pushAll . (<> [ResolveSurge (toTarget sideQuest) card]) =<< revealing (toTarget sideQuest)
        pure $ g & entitiesL . questsL %~ insertEntity sideQuest
      _ -> do
        pushAll =<< inLogBlock (RevealBlock card) []
        pure $ g & scenarioL %~ overAttrs (encounterDiscardL %~ (card :))
  ResolveWhenRevealedOf card -> do
    let def = toCardDef card
        whenRevealed t = inLogBlock (WhenRevealedBlock card) [if cdWhenRevealed def then AskToResolveWhenRevealed t card else WhenRevealed t]
    case cdCardType def of
      TreacheryType -> do
        treachery <- newTreachery card
        pushAll . (<> [FinishRevelation (toId treachery)]) =<< whenRevealed (toTarget treachery)
        pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
      -- an enemy or location goes to the staging area, as each card resolving one this
      -- way reads (The North Stair, Lying in Wait's locations); one wanting it elsewhere
      -- would move it after
      EnemyType -> do
        enemy <- newEnemy card
        pushAll =<< whenRevealed (toTarget enemy)
        pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        pushAll =<< whenRevealed (toTarget location)
        pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      _ -> pure $ g & scenarioL %~ overAttrs (encounterDiscardL %~ (card :))
  -- an enemy destroyed as it was revealed resolves no more of its revelation (FAQ)
  AskToResolveWhenRevealed t _ | not (inPlay g t) -> pure g
  AskToResolveWhenRevealed t card -> do
    lead <- getLead
    push
      $ questionLabelWith
        (ikeyWith "label.resolveWhenRevealed" (cardVar "card" card))
        (Just t)
        []
        lead
        [targetLabel t [WhenRevealed t]]
    pure g
  ResolveSurge t card -> do
    surges <- hasKeyword Surge t (toCardDef card)
    pushWhen surges RevealEncounterCard
    pure g
  FinishRevelation tid -> do
    -- a treachery that did not attach itself to something is discarded once it resolves
    for_ (Map.lookup tid g.entities.treacheries) \treachery ->
      when (treachery.placement == Unplaced) $ push (Discard $ toTarget treachery)
    -- its effects have resolved or been canceled, so it guards nothing any more
    let cid = (.cardId) . toAttrs <$> Map.lookup tid g.entities.treacheries
        guarding o = isJust cid && (toAttrs o).objectiveGuard == cid
    pure
      $ g
      & entitiesL
      . objectivesL
      . traverse
      %~ (\o -> if guarding o then overAttrs (\a -> a {objectiveGuard = Nothing}) o else o)
  PutObjectiveAllyIntoPlay oid pid | Just o <- Map.lookup oid g.entities.objectives -> do
    let thisAlly =
          -- the damage it took in the staging area stays on it, as Celador's
          overAttrs (\a -> a {allyPlacement = InPlayArea pid, allyTokens = addTokens Damage (countTokens Damage $ objectiveTokens $ toAttrs o) mempty})
            $ lookupAlly (toCardCode o) (toCard o).id (AllyId $ unObjectiveId oid) pid
    push $ enteredPlayThisRound thisAlly
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . objectivesL %~ Map.delete oid & entitiesL . alliesL %~ insertEntity thisAlly
  ShuffleIntoEncounterDeck target@(LocationTarget lid) | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
    deck <-
      shuffleM
        (MkEncounterCard (locationCardId $ toAttrs l) (toCardCode l) : (toAttrs g.scenario).encounterDeck)
    pure
      $ g
      & entitiesL
      . locationsL
      %~ Map.delete lid
      & scenarioL
      %~ overAttrs (encounterDeckL .~ deck)
  ShuffleIntoEncounterDeck (TreacheryTarget tid) | Just t <- Map.lookup tid g.entities.treacheries -> do
    deck <-
      shuffleM
        (MkEncounterCard (treacheryCardId $ toAttrs t) (toCardCode t) : (toAttrs g.scenario).encounterDeck)
    pure
      $ g
      & entitiesL
      . treacheriesL
      %~ Map.delete tid
      & scenarioL
      %~ overAttrs (encounterDeckL .~ deck)
  DrawTreacheryIntoHand pid tid | Just t <- Map.lookup tid g.entities.treacheries -> do
    let card = EncounterCard $ MkEncounterCard (treacheryCardId $ toAttrs t) (toCardCode t)
    pure $ g & entitiesL . treacheriesL %~ Map.delete tid & playersL . ix pid . handL %~ (<> [card])
  -- an enemy leaves play for the encounter deck, its attachments and shadow cards discarded
  ShuffleIntoEncounterDeck target@(EnemyTarget eid) | Just e <- Map.lookup eid g.entities.enemies -> do
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      <> [AddToEncounterDiscard e.shadowCards | notNull e.shadowCards]
    deck <- shuffleM (MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e) : (toAttrs g.scenario).encounterDeck)
    pure $ g & entitiesL . enemiesL %~ Map.delete eid & scenarioL %~ overAttrs (encounterDeckL .~ deck)
  AddToStaging card | FirstPlayerGainsControl `member` cdKeywords (toCardDef card) -> do
    thisObjective <- newObjective card
    lead <- getLead
    push $ PutObjectiveAllyIntoPlay (toId thisObjective) lead
    pure
      $ g
      & entitiesL
      . objectivesL
      %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  AddToStaging card -> case cdCardType (toCardDef card) of
    EnemyType -> do
      thisEnemy <- newEnemy card
      pushAll [CheckWindows [mkAfter $ EntersStagingArea (toTarget thisEnemy)], AmbushEngagementCheck (toId thisEnemy)]
      pure
        $ g
        & entitiesL
        . enemiesL
        %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) thisEnemy)
    LocationType -> do
      thisLocation <- newLocation card
      push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget thisLocation)]
      pure
        $ g
        & entitiesL
        . locationsL
        %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) thisLocation)
    EncounterSideQuestType -> do
      sideQuest <- newQuest card
      push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget sideQuest)]
      pure $ g & entitiesL . questsL %~ insertEntity sideQuest
    _ -> do
      thisObjective <- newObjective card
      push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget thisObjective)]
      pure
        $ g
        & entitiesL
        . objectivesL
        %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  _ -> pure g

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
          [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
  case target of
    EnemyTarget eid | Just e <- Map.lookup eid g.entities.enemies -> do
      withAttached
      pushAll $ discardShadowsOf g target
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

eliminationThreat :: Int
eliminationThreat = 50

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
    toTop <- elem LeavesPlayToTopOfEncounterDeck <$> getModifiers target
    g <$ push (if toTop then ReturnToTopOfEncounterDeck target else PlaceObjective oid StagingArea)
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
        card -> pure $ addToDiscard card removed
  addToDiscard card = case card of
    PlayerCard pc | Just owner <- pcOwner pc -> playersL . ix owner . discardL %~ (card :)
    EncounterCard ec -> scenarioL %~ overAttrs (encounterDiscardL %~ (ec :))
    _ -> id

-- | Attachments, and encounter cards attached as conditions, leave play with their host,
-- and an enemy's shadow cards are discarded with it (ALeP 3.068).
discardAttached :: Game -> Target -> [Message]
discardAttached g target =
  [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
    <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
    <> [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target]
    <> [Discard (toTarget e) | e <- toList g.entities.enemies, e.placement == AttachedTo target]
    <> discardShadowsOf g target

-- | The shadow cards dealt to an enemy leaving play, discarded with it.
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

-- | The character is defending the attack being resolved.
defendingEffect :: Target -> Message
defendingEffect d = CreateEffect $ Effect GameSource d [Defending] UntilEndOfAttack

gainedTraits :: Target -> GameT (Set Trait)
gainedTraits target = setFromList . mapMaybe (\case GainTrait t -> Just t; _ -> Nothing) <$> getModifiers target

-- | A character's traits, printed and gained.
characterTraits :: Target -> GameT (Set Trait)
characterTraits target = (<>) . cdCardTraits <$> characterCardDef target <*> gainedTraits target

-- | The characters with the keyword, printed or gained.
withKeyword :: Keyword -> [Target] -> GameT [Target]
withKeyword k = filterM (\t -> hasKeyword k t =<< characterCardDef t)

-- | Whether the card has the restricted keyword.
isRestricted :: HasCardDef a => a -> Bool
isRestricted = member Restricted . cdKeywords . toCardDef

-- | With a third restricted card on the character, its controller discards one of them.
checkRestricted :: Game -> Target -> GameT ()
checkRestricted g host = do
  let restrictedOn =
        [toTarget x | x <- toList g.entities.attachments, x.placement == AttachedTo host, isRestricted x]
          <> [toTarget o | o <- toList g.entities.objectives, o.placement == AttachedTo host, isRestricted o]
  controller <- characterController host
  for_ controller \pid ->
    when (length restrictedOn > 2)
      $ push
      $ questionLabelWith
        (ikey "label.discardRestricted")
        (Just host)
        []
        pid
        [targetLabel x [Discard x] | x <- restrictedOn]

-- | Mark the ally as having entered play this round.
enteredPlayThisRound :: Ally -> Message
enteredPlayThisRound a = CreateEffect $ Effect GameSource (toTarget a) [EnteredPlayThisRound] UntilEndOfRound

-- | The windows for an ally entering play under the player.
allyEntersPlay :: PlayerId -> Ally -> Message
allyEntersPlay pid a =
  CheckWindows
    [ mkAfter $ EntersPlay $ toTarget a
    , mkAfter $ AllyEnteredPlay (toTarget a) pid (cdCardTraits $ toCardDef a)
    ]

{- | Cards in a player's hand that can be played or put into play in response to the
windows: response events, and allies like Brok Ironfist.
-}
handResponses :: PlayerId -> [Window] -> Message -> GameT [UI Message]
handResponses pid windows continue = do
  player <- getPlayer pid
  let matches = maybe (pure False) (\w -> anyM (windowMatchesM w) windows)
  fmap catMaybes $ for player.hand \case
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
        ok <-
          andM [matches (allyHandWindow $ toAttrs $ createAlly pc (AllyId UUID.nil) pid), passesUnique card]
        pure $ guard ok $> targetLabel (CardIdTarget pc.id) [PutAllyIntoPlay pid pc.id, continue]
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
    Criteria cs -> any before cs
    _ -> False

{- | Cards in a player's discard pile that respond to the windows from there: allies like
Ered Luin Miner, put into play, and events like Hidden Cache, which resolve it. The
continuation gets the card id, to mark the card as used.
-}
discardResponses
  :: PlayerId -> [Window] -> [(Source, Int)] -> (CardId -> Message) -> GameT [UI Message]
discardResponses pid windows used continue = do
  player <- getPlayer pid
  let matches = maybe False (\w -> any (windowMatches w) windows)
  fmap catMaybes $ for player.discard \case
    PlayerCard pc | (CardIdSource pc.id, 0) `notElem` used -> case cdCardType (toCardDef pc) of
      AllyType
        | matches (allyDiscardWindow $ toAttrs $ createAlly pc (AllyId UUID.nil) pid) -> do
            ok <- passesUnique (PlayerCard pc)
            pure
              $ guard ok
              $> targetLabel
                (CardIdTarget pc.id)
                [RemoveFromDiscard pid pc.id, PutAllyIntoPlayUnder pid pc, continue pc.id]
      EventType
        | Just e <- lookupEvent (toCardCode pc) pc.id pid
        , matches (toAttrs e).eventDiscardWindow ->
            pure $ Just $ targetLabel (CardIdTarget pc.id) [RespondFromDiscard pid pc windows, continue pc.id]
      _ -> pure Nothing
    _ -> pure Nothing

-- | The next player clockwise from the given one.
seatAfter :: [PlayerId] -> PlayerId -> PlayerId
seatAfter order pid = case dropWhile (/= pid) order of
  (_ : next : _) -> next
  _ -> fromMaybe pid (headMay order)

{- | The characters that may take another point of the damage being assigned: damage
assigned among characters cannot be assigned past a character's remaining hit points
(ALeP 3.088, 3.132, 3.263).
-}
withHitPointsLeft :: [Target] -> [Target] -> GameT [Target]
withHitPointsLeft assigned = filterM \c -> do
  left <- (-) <$> getHitPoints c <*> getDamage c
  pure $ left > length (filter (== c) assigned)

-- | The damage assigned among characters, a point at a time, dealt once all is assigned.
dealAssigned :: Source -> [Target] -> [Message]
dealAssigned source assigned = [DealDamage source c k | (c, k) <- Map.toList (Map.fromListWith (+) [(c, 1) | c <- assigned])]

-- | Enemies engaged with a player, highest engagement cost first.
engagedByCost :: PlayerId -> GameT [EnemyId]
engagedByCost pid = do
  enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
  costs <- traverse getEngagementCost enemies
  pure $ map fst $ sortOn (Down . snd) $ zip enemies costs

-- | Whether the enemy is engaged with each player from the staging area, as Durin's Bane.
engagedWithEach :: EnemyId -> GameT Bool
engagedWithEach eid = any (\case EngagedWithEachPlayer _ -> True; _ -> False) <$> getModifiers eid

-- | The characters that may attack an enemy in the staging area.
stagingAttackers :: [Target] -> GameT [Target]
stagingAttackers = filterM (fmap (elem CanAttackStagingArea) . getModifiers)

-- | The player's characters that may attack or defend.
readyCharacters :: PlayerId -> GameT [Target]
readyCharacters pid = do
  heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
  allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
  filterM (fmap (notElem CannotAttackOrDefend) . getModifiers)
    $ map toTarget heroes
    <> map toTarget allies

-- | The player's characters that may attack: one that cannot exhaust only if it attacks
-- without exhausting.
readyAttackers :: PlayerId -> GameT [Target]
readyAttackers pid = do
  -- a player who cannot declare attacks, as Hobbit-sense reads
  barred <- elem CannotAttack <$> getModifiers (PlayerTarget pid)
  let free ms = CannotExhaust `notElem` ms || DoesNotExhaustToAttack `elem` ms
  if barred then pure [] else filterM (fmap (\ms -> CannotAttack `notElem` ms && free ms) . getModifiers) =<< readyCharacters pid

{- | Shadow cards are not in play, so the card's own module resolves the effect
through a temporary instance of it.
-}
resolveShadowEffect :: EncounterCard -> Message -> GameT ()
resolveShadowEffect card msg = case cdCardType (toCardDef card) of
  EnemyType -> void . runEach msg . createEnemy card =<< getRandom
  LocationType -> void . runEach msg . createLocation card =<< getRandom
  TreacheryType -> void . runEach msg . createTreachery card =<< getRandom
  t | t `elem` [ObjectiveType, ObjectiveAllyType] -> void . runEach msg . createObjective card =<< getRandom
  _ -> pure ()

{- | The damage an enemy's attack deals to a character, and, for an enemy whose excess
damage counts against the player, the threat it raises.
-}
attackDamage :: PlayerId -> EnemyId -> Target -> Int -> GameT [Message]
attackDamage pid eid target n = do
  mods <- getModifiers eid
  let excessToThreat = ExcessDamageToThreat `elem` mods
      excessToOthers = ExcessDamageToOtherCharacters `elem` mods
      excessToLocation = ExcessDamageToActiveLocation `elem` mods
  excess <-
    if excessToThreat || excessToOthers || excessToLocation
      then (\remaining -> max 0 (n - remaining)) <$> ((-) <$> getHitPoints target <*> getDamage target)
      else pure 0
  active <- select IsActiveLocation
  pure $ DealDamage (EnemyAttackSource eid) target n
    : concat [[RaiseThreat pid excess, CheckWindows [mkAfter $ ThreatRaisedByEncounter pid excess]] | excessToThreat, excess > 0]
    <> replicate (if excessToOthers then excess else 0) (DamageAnotherCharacter (EnemyAttackSource eid) pid target)
    <> concat [[PlaceTokens (EnemyAttackSource eid) (LocationTarget l) Damage excess, CheckDefeated (EnemyAttackSource eid) (LocationTarget l)] | excessToLocation, excess > 0, l <- active]

-- | The player's characters that may yet be declared as attackers against the enemy.
ownAttackers :: PlayerId -> EnemyId -> GameT [Target]
ownAttackers pid eid = do
  -- only those allowed to may attack an enemy in the staging area, unless anyone may
  -- one engaged with the player from the staging area, as Durin's Bane, is attacked as engaged
  staged <- selectAny (EnemyWithId eid <> EnemyInStagingArea <> not_ (EnemyEngagedWith $ PlayerWithId pid))
  mods <- getModifiers eid
  let open' = AttackableByAnyCharacter `elem` mods
      fromStaging cs = (<>) <$> stagingAttackers cs <*> if AttackableInStagingAreaByRanged `elem` mods then withKeyword Ranged cs else pure []
  -- one who does not exhaust to attack is still declared only once
  attacking <- selectCharacters AttackingHero AttackingAlly
  filter (`notElem` attacking)
    . ordNub
    <$> ( filterM (canFight eid)
            =<< (if staged && not open' then fromStaging else pure)
            =<< readyAttackers pid
        )

{- | Who may attack the enemy for the player: their own characters, then the ranged
characters of other players, when it is engaged with them.
-}
mayAttack :: PlayerId -> EnemyId -> GameT [Target]
mayAttack pid eid = do
  blocked <- elem CannotBeAttacked <$> getModifiers eid
  if blocked then pure [] else mayAttack' pid eid

mayAttack' :: PlayerId -> EnemyId -> GameT [Target]
mayAttack' pid eid = do
  own <- ownAttackers pid eid
  engaged <- selectAny (EnemyWithId eid <> EnemyEngagedWith (PlayerWithId pid))
  others <- if engaged then filter (/= pid) <$> getPlayerOrder else pure []
  ranged <- filterM (canFight eid) =<< withKeyword Ranged . concat =<< traverse readyAttackers others
  pure $ own <> ranged

-- | An action window for each player, in turn order, asked only of those with an action.
actionWindows :: GameT [Message]
actionWindows = map OptionalActionWindow <$> getPlayerOrder

-- | The enemy a step of an enemy's attack, or of an attack on one, is about.
attackingEnemy :: Message -> Maybe EnemyId
attackingEnemy = \case
  EnemyAttack _ eid -> Just eid
  DefendWith _ eid _ -> Just eid
  DeclareDefenders _ eid _ -> Just eid
  DeclareSentinels _ eid _ _ -> Just eid
  DefendedByMany _ eid _ -> Just eid
  UndefendedAttack _ eid -> Just eid
  UndefendedDamage _ eid -> Just eid
  AttackDamage eid _ -> Just eid
  ResolveShadow eid _ _ -> Just eid
  DeclareAttackers _ eid -> Just eid
  _ -> Nothing

{- | Whether the character may attack or defend against the enemy: some enemies only
characters of a trait, or with a keyword, can.
-}
canFight :: EnemyId -> Target -> GameT Bool
canFight eid c = do
  mods <- getModifiers eid
  def <- characterCardDef c
  andM
    [ allM (\(t, k) -> orM [hasTrait t c def, hasKeyword k c def]) [(t, k) | OnlyFoughtBy t k <- mods]
    , allM (\k -> hasKeyword k c def) [k | OnlyFoughtByKeyword k <- mods]
    ]

-- | Whether the character may defend against the enemy's attack: one that cannot exhaust
-- only if it defends without exhausting.
canDefend :: EnemyId -> Target -> GameT Bool
canDefend eid c = do
  noAllies <- elem CannotBeDefendedByAllies <$> getModifiers eid
  stuck <- elem CannotExhaust <$> getModifiers c
  exhausts <- if stuck then exhaustToDefend c eid else pure False
  let isAlly = case c of
        AllyTarget _ -> True
        _ -> False
  if (noAllies && isAlly) || exhausts then pure False else canFight eid c

-- | Whether declaring the character as a defender against the enemy exhausts it.
exhaustToDefend :: Target -> EnemyId -> GameT Bool
exhaustToDefend d eid = do
  mods <- getModifiers d
  enemy <- getEnemy eid
  if DoesNotExhaustToDefend `elem` mods
    then pure False
    else
      not
        <$> anyM
          (\t -> hasTrait t (toTarget eid) (toCardDef enemy))
          [t | DoesNotExhaustToDefendAgainst t <- mods]

-- | Whether a character is still in play.
-- | The encounter cards attached to the stage as conditions, as Old Hatreds, leave play with it.
discardAttachedToQuest :: Game -> QuestId -> [Message]
discardAttachedToQuest g qid = [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo (QuestTarget qid)]

-- | Whether the source is a player's attack on an enemy, its damage.
isPlayerAttack :: Source -> Bool
isPlayerAttack = \case
  PlayerAttackSource _ -> True
  _ -> False

inPlay :: Game -> Target -> Bool
inPlay g = \case
  HeroTarget hid -> Map.member hid g.entities.heroes
  AllyTarget aid -> Map.member aid g.entities.allies
  EnemyTarget eid -> Map.member eid g.entities.enemies
  LocationTarget lid -> Map.member lid g.entities.locations
  _ -> True

-- | Put the card's cost up to be paid, less what the host it was chosen for takes off.
payFor :: Game -> PlayerId -> Card -> Bool -> [Window] -> Maybe Target -> Int -> GameT Game
payFor g pid card fromHand windows host discount = do
  costId <- getRandom
  -- the card has left its owner's hand and is not yet being paid for, so the modifiers it
  -- gives itself are asked for here
  MonoidalMap own <- execWriterT $ handCardModifiers pid card
  let ownMods = Map.findWithDefault [] (CardIdTarget card.id) own
      ownExtra = sum [k | Modifier _ (CostModifier k) <- ownMods]
  extra <- (+ ownExtra) . subtract discount <$> costModifier pid card
  -- a reduction of the next matching card's cost is spent on this one, and the Doomed
  -- that comes with it, as Gríma's
  let spent e =
        effectTarget e
          `elem` [PlayerTarget pid, GameTarget]
          && or
            ( [cardMatches m card | ReduceNextCostOf m _ <- effectModifiers e]
                <> [cardMatches m card | LowerNextCostOf m _ <- effectModifiers e]
                <> [fromHand | NextCardGainsDoomed _ <- effectModifiers e]
            )
      spentEffects = filter spent (gameEffects g)
  let activeCost =
        ActiveCost
          { activeCostId = costId
          , activeCostCost = raiseCost extra (cardCost card)
          , activeCostCard = card
          , activeCostPlayer = pid
          , activeCostWindows = windows
          , activeCostHost = host
          , activeCostFromHand = fromHand
          , activeCostDoomed = sum [n | e <- spentEffects, NextCardGainsDoomed n <- effectModifiers e]
          }
  -- resources on allies, as Radagast's, may pay for the cards they are for
  payers <- allyPayersFor pid card
  let n = staticCost activeCost.activeCostCost
      pay left =
        if null payers || left == 0
          then PayCost costId
          else
            questionLabel (ikey "label.payFromAlly") pid
              $ [ targetLabel
                    aid
                    [ RemoveTokens (PlayerSource pid) (toTarget aid) Resource k
                    , ReduceActiveCost costId k
                    , PayCost costId
                    ]
                | (aid, r) <- payers
                , k <- [1 .. min r left]
                ]
              <> [Label (ikey "label.payFromHeroes") [PayCost costId]]
  -- a card that may cost less for cards discarded from the deck, as Erebor Guard, asks
  -- first; it is offered only with the cards to discard
  deckSize <- length . (.deck) <$> getPlayer pid
  push $ case [k | Modifier _ (ReducedByDiscardingFromDeck k) <- ownMods, k <= deckSize] of
    k : _ ->
      questionLabelWith
        (ikeyWith "label.discardToReduceCost" [intVar "count" k])
        (Just $ CardIdTarget card.id)
        []
        pid
        [ Label
            (ikey "label.discardAndReduce")
            [DiscardTopOfDeck pid k, ReduceActiveCost costId k, pay (max 0 $ n - k)]
        , Label (ikey "label.payFullCost") [pay n]
        ]
    [] -> pay n
  -- a card its player may give Doomed as they play it from hand, as Greyflood Wanderer,
  -- asks first; what it gains it keeps for the round, for its abilities to read, and
  -- loses as it is played again, a new card
  for_ (headMay [k | fromHand, Modifier _ (MayGainDoomed k) <- ownMods]) \k ->
    push
      $ questionLabelWith
        (ikeyWith "label.giveDoomed" [intVar "count" k])
        (Just $ CardIdTarget card.id)
        []
        pid
        [ Label
            (ikey "label.giveItDoomed")
            [CreateEffect $ Effect (CardIdSource card.id) (CardIdTarget card.id) [GainKeyword (Doomed k)] UntilEndOfRound]
        , Label (ikey "label.doNotGiveDoomed") []
        ]
  let gained e = effectSource e == CardIdSource card.id && effectTarget e == CardIdTarget card.id
  pure $ g & activeCostsL %~ insertEntity activeCost & effectsL %~ filter (\e -> not (spent e || gained e))

-- | The Doomed a card is printed with.
printedDoomed :: HasCardDef a => a -> Int
printedDoomed a = sum [n | Doomed n <- toList (cdKeywords $ toCardDef a)]

-- | The card a message reveals, with the player it is put into play engaged with, if any.
revealedCard :: Message -> Maybe (Maybe PlayerId, EncounterCard)
revealedCard = \case
  RevealCard card -> Just (Nothing, card)
  RevealCardEngaged pid card -> Just (Just pid, card)
  _ -> Nothing

isDiscardNextRevealed :: ModifierType -> Bool
isDiscardNextRevealed = \case
  DiscardNextRevealed _ -> True
  _ -> False

-- | Progress placed on a quest card or a location opens a window, as Pathless Country answers.
isQuestOrLocation :: Target -> Bool
isQuestOrLocation = \case
  QuestTarget _ -> True
  LocationTarget _ -> True
  _ -> False

-- | The locations in the staging area that take progress before the active location, as
-- Grim Foothills, with the progress each still takes.
takingProgressFirst :: GameT [(LocationId, Int)]
takingProgressFirst = do
  staged <- select LocationInStagingArea
  flip mapMaybeM staged \lid -> do
    first <- elem TakesProgressBeforeActiveLocation <$> getModifiers lid
    l <- getLocation lid
    room <- subtract l.progress <$> getLocationQuestPoints lid
    pure $ guard (first && room > 0) $> (lid, room)

{- | Place the progress on one of the locations that take it first, the first player
choosing which, and the rest as the function says.
-}
progressFirst :: Source -> Int -> [(LocationId, Int)] -> (Int -> [Message]) -> GameT Message
progressFirst source n firsts rest = do
  lead <- getLead
  let place (lid, room) = let k = min n room in PlaceTokens source (toTarget lid) Progress k : rest (n - k)
  pure $ case firsts of
    [first] -> Run (place first)
    _ ->
      questionLabelWith (ikey "label.progressFirst") Nothing (map (toTarget . fst) firsts) lead
        $ [targetLabel lid (place first) | first@(lid, _) <- firsts]
