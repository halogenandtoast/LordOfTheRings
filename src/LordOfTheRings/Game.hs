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
import LordOfTheRings.Card.Synthetic (orcGuard, reanimatedDead)
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
import LordOfTheRings.Location.Grid (adjacentPositions)
import LordOfTheRings.Log.Block
import LordOfTheRings.Log.Narrator
import LordOfTheRings.Log.Refs (cardTitle, sendLogInOpenBlock, sourceCardTitle)
import LordOfTheRings.Matcher
import LordOfTheRings.Matcher.Window (windowMatches)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (choosePlayerM, drawCards, raiseThreatFromDoomed, shuffleIntoDeck)
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
import LordOfTheRings.Scenario.Sailing (discardEncounterCard)
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Trait (Trait (Grotto))
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
    , gameLeftPlayLocations = mempty
    , gameAttackDestroyed = []
    , gameDamageDealt = []
    , gameSeparation = Nothing
    , gameAdditionalPhases = []
    , gameResumeAfter = Nothing
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
      checkUnblockedQuests g
      discardAtNoWillpower
      discardDownToHandLimit
      claimFreeObjectives
      endWaitingGame
      pure True

-- | A game that could not end, as Ghulat's, ends with the outcome that waited once it can.
endWaitingGame :: GameT ()
endWaitingGame = do
  g <- getGame
  let waiting = [o | e <- gameEffects g, GameEndsWith o <- effectModifiers e]
      isWaiting = any (\case GameEndsWith _ -> True; _ -> False) . effectModifiers
  for_ (headMay waiting) \outcome -> do
    cannotEnd <- gameCannotEndWith outcome
    unless cannotEnd do
      putGame $ g & effectsL %~ filter (not . isWaiting)
      push $ GameOver outcome

-- | The game cannot end now with this outcome: it cannot end, or the players cannot win.
gameCannotEndWith :: Outcome -> GameT Bool
gameCannotEndWith outcome = do
  mods <- getModifiers GameTarget
  pure $ GameCannotEnd `elem` mods || (outcome == Won && PlayersCannotWin `elem` mods)

{- | A quest card that could not be defeated, and now can, is checked again: one with
enough progress is defeated. The game given has the modifiers from before the message.
-}
checkUnblockedQuests :: Game -> GameT ()
checkUnblockedQuests before = do
  let wasBlocked q = CannotBeDefeated `elem` map modifierType (Map.findWithDefault [] (toTarget q) (gameModifiers before))
  quests <- getsGame (filter wasBlocked . toList . (.entities.quests))
  for_ quests \q -> do
    blocked <- elem CannotBeDefeated <$> getModifiers (toTarget q)
    unless blocked $ push $ CheckStage (toId q)

{- | A player with more cards in hand than he may have, as In Enemy Territory allows him
5, chooses and discards cards down to it at once.
-}
discardDownToHandLimit :: GameT ()
discardDownToHandLimit = do
  over <- getsGame (isJust . (.outcome))
  unless over do
    queued <- peekQueue
    players <- getPlayers
    for_ players \p -> do
      mods <- getModifiers (PlayerTarget p.id)
      let excess = maybe 0 (length p.hand -) (minimumMay [k | HandSizeLimit k <- mods])
          -- a discard already on its way, or being asked for
          pending = flip any queued \case
            ChooseAndDiscardFromHand pid _ _ -> pid == p.id
            DiscardFromHand pid _ -> pid == p.id
            Ask pid (QuestionLabel l _ _ _) -> pid == p.id && l == ikey "label.chooseCardToDiscardFromHand"
            _ -> False
      when (excess > 0 && not pending && CannotDiscardFromHand `notElem` mods && notNull (discardableHand p))
        $ push
        $ ChooseAndDiscardFromHand p.id GameSource excess

{- | An objective the first player claims once nothing guards it ('FirstPlayerClaimsWhenFree'):
one in the staging area whose guard has left play, or that never found one, unless it is
still being guarded, or claimed, by what is queued.
-}
claimFreeObjectives :: GameT ()
claimFreeObjectives = do
  g <- getGame
  unless (isJust g.outcome) do
    queued <- peekQueue
    let guards = map ((.id) . toCard) (toList g.entities.enemies) <> map ((.id) . toCard) (toList g.entities.locations)
        free o = o.placement == StagingArea && maybe True (`notElem` guards) (objectiveGuard $ toAttrs o)
        claiming = flip any queued \case
          ClaimFreeObjective _ -> True
          ClaimObjective {} -> True
          Ask _ (QuestionLabel l _ _ _) -> l == ikey "label.claimObjective"
          _ -> False
        guarding oid = flip any queued \case
          GuardObjective o -> o == oid
          SetObjectiveGuard o _ -> o == oid
          DiscardTopOfEncounterDeck s _ _ -> s == ObjectiveSource oid
          DiscardedTopOfEncounterDeck s _ _ -> s == ObjectiveSource oid
          _ -> False
    candidates <- flip filterM (filter free $ toList g.entities.objectives) \o ->
      if guarding (toId o) then pure False else elem FirstPlayerClaimsWhenFree <$> getModifiers (toTarget o)
    unless (claiming || null candidates) do
      lead <- getLead
      hasHero <- selectAny (HeroControlledBy $ PlayerWithId lead)
      when hasHero $ for_ (take 1 candidates) $ push . ClaimFreeObjective . toId

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
event in a hand, or being paid for, is built from its card to be asked. A player side quest
in the victory display is built from its card too, for what it gives while there, as The
Storm Comes reads "while this quest is in the victory display" (see 'inVictoryDisplay').
-}
withHandModifiers :: Game -> GameT Game
withHandModifiers g = do
  MonoidalMap fromHands <- execWriterT do
    for_ g.players \player -> for_ player.hand (handCardModifiers player.id)
    for_ (gameActiveCosts g) \ac -> handCardModifiers ac.activeCostPlayer ac.activeCostCard
    for_ (toAttrs g.scenario).victoryDisplay \case
      PlayerCard pc | cdCardType (toCardDef pc) == PlayerSideQuestType, Just owner <- pc.owner ->
        getModifiersFor $ overAttrs (\a -> a {questOwner = Just owner}) $ lookupQuest (toCardCode pc) pc.id (QuestId UUID.nil)
      _ -> pure ()
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
      . withDefaults [("logBlocks", toJSON ([] :: [Value])), ("leftPlay", object []), ("leftPlayLocations", object []), ("attackDestroyed", toJSON ([] :: [Value])), ("damageDealt", toJSON ([] :: [Value])), ("separation", Null), ("additionalPhases", toJSON ([] :: [Value])), ("resumeAfter", Null)]

instance FromJSON Separation where
  parseJSON = genericParseJSON (aesonOptions $ Just "separation")

instance FromJSON Area where
  parseJSON = genericParseJSON (aesonOptions $ Just "area") . withDefaults [("heldModifiers", toJSON (mempty :: Map Target [Modifier]))]

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
      , areaHeldModifiers = gameModifiers g
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
      [] -> placeOnQuest
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
  -- an enemy that engages only one player, as Chief Turch the first player, engages him
  -- whoever it would have engaged
  EngageEnemy pid eid -> do
    only <- (\ms -> [p | EngagesOnly p <- ms]) <$> getModifiers (EnemyTarget eid)
    case only of
      p : _ | p /= pid -> g <$ push (EngageEnemy p eid)
      _ -> runGame'' msg g
  -- no ally enters play once one has this round, as Corsair Waters reads: one to be put
  -- into play stays where it is, or goes to its owner's discard pile if it has left it
  PutAllyIntoPlay {} | allyEntryBarred g -> pure g
  PutAllyIntoPlayUnder _ card | allyEntryBarred g -> do
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
        pushAll $ letGoOf g lid <> [ArrangeIslandMap | onMap] <> [CheckWindows [mkWhen $ BecameActiveLocation lid], CheckWindows [mkAfter $ BecameActiveLocation lid]]
        pure g'
  -- and as it is explored, before it leaves play
  CheckWindows ws | explored@(_ : _) <- [lid | Window When (Explored lid) <- ws] -> do
    pushAll $ concatMap (letGoOf g) explored
    runGame'' msg g
  -- an Uncharted location's card cannot enter the discard pile or the encounter deck: it
  -- returns to the Uncharted deck (The Grey Havens rules insert)
  Discard (LocationTarget lid) | unchartedCard lid -> g <$ push (ShuffleIntoUnchartedDeck lid)
  ShuffleIntoEncounterDeck (LocationTarget lid) | unchartedCard lid -> g <$ push (ShuffleIntoUnchartedDeck lid)
  -- nor can a Grotto deck card: it returns to the Grotto deck (The Drowned Ruins)
  Discard (LocationTarget lid) | grottoCard lid -> g <$ push (ShuffleIntoGrottoDeck lid)
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
  Discard t | character t -> Just t
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
            _ -> False
          -- a location leaving play is kept aside for its own abilities to answer it
          departed = if movingAreas then mempty else Map.difference g.entities.locations g'.entities.locations
      unless movingAreas
        $ pushAll
        $ map (PlaceTimeCounters . EnemyTarget) (entered g.entities.enemies g'.entities.enemies)
        <> map (PlaceTimeCounters . LocationTarget) (entered g.entities.locations g'.entities.locations)
        <> [PlaceTimeCounters (toTarget q) | q <- toList g'.entities.quests, q.flipped, not (wasFlipped $ toId q)]
        <> [PlaceTimeCounters (toTarget q) | q <- toList g'.entities.quests, enteredSideQuest q]
        <> [CheckWindows [mkAfter $ LocationLeftPlay lid] | lid <- Map.keys departed]
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
  -- the refresh phase's readying is over once the character has readied
  Ready t -> pure $ g & effectsL %~ filter (\e -> not (effectTarget e == t && effectModifiers e == [ReadiedByRefresh]))
  -- read off the damage its last attack dealt (see 'attackDamaged')
  IfAttackDamaged eid targets msgs -> do
    damaged <- attackDamaged eid
    when (any (\(t, n) -> n > 0 && t `elem` targets) damaged) $ pushAll msgs
    pure g
  IfAttackDealtNoDamage eid msgs -> do
    dealt <- sum . map snd <$> attackDamaged eid
    when (dealt == 0) $ pushAll msgs
    pure g
  OpenLogBlock block -> pure $ g & logBlocksL %~ (<> [block])
  -- an ally that has just left play answers its own leaving
  UseCardAbility _ (AllySource aid) _ _ | Just a <- Map.lookup aid (gameLeftPlay g) -> do
    a' <- runEach msg a
    pure $ g & leftPlayL . ix aid .~ a'
  ForgetLeftPlay aid -> pure $ g & leftPlayL %~ Map.delete aid
  -- a location that has left play answers its own leaving
  UseCardAbility _ (LocationSource lid) _ _ | Just l <- Map.lookup lid (gameLeftPlayLocations g), not (Map.member lid g.entities.locations) -> do
    l' <- runEach msg l
    pure $ g & leftPlayLocationsL . ix lid .~ l'
  CloseLogBlock bid -> pure $ g & logBlocksL %~ filter ((/= bid) . (.id))
  -- step 8 follows the quest's own setup: a nightmare setup card's instructions, as its
  -- "the game is now ready to begin" reads; step 9 a campaign card's setup that needs what
  -- the quest put into play, as Iârion, or the fleet, as the Dream-chaser campaign's boons
  Setup -> do
    pushAll [DoStep n Setup | n <- [1 .. 9]]
    pure g
  DoStep 3 Setup -> do
    firstPlayer <- uniform g.playerOrder
    push $ SetFirstPlayer firstPlayer
    pure g
  BeginRound -> do
    pushAll [BeginPhase minBound, EndPhase minBound]
    -- no area has begun the round's first phase, those separated before the first round
    -- too, as at Unexpected Rescue
    pure $ g & roundL +~ 1 & separationL %~ fmap (\s -> s {separationAreas = [a {areaStatus = AreaWaiting} | a <- separationAreas s]})
  EndRound -> do
    -- each area ends the round in turn
    let (step, g') = eachAreaThen (const [EndRound]) id BeginRound g
    unless (isJust g.outcome) do
      -- a player who played Justice Shall Be Done this round leaves the game
      eliminated <- filterM (fmap (elem EliminatedAtEndOfRound) . getModifiers . PlayerTarget) (Map.keys g.players)
      pushAll $ map EliminatePlayer eliminated
      leaving <- filterM (fmap (elem DiscardAtEndOfRound) . getModifiers) (toList g.entities.allies)
      pushAll [Discard (toTarget a) | a <- leaving]
      removed <- filterM (fmap (elem RemoveFromGameAtEndOfRound) . getModifiers) (toList g.entities.heroes)
      pushAll [RemoveFromGame (toTarget h) | h <- removed]
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
      -- each Aflame location in the staging area takes 1 damage (Raid on the Grey Havens)
      burning <- filterM (fmap (> 0) . getAflame) =<< select LocationInStagingArea
      pushAll [DealDamage GameSource (LocationTarget lid) 1 | lid <- burning]
      -- the round's lasting effects end once its end-of-round effects have resolved, which
      -- may read what happened this round, as Sneak Past the Guards
      pushAll [CheckWindows [mkWhen RoundEnds], EndEffects UntilEndOfRound]
      pushEnd step
    pure $ if isJust g.outcome then g' & effectsL %~ filter ((/= UntilEndOfRound) . effectDuration) else g'
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
    raises <- for players \pid -> (pid,) . fromMaybe 1 . lastMay . (\ms -> [n | RefreshThreatRaise n <- ms]) <$> getModifiers (PlayerTarget pid)
    -- each character readied opens its own window, for responses to it readying
    readied <- for (filter (`notElem` held) characters) \c -> do
      traits <- characterTraits c
      pure [refreshing c, Ready c, CheckWindows [mkAfter $ Readied c traits]]
    pushAll
      $ concat regenerating
      <> concat readied
      <> [RefreshUpTo pid n cs | (pid, n, cs) <- limited]
      <> [Ready (toTarget a) | a <- toList g.entities.attachments, attachmentExhausted (toAttrs a)]
      <> [Ready (toTarget o) | o <- toList g.entities.objectives, o.exhausted]
      <> [RaiseThreat pid n | (pid, n) <- raises, n > 0]
      <> passToken
      -- the phase ends with an action window, for Refresh Actions as Escape from Darkness gives
      <> map OptionalActionWindow players
    pure $ g & phaseL .~ RefreshPhase
  SetFirstPlayer pid -> pure $ g & firstPlayerL .~ pid & separationL %~ fmap (\s -> s {separationFirstPlayer = pid})
  SeparatePlayers groups -> pure $ separate groups g
  FocusArea pid -> pure $ focusArea pid g
  -- while the players are separated, an enemy sent to another area's staging area leaves
  -- this one, what is attached to it going with it
  PlaceInStagingAreaOf pid eid
    | Map.member pid g.players -> g <$ push (PlaceEnemy eid StagingArea)
    | Just s <- gameSeparation g
    , any (elem pid . areaPlayers) (separationAreas s)
    , Just e <- Map.lookup eid g.entities.enemies -> do
        pushAll $ discardShadowsOf g (EnemyTarget eid)
        let on' :: HasField "placement" a Placement => a -> Bool
            on' a = a.placement == AttachedTo (EnemyTarget eid)
            moving = mempty {entitiesEnemies = Map.singleton eid (overAttrs (\a -> a {enemyPlacement = StagingArea, enemyShadowCards = []}) e), entitiesAttachments = Map.filter on' g.entities.attachments, entitiesTreacheries = Map.filter on' g.entities.treacheries}
            arrive a = if elem pid (areaPlayers a) then a {areaHeldEntities = areaHeldEntities a <> moving} else a
        pure
          $ g
          & entitiesL . enemiesL %~ Map.delete eid
          & entitiesL . attachmentsL %~ Map.filter (not . on')
          & entitiesL . treacheriesL %~ Map.filter (not . on')
          & separationL ?~ s {separationAreas = map arrive (separationAreas s)}
  -- a shadow card for an enemy known by its card, as one just put into play
  DealShadowCardToCard cid -> do
    for_ (find ((== cid) . enemyCardId . toAttrs) (toList g.entities.enemies)) \e -> push $ DealShadowCard (toId e)
    pure g
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
      pure $ targetLabel c [refreshing c, Ready c, CheckWindows [mkAfter $ Readied c traits], RefreshUpTo pid (n - 1) (filter (/= c) cs)]
    push $ questionLabel (ikeyWith "label.refreshUpTo" [intVar "count" n]) pid choices
    pure g
  CheckEliminations -> do
    emptyDeck <- elem EliminatedWithEmptyDeck <$> getModifiers GameTarget
    eliminated <- forMaybeM (toList g.players) \player -> do
      heroes <- select (HeroControlledBy $ PlayerWithId player.id)
      level <- getEliminationLevel player.id
      -- a hero shuffled into the deck, as Lost and Alone does, is not killed (FAQ 2.37)
      let noHeroes = null heroes && not (any ((== HeroType) . (.cardType)) player.deck)
          byThreat = player.threat >= level
          -- Deadmen's Dike eliminates a player whose deck runs out
          byEmptyDeck = emptyDeck && null player.deck
      pure $ guard (not player.eliminated && (byThreat || noHeroes || byEmptyDeck)) $> (player.id, byThreat && not noHeroes)
    -- one reaching the elimination level opens a window first, as Favor of the Valar
    -- answers to stop it
    pushAll $ concat [[CheckWindows [mkWhen $ EliminatedByThreat pid] | byThreat] <> [EliminatePlayer pid] | (pid, byThreat) <- eliminated]
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
    cannotEnd <- gameCannotEndWith outcome
    -- the outcome waits while the game cannot end, unless no player is left to play on;
    -- see 'endWaitingGame'
    if cannotEnd && any (not . (.eliminated)) g.players
      then g <$ push (CreateEffect $ Effect GameSource GameTarget [GameEndsWith outcome] UntilSpent)
      else do
        -- won in campaign mode, the campaign card's resolution is recorded in the log
        g' <-
          if outcome == Won && isJust (toAttrs g.scenario).campaign
            then traverseOf scenarioL (runEach ResolveCampaign) g
            else pure g
        clearQueue
        pure $ g' & outcomeL ?~ outcome
  -- conceding is not a loss by the rules, so nothing that stops the game ending stops it
  Surrender -> do
    clearQueue
    pure $ g & questionL .~ mempty & outcomeL ?~ Lost
  -- an ally put into play by its card, as Emery is, discarded by its card
  Discard (CardIdTarget cid)
    | Just a <- find ((== cid) . allyCardId . toAttrs) (toList g.entities.allies) ->
        g <$ push (Discard $ toTarget a)
  Discard target -> discardTarget target g
  DealDamage source target n | n > 0 -> do
    mods <- getModifiers target
    -- an enemy's attack's damage may go elsewhere, as Orc Saboteur's to a Battleground,
    -- not to one of those it already goes to
    attackRedirects <- case source of
      EnemyAttackSource eid -> (\ms -> [t | AttackDamageRedirectedTo t <- ms]) <$> getModifiers eid
      _ -> pure []
    -- the player whose card deals it, as Marshland Outlaws reads
    controller <- if isPlayerCardSource source then playerCardController source else pure Nothing
    -- damage another character takes in its place, as Song of Mocking's
    let redirected =
          ordNub
            $ filter (inPlay g)
            $ [t | DamageRedirectedTo t <- mods]
            <> (if target `elem` attackRedirects then [] else filter (/= target) attackRedirects)
    lead <- getLead
    -- before it is dealt, a Forced effect may cancel it, as Patrol Leader's: see 'cancelDamage'
    -- damage redirected is applied where it goes, not redirected again, as two Songs of
    -- Mocking each choosing the other's hero
    let dealTo t = do
          mods' <- getModifiers t
          -- damage reduced to at most so much, as Nazgûl of Minas Morgul's to 1
          let n' = foldr min n [k | DamageReducedTo k <- mods']
              combat = case source of
                PlayerAttackSource _ -> True
                _ -> False
              barred =
                (CannotBeDamagedByPlayerCardEffects `elem` mods' && isPlayerCardSource source)
                  || maybe False (\p -> CannotBeDamagedByEffectsOf p `elem` mods') controller
          pure
            $ if CannotTakeDamage `elem` mods' || (CannotTakeNonCombatDamage `elem` mods' && not combat) || barred
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
    attackMods <- case source of
      EnemyAttackSource eid -> getModifiers eid
      _ -> pure []
    let raising = [pid | AttackDamageRaisesThreatOf pid <- attackMods]
        -- each point of it removes 1 progress from each quest card, as Traitorous Wight's
        -- shadow reads
        removing =
          [ RemoveTokens source (toTarget q) Progress (min n q.progress)
          | AttackDamageRemovesProgress `elem` attackMods
          , q <- toList g.entities.quests
          , q.progress > 0
          ]
    defending <- elem Defending <$> getModifiers target
    let recorded = damageDealtL %~ (<> [(source, target, n)])
    pushAll
      $ [CreateEffect $ Effect GameSource target [DamagedWhileDefending] UntilEndOfAttack | defending]
      -- kept until the enemy attacks again, as what its attack destroyed is
      <> [ PlaceTokens source target Damage n
         , CheckWindows $ mkAfter (DealtDamage target n) : [mkAfter $ DamagedByEnemyAttack target eid | EnemyAttackSource eid <- [source]]
         , CheckDefeated source target
         ]
      -- a hero it leaves standing is taken prisoner, as Torture Master's shadow reads
      <> [TakePrisoner hid | AttackDamageTakesPrisoner `elem` attackMods, HeroTarget hid <- [target]]
      <> concat [[RaiseThreat pid n, CheckWindows [mkAfter $ ThreatRaisedByEncounter pid n]] | pid <- raising]
      <> [DiscardRandomFromHand pid n | AttackDamageDiscardsRandomFromHandOf pid <- attackMods]
      <> removing
      -- each point of it adds 1 to the staging area's threat this phase, as Ramming Speed!
      <> [CreateEffect $ Effect source GameTarget [StagingThreatBonus n] UntilEndOfPhase | AttackDamageAddsStagingThreat `elem` attackMods]
      <> [DiscardRandomFromHand pid n | AttackDamageDiscardsRandomFrom pid <- attackMods]
      <> [HealDamage source t n | AttackDamageHeals t <- attackMods]
      <> [CreateEffect $ Effect source t [m] UntilEndOfRound | n > 0, AttackDamageModifiesUntilEndOfRound t m <- attackMods]
    pure $ recorded g
  HealDamage source target n | n > 0 -> do
    damage <- getDamage target
    mods <- getModifiers target
    -- a card is named by its title (rules reference), so "other than Athelas" spares the
    -- player attachment Athelas too
    let from = sourceCardTitle g source
        title code = cardTitle <$> lookupCardDef code
        healed = foldr min (min n damage) $ [k | HealLimit k <- mods] <> [k | HealLimitExceptFrom k code <- mods, from /= title code]
    when (healed > 0 && CannotBeHealed `notElem` mods)
      $ pushAll [RemoveTokens source target Damage healed, CheckWindows [mkAfter $ Healed target source]]
    pure g
  ChooseAndDiscardFromHand pid source n | n > 0 -> do
    player <- getPlayer pid
    barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
    unless (barred || null (discardableHand player))
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
      -- an indestructible enemy, printed or gained as Daechanar's, keeps taking damage past
      -- its hit points; one that lost the keyword, as Thaurdir may, is destroyed as any other
      EnemyTarget eid | Just e <- Map.lookup eid g.entities.enemies -> do
        indestructible <- hasKeyword Indestructible target (toCardDef e)
        if indestructible then pure False else (>=) . (.damage) <$> getEnemy eid <*> getEnemyHitPoints eid
      -- an objective ally in the staging area, as Celador
      ObjectiveTarget oid | Just o <- Map.lookup oid g.entities.objectives, isJust (cdHitPoints $ toCardDef o) ->
        (>=) <$> getDamage target <*> getHitPoints target
      _ -> pure False
    when destroyed $ push (Destroy source target)
    pure g
  -- the When window first, while the enemy and what is attached to it are still in play,
  -- as Secret Vigil answers
  Destroy _ (EnemyTarget eid) | Map.member eid g.entities.enemies -> do
    pushAll [CheckWindows [mkWhen $ EnemyDefeated eid], DoStep 1 msg]
    pure g
  DoStep 1 (Destroy source target@(EnemyTarget eid)) | Map.member eid g.entities.enemies -> do
    enemy <- getEnemy eid
    stays <- elem StaysInPlayWhenDefeated <$> getModifiers target
    let ec = MkEncounterCard (enemyCardId $ toAttrs enemy) (toCardCode enemy)
        card = EncounterCard ec
        -- what a player's attack destroyed, for the attack to read as it resolves
        recorded = attackDestroyedL %~ (<> [AttackDestroyed source target Nothing card Nothing | PlayerAttackSource _ <- [source]])
        -- one of the Evil Creatures deck goes to its discard pile, even with victory
        -- points (Journey Up the Anduin rules)
        victory = cdVictoryPoints (toCardDef enemy) <* guard (not $ isEvilCreaturesCard (toAttrs g.scenario) ec)
    push $ CheckWindows [mkAfter $ EnemyDefeated eid, mkAfter $ EnemyDestroyed card]
    -- one that stays in play for its own effect was still destroyed by the attack
    if stays then pure (recorded g) else recorded <$> case victory of
      Just _ -> do
        -- what is attached to it is discarded, an objective it guarded as a Key of The Three
        -- Trials, and a Mount, as Mordor Warg, goes to the staging area
        pushAll $ discardAttached g target
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
        under = [CharacterDestroyedUnder pid target | Just pid <- [controller]]
    pushAll
      $ [CheckWindows $ map mkWhen (CharacterDestroyed target c : under) | Just c <- [card]]
      <> [Discard target, CheckEliminations]
      <> [ CheckWindows
             $ mkAfter (CharacterDestroyed target c)
             : map mkAfter under
             <> [mkAfter $ CharacterDestroyedByAttack eid target pid | (eid, _, pid) <- byAttack]
         | Just c <- [card]
         ]
    let attacker eid = toCard <$> Map.lookup eid g.entities.enemies
    pure $ g & attackDestroyedL %~ (<> [AttackDestroyed (EnemyAttackSource eid) target (Just pid) c (attacker eid) | (eid, c, pid) <- byAttack])
  -- the heroes collect and the players draw, then each player has an action window, for
  -- Resource Actions as Captain's Wisdom's
  BeginPhase ResourcePhase -> do
    order <- getPlayerOrder
    pushAll $ DoPhase ResourcePhase : map OptionalActionWindow order
    pure $ g & phaseL .~ ResourcePhase
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
    skipped <- elem SkipQuestPhase <$> getModifiers GameTarget
    -- a stage with the Sailing keyword has the first player make a Sailing test before
    -- characters commit (Dream-chaser rules)
    let sailing = [SailingTest | any (\q -> not (isSideQuest q) && Sailing `member` cdKeywords (toCardDef q)) g.entities.quests]
    unless skipped $ pushAll $ sailing <> chooseQuest <> map CommitCharacters order <> [Staging] <> map ActionWindow order <> [QuestResolution]
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
    pushAll [PlaceLocation lid ActiveLocation, CheckWindows [mkAfter $ Traveled lid], ResolveDeep lid]
    pure g
  -- a Deep location, still the active location after the windows of traveling to it, is
  -- replaced by the top card of the Caves deck, unless that deck is empty (MEC65 rules)
  ResolveDeep lid | Just l <- Map.lookup lid g.entities.locations -> do
    deep <- hasKeyword Deep (toTarget l) (toCardDef l)
    ignored <- elem IgnoreDeep <$> getModifiers GameTarget
    pushWhen (deep && not ignored && l.placement == ActiveLocation && notNull (toAttrs g.scenario).cavesDeck)
      $ ReplaceWithTopOfCavesDeck lid
    pure g
  ResolveDeep _ -> pure g
  -- a location becomes the active location, opening its windows; a Dragon Sign, or another
  -- objective, goes to the staging area and resolves its guarded keyword (MEC66 rules)
  ReplaceWithTopOfCavesDeck lid -> case (toAttrs g.scenario).cavesDeck of
    card : rest -> do
      push $ Discard (LocationTarget lid)
      let g' = g & scenarioL %~ overAttrs (cavesDeckL .~ rest)
      case cdCardType (toCardDef card) of
        LocationType -> do
          thisLocation <- newLocation card
          push $ PlaceLocation (toId thisLocation) ActiveLocation
          pure $ g' & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = Unplaced}) thisLocation)
        t | t `elem` [ObjectiveType, ObjectiveAllyType] -> g' <$ push (PutObjectiveIntoPlay card)
        _ -> g' <$ push (AddToStaging card)
    [] -> pure g
  BeginPhase EncounterPhase -> do
    order <- getPlayerOrder
    -- players may act after the optional engagements and after the engagement checks
    pushAll
      $ map OptionalEngagement order
      <> map OptionalActionWindow order
      <> [EngagementCheck pid 0 | pid <- take 1 order]
      <> [CreateEffect $ Effect GameSource GameTarget [AfterEngagementChecks] UntilEndOfPhase]
      <> map OptionalActionWindow order
    pure $ g & phaseL .~ EncounterPhase
  -- a player who may engage an additional enemy, as Halbarad lets, is asked again once
  (optionalEngagement -> Just (pid, first)) -> do
    again <- (first &&) . elem OptionallyEngagesAdditionalEnemy <$> getModifiers (PlayerTarget pid)
    staged <- engageableFromStaging
    -- some enemies may only be engaged once they are alone in the staging area
    barredBy <- (\ms -> [m | CannotBeOptionallyEngagedBy m <- ms]) <$> getModifiers GameTarget
    resources <- getResourcesOfSphere pid Neutral
    enemies <- flip mapMaybeM staged \eid -> do
      mods <- getModifiers eid
      barred <- anyM (\m -> selectAny (PlayerWithId pid <> m)) $ barredBy <> [m | CannotBeOptionallyEngagedBy m <- mods]
      -- the resources it costs to engage, as Drúadan Stalker's 2
      let cost = sum [n | OptionalEngagementCost n <- mods]
      pure
        $ guard
          ( not barred
              && CannotEngagePlayer pid `notElem` mods
              && (length staged <= 1 || CannotBeOptionallyEngagedWhileOthersInStaging `notElem` mods)
              && cost <= resources
          )
        $> (eid, cost)
    unless (null enemies)
      $ push
      $ questionLabel (ikey "label.optionalEngagement") pid
      $ [ targetLabel eid
            $ [SpendResources pid Neutral cost | cost > 0]
            <> [EngageEnemy pid eid, CheckWindows [mkAfter $ OptionallyEngaged eid pid]]
            <> [DoStep 1 (OptionalEngagement pid) | again]
        | (eid, cost) <- enemies
        ]
      <> [Label (ikey "label.doNotEngage") []]
    pure g
  EngagementCheck pid seatsPassed -> do
    skip <- elem NoEngagementChecks <$> getModifiers GameTarget
    order <- getPlayerOrder
    let next = seatAfter order pid
    threat <- getEngagementThreat pid
    candidates <- traverse (\eid -> (eid,) <$> getEngagementCost eid) =<< engageableFromStaging
    -- an enemy can be spared engagement checks against it, as a trap does
    checked <- filterM (fmap (\ms -> all (`notElem` ms) [NoEngagementChecks, CannotEngagePlayer pid, NoEngagementChecksAgainst pid]) . getModifiers . fst) candidates
    -- its cost against this player, as Hunting Pack's is lowered by his damaged characters
    costed <- for checked \(eid, cost) -> (\ms -> (eid, cost + sum [n | EngagementCostAgainst p n <- ms, p == pid])) <$> getModifiers eid
    case sortOn (Down . snd) [c | not skip, c@(_, cost) <- costed, cost <= threat] of
      ((eid, _) : _) -> pushAll [EngageEnemy pid eid, EngagementCheck next 0]
      [] -> when (seatsPassed + 1 < length order) $ push (EngagementCheck next (seatsPassed + 1))
    pure g
  AmbushEngagementCheck eid | Just enemy <- Map.lookup eid g.entities.enemies, enemy.placement == StagingArea -> do
    ambush <- hasKeyword Ambush (toTarget eid) (toCardDef enemy)
    gameMods <- getModifiers GameTarget
    pushWhen (ambush && IgnoreAmbush `notElem` gameMods) $ EnemyEngagementChecks eid
    pure g
  EnemyEngagementChecks eid | Just enemy <- Map.lookup eid g.entities.enemies, enemy.placement == StagingArea -> do
    gameMods <- getModifiers GameTarget
    mods <- getModifiers eid
    when (NoEngagementChecks `notElem` (gameMods <> mods)) do
      cost <- getEngagementCost eid
      -- its cost against each player, as for an engagement check
      let costFor p = cost + sum [n | EngagementCostAgainst p' n <- mods, p' == p]
      players <- traverse (\p -> (p,) <$> getEngagementThreat p) =<< getPlayerOrder
      for_ (find (\(p, threat) -> threat >= costFor p && all (`notElem` mods) [CannotEngagePlayer p, NoEngagementChecksAgainst p]) players) \(p, _) -> push (EngageEnemy p eid)
    pure g
  EngageEnemy pid eid -> do
    -- a Ship enemy engaging from the staging area resolves its Boarding X (Dream-chaser rules)
    let fromStaging = maybe False ((== StagingArea) . (.placement)) (Map.lookup eid g.entities.enemies)
    pushAll
      $ [ CheckWindows [mkWhen $ EnemyEngaged eid pid]
        , PlaceEnemy eid (EngagedWith pid)
        ]
      <> [ResolveBoarding pid eid | fromStaging]
      <> [ CreateEffect $ Effect GameSource (PlayerTarget pid) [EngagedAnEnemyThisRound] UntilEndOfRound
         , CreateEffect $ Effect GameSource (EnemyTarget eid) [EngagedThisRound] UntilEndOfRound
         , CreateEffect $ Effect GameSource GameTarget [EnemyEngagedThisPhase] UntilEndOfPhase
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
    gameMods <- getModifiers GameTarget
    -- Ship objectives take it all while they can, as City Under Siege reads
    ships <-
      if ArcheryDamageToShipObjectives `elem` gameMods
        then withHitPointsLeft assigned . map toTarget =<< select (shipObjective <> AllyControlledBy AnyPlayer)
        else pure []
    let staying = case assigned of
          c : _ | ArcheryDamageStaysOnCharacter `elem` gameMods, c `elem` characters -> [c]
          _ -> characters
        characters'
          | notNull ships = ships
          | otherwise = staying <> ordNub (filter (inPlay g) [t | ArcheryDamageMayGoTo t <- gameMods])
    push
      $ if n > 0 && notNull characters'
        then
          questionLabel (ikeyWith "label.assignArcheryDamage" [intVar "count" n]) lead
            $ [targetLabel c [ArcheryDamage (n - 1) (c : assigned)] | c <- characters']
        else Run $ dealAssigned GameSource assigned
    pure g
  DealShadowCards -> do
    order <- getPlayerOrder
    -- one engaged with each player from the staging area is dealt one for each attack
    -- an enemy with hinder is dealt none
    enemies <- filterM (\e -> not <$> orM [engagedWithEach e, hinders g e]) . ordNub =<< concatMapM attacksOf order
    pushAll $ map DealShadowCard enemies
    pure g
  EnemyAttacks pid -> do
    pushAll . (CreateEffect (Effect GameSource GameTarget [ResolvingEnemyAttacks] UntilEndOfPhase) :) . pure . ChooseEnemyAttack pid =<< attacksOf pid
    pure g
  ChooseEnemyAttack pid candidates -> do
    -- checked again before each attack: a card played between attacks may stop one
    engaged <- (<>) <$> select (EnemyEngagedWith $ PlayerWithId pid) <*> attacksAsIfEngaged pid
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
    push $ CreateEffect $ Effect GameSource (EnemyTarget eid) [AttackedThisPhase] UntilEndOfPhase
    -- what its last attack destroyed or damaged is forgotten as it makes a new one
    pure
      $ g
      & attackDestroyedL %~ filter ((/= EnemyAttackSource eid) . attackDestroyedBy)
      & damageDealtL %~ filter (\(s, _, _) -> s /= EnemyAttackSource eid)
  DefendWith pid eid defender -> do
    windows <- actionWindows
    pushAll
      $ [ defendingEffect eid defender
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
    playerMods <- getModifiers (PlayerTarget pid)
    -- or up to so many, as Hold the Line lets
    let limit = minimumMay [n | MayDeclareUpToDefenders n <- playerMods]
        many = MayDeclareMultipleDefenders `elem` playerMods || isJust limit
        full = maybe False (length chosen >=) limit
    others <- filter (/= pid) <$> getPlayerOrder
    defenders <- if full then pure [] else filter (`notElem` chosen) <$> (filterM (canDefend eid) =<< readyCharacters pid)
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
                  <> [ defendingEffect eid d
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
  DeclareAsDefender pid eid d -> do
    undefendable <- elem CannotBeDefended <$> getModifiers eid
    able <- canFight eid d
    unless (undefendable || not able) do
      pushAll [defendingEffect eid d, CheckWindows [mkAfter $ DeclaredAsDefender d eid]]
      -- the declaration still to come is the card's instead
      withQueue_ \q -> case break pending q of
        (before, DeclareDefenders _ _ chosen : after) -> before <> (DeclareSentinels pid eid (chosen <> [d]) [] : after)
        (before, DeclareSentinels _ _ chosen [] : after) -> before <> (DeclareSentinels pid eid (chosen <> [d]) [] : after)
        _ -> q
    pure g
   where
    pending = \case
      DeclareDefenders pid' eid' _ -> pid == pid' && eid == eid'
      DeclareSentinels pid' eid' _ [] -> pid == pid' && eid == eid'
      _ -> False
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
                <> [ defendingEffect eid d
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
    defense <- sum <$> traverse (getDefenseAgainst eid) defenders
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
    -- a Ship enemy's goes to a Ship objective the player controls (Dream-chaser rules)
    ship <- selectAny (EnemyWithId eid <> shipEnemy)
    ships <- if ship then map toTarget <$> shipObjectivesOf pid else pure []
    heroes <-
      if
        | notNull redirected -> pure []
        | notNull ships -> pure ships
        | otherwise -> (<> watchmen) . map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
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
          $ questionLabelWith (ikey "label.chooseNextEnemyToAttack") Nothing (map toTarget targets) pid
          $ [ targetLabel eid [DeclareAttackers pid eid, ChooseAttackTarget pid (filter (/= eid) targets)]
            | eid <- targets
            ]
          <> [Label (ikey "label.doNotAttack") []]
    pure g
  AttackStagingAreaAlone pid attacker -> do
    ready <- elem attacker <$> readyAttackers pid
    staged <-
      filterM (\e -> andM [notElem CannotBeAttacked <$> getModifiers e, (> 0) . fromMaybe 1 <$> attackerRoom e])
        =<< select EnemyInStagingArea
    when (ready && notNull staged)
      $ push
      $ questionLabelWith (ikey "label.attackStagingAlone") (Just attacker) [] pid
      $ [ targetLabel
            eid
            $ [ AttackWith attacker
              , CreateEffect $ Effect GameSource attacker [AttackModifier 1] UntilEndOfAttack
              ]
            <> declaredAttacker attacker eid
            <> [CheckWindows [mkAfter $ AttackersDeclared eid], ResolveAttack pid eid]
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
                  (AttackWith a : declaredAttacker a eid <> [DeclareAttackers pid eid])
              | a <- attackers
              ]
            <> [Label (ikey "label.doneAttacking") [DeclareRangedAttackers pid eid others]]
    pure g
  -- the attack resolves, or its attackers stop, once no one else is left to ask; players
  -- may act once its attackers are declared and once its damage is dealt
  DeclareRangedAttackers pid eid others | null others || not (Map.member eid g.entities.enemies) -> do
    attackers <- selectCharacters AttackingHero AttackingAlly
    let attacked = notNull attackers && Map.member eid g.entities.enemies
    windows <- if attacked then actionWindows else pure []
    pushAll $ [CheckWindows [mkAfter $ AttackersDeclared eid] | attacked] <> windows <> [ResolveAttack pid eid] <> windows
    pure g
  DeclareRangedAttackers pid eid (other : rest) -> do
    attacking <- selectCharacters AttackingHero AttackingAlly
    ranged <-
      filterM (canAttack eid)
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
                  (AttackWith a : declaredAttacker a eid <> [DeclareRangedAttackers pid eid (other : rest)])
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
        destroyed = any byThisAttack $ gameAttackDestroyed g
    pushWhen destroyed $ CheckWindows $ mkAfter (EnemyDestroyedByAttack eid attackers traits)
      : [mkAfter $ EngagedEnemyDestroyedByAttack pid attackers | Just pid <- [engaged]]
      <> take 1 [mkAfter $ EnemyCardDestroyedByAttack eid (attackDestroyedCard d).id | d <- gameAttackDestroyed g, byThisAttack d]
    -- the attacking player draws for it, as Knife-work lets
    when destroyed do
      attacking <- ordNub . catMaybes <$> traverse characterController attackers
      for_ attacking \pid -> do
        mods <- Map.findWithDefault [] (PlayerTarget pid) <$> getsGame gameModifiers
        for_ [m.modifierSource | m <- mods, m.modifierType == DrawsOnDestroyingEnemy] \source ->
          runQueueT $ drawCards source pid 1
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
    defense <- getDefenseAgainst eid defender
    engaged <- placementController . (.placement) <$> getEnemy eid
    lead <- getLead
    pushAll =<< attackDamage (fromMaybe lead engaged) eid defender (attack - defense)
    pure g
  ResolveShadow eid pid defenders -> do
    -- a shadow card with no effect may be given one, as Mountains of Angmar gives
    gained <- elem ShadowlessCardsGainEffect <$> getModifiers GameTarget
    for_ (Map.lookup eid g.entities.enemies) \enemy -> do
      -- each shadow card turns face up, and one with an effect resolves it; one already face
      -- up resolved in an earlier attack, as an enemy attacking again has, unless it was
      -- dealt face up
      let resolving card =
            RevealShadowCard eid (ecId card)
              : if gained || cdShadowEffect (toCardDef card)
                then
                  [ CheckWindows $ mkWhen (ShadowEffectTriggered (ecId card) eid)
                      : [mkWhen $ ShadowEffectAgainst (ecId card) d | d <- defenders]
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
        push . CheckShadowRaisedAttack cid eid =<< getEnemyAttack eid
        resolveShadowEffect card (ResolveShadowEffect cid eid pid defenders)
    pure g
  -- the shadow effect has resolved: one that raised its enemy's attack opens a window, as
  -- Tides of Fate answers
  CheckShadowRaisedAttack cid eid before -> do
    when (Map.member eid g.entities.enemies) do
      after <- getEnemyAttack eid
      pushWhen (after > before) $ CheckWindows [mkWhen $ ShadowIncreasedAttack cid eid]
    pure g
  -- the card, in the discard pile, is built again to resolve its Forced effect, as a
  -- shadow card is
  ResolveSailingDiscard card _ -> do
    resolveShadowEffect card msg
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
  AttachFromEncounterDiscard card host | card `elem` (toAttrs g.scenario).encounterDiscard -> do
    treachery <- overAttrs (\a -> a {treacheryPlacement = AttachedTo host}) <$> newTreachery card
    pure $ g & entitiesL . treacheriesL %~ insertEntity treachery & scenarioL %~ overAttrs (encounterDiscardL %~ filter (/= card))
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
    divideDamage source pid n assigned characters (DealDamageDivided source pid)
    pure g
  DealDamageDividedAmong source pid among n assigned -> do
    characters <- withHitPointsLeft assigned $ filter (inPlay g) among
    if n > 0 && notNull characters
      then
        push
          $ questionLabelWith (ikeyWith "label.dealDamageDivided" [intVar "n" n]) (sourceTarget source) [] pid
            [targetLabel c [DealDamageDividedAmong source pid among (n - 1) (c : assigned)] | c <- characters]
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
  PayDamageHeroCost pid matcher n rest -> do
    heroes <- asPlayer pid (damageableHeroes matcher)
    push
      $ questionLabelWith (ikeyWith "label.dealDamageToOneHero" [intVar "n" n]) Nothing [] pid
        [targetLabel h (DealDamage (PlayerSource pid) (toTarget h) n : rest) | h <- heroes]
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
  SpendGroupResources pid sphere n | n > 0 -> g <$ push (SpendResourcesChosen pid True sphere n [])
  SpendResourcesChosen pid True sphere n chosen -> do
    heroes <- traverse getHero =<< select AnyHero
    payers <- filterM (heroPaysFor sphere) heroes
    push $ spendChosen pid True sphere n chosen [(h.id, h.resources) | h <- payers]
    pure g
  DiscardGroupResources pid n | n > 0 -> do
    heroes <- filter ((> 0) . (.resources)) <$> (traverse getHero =<< select AnyHero)
    unless (null heroes)
      $ push
      $ questionLabel (ikeyWith "label.discardMore" [intVar "count" n]) pid
      $ [ targetLabel h [RemoveTokens GameSource (toTarget h) Resource 1, DiscardGroupResources pid (n - 1)]
        | h <- heroes
        ]
    pure g
  DamageAflameLocation source pid -> do
    burning <- filterM (fmap (> 0) . getAflame) =<< select LocationInStagingArea
    pushWhen (notNull burning) $ chooseOrRunOne pid [targetLabel l [DealDamage source (LocationTarget l) 1] | l <- burning]
    pure g
  AllyEnteredPlayUnderController aid -> do
    for_ (Map.lookup aid g.entities.allies) \a -> case (toAttrs a).allyPlacement of
      InPlayArea pid -> push $ allyEntersPlay pid a
      _ -> pure ()
    pure g
  AttackEndedUnresolved _ -> pure $ g & effectsL %~ filter ((/= UntilEndOfAttack) . effectDuration)
  DamageAnotherCharacter source pid damaged -> do
    others <- filter (/= damaged) <$> charactersControlledBy pid
    -- each it damages is exhausted, as Terrible Strength reads: checked once the damage is
    -- dealt, against what the character had before
    exhausting <- case source of
      EnemyAttackSource eid -> elem ExhaustsCharactersDamagedByExcess <$> getModifiers eid
      _ -> pure False
    damage <- for others \c -> (c,) <$> getDamage c
    let deal c =
          DealDamage source c 1
            : [DoStep before (DamageAnotherCharacter source pid c) | exhausting, Just before <- [lookup c damage]]
    push $ case others of
      [] -> Run []
      [c] -> Run (deal c)
      _ ->
        questionLabelWith (ikey "label.damageAnotherCharacter") (sourceTarget source) [] pid
          $ [targetLabel c (deal c) | c <- others]
    pure g
  DoStep before (DamageAnotherCharacter _ _ c) -> do
    damaged <- if inPlay g c then (> before) <$> getDamage c else pure False
    pushWhen damaged $ Exhaust c
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
  CommitCharacters pid -> do
    -- one that can commit while exhausted, as Lindon Navigator, need not be ready
    let commitsExhausted :: Targetable t => t -> GameT Bool
        commitsExhausted c = elem CanCommitWhileExhausted <$> getModifiers c
    heroes <-
      (<>)
        <$> select (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> not_ CommittedHero)
        <*> (filterM commitsExhausted =<< select (HeroControlledBy (PlayerWithId pid) <> ExhaustedHero <> not_ CommittedHero))
    allies <-
      (<>)
        <$> select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> not_ CommittedAlly)
        <*> (filterM commitsExhausted =<< select (AllyControlledBy (PlayerWithId pid) <> ExhaustedAlly <> not_ CommittedAlly))
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
    mustCommit <- filterM (fmap (elem MustCommitToQuest) . getModifiers) characters
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
         , -- a character that must commit, as A Fell Dread's hero, is committed first
           null mustCommit
         ]
    pure g
  Staging -> do
    push $ CreateEffect $ Effect GameSource GameTarget [StagingStepBegun] UntilEndOfPhase
    mods <- getModifiers GameTarget
    let extra = sum [n | AdditionalStagingReveals n <- mods]
        fewer = sum [n | FewerStagingReveals n <- mods]
        players = length [() | player <- toList g.players, not player.eliminated]
        reveals = max 0 $ max 1 (players + extra) - fewer
    -- what is played "after the staging step", as Elevenses, waits for its end
    pushAll
      $ ( if SkipStagingStep `elem` mods
            then []
            else
              CheckWindows [mkAfter StagingStepBegins]
                : replicate reveals RevealEncounterCard
                <> [CheckWindows [mkWhen StagingStepEnds]]
        )
      <> [CreateEffect $ Effect GameSource GameTarget [AfterStagingStep] UntilEndOfPhase]
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
            , CheckWindows [mkAfter PlacedProgressFromQuesting]
            ]
      | threat > willpower ->
          -- responses before the threat is raised may stop it, as Ever Onward's: see
          -- 'doesNotRaiseThreat'
          do
            spared <- elem NoThreatFromQuestingUnsuccessfully <$> getModifiers GameTarget
            let delayed = [PlaceTokens (effectSource e) t Progress n | e <- gameEffects g, ProgressIfQuestUnsuccessful t n <- effectModifiers e]
            pushAll
              $ CheckWindows [mkWhen QuestedUnsuccessfully]
              : delayed
              <> [RaiseThreat pid (threat - willpower) | not spared, pid <- players]
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
    | Just o <- find ((== cid) . objectiveCardId . toAttrs) (toList g.entities.objectives) ->
        g <$ push (PlaceTokens source (toTarget o) token n)
    -- an enemy that enters play with tokens, as Light Cruiser's boarders
    | Just e <- find ((== cid) . enemyCardId . toAttrs) (toList g.entities.enemies) ->
        g <$ push (PlaceTokens source (toTarget e) token n)
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
                -- or progress on a location, as Assault on Osgiliath's Nightmare setup card reads
                onto = filter (inPlay g) [t | ProgressBeyondActiveTo t <- gameMods]
                canceledBy q = find ((== Just q) . sourceTarget) [s | QuestProgressCanceledBy s <- gameMods]
                onQuests k =
                  [ maybe (PlaceTokens GameSource q Progress k) (`ProgressCanceled` k) (canceledBy q)
                  | q <- quests
                  , k > 0
                  ]
            -- a location in the staging area may take it before the quest, as Explorer's
            -- Almanac's, the first player choosing
            almanacs <- if overflow > 0 && null instead then mayTakeQuestingProgress else pure []
            lead <- getLead
            pushAll
              $ concat onLocations
              <> case instead of
                t : _ -> [DealDamage GameSource t overflow | overflow > 0]
                []
                  | t : _ <- onto -> [PlaceTokens GameSource t Progress overflow | overflow > 0]
                  | null almanacs -> onQuests overflow
                  | otherwise ->
                      [ questionLabelWith (ikey "label.questingProgress") Nothing (map (toTarget . fst) almanacs) lead
                          $ [ targetLabel lid (PlaceTokens GameSource (toTarget lid) Progress k : onQuests (overflow - k))
                            | (lid, room) <- almanacs
                            , let k = min overflow room
                            ]
                          <> [Label (ikey "label.placeOnQuest") (onQuests overflow)]
                      ]
        pure g
  PayForCard pid card fromHand windows
    -- a Guarded (X) attachment is not attached until it is free of encounters
    | card.cardType == AttachmentType
    , not (any (\case GuardedBy _ -> True; _ -> False) (cdKeywords $ toCardDef card)) -> do
        -- a host that lowers the cost is chosen before paying
        hosts <- cardHosts pid (toCardCode card)
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
  EndEffects duration -> pure $ g & effectsL %~ filter ((/= duration) . effectDuration)
  AdditionalPhases phases -> pure $ g & additionalPhasesL %~ (phases <>)
  -- the round goes on from its own phase once the additional instances after it are done
  NextPhase phase -> case gameAdditionalPhases g of
    extra : rest -> do
      push $ Run [BeginPhase extra, EndPhase extra]
      pure $ g & additionalPhasesL .~ rest & resumeAfterL %~ (<|> Just phase)
    [] -> do
      push $ maybe EndRound (\next -> Run [BeginPhase next, EndPhase next]) (nextPhase $ fromMaybe phase (gameResumeAfter g))
      pure $ g & resumeAfterL .~ Nothing
  EndPhase phase -> do
    push $ CheckWindows [mkWhen $ PhaseEnds phase]
    -- an enemy still in play raises a player's threat, as Crazed Captive's shadow reads
    raising <- for (toList g.entities.enemies) \e -> (\ms -> [(p, n) | RaisesThreatAtEndOfPhase p n <- ms]) <$> getModifiers e
    pushAll $ concat [[RaiseThreat p n, CheckWindows [mkAfter $ ThreatRaisedByEncounter p n]] | (p, n) <- concat raising]
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
    -- an ally put into play until the end of the phase, as Command's, is discarded
    discarding <- flip filterM (toList g.entities.allies) \a ->
      elem DiscardAtEndOfPhase <$> getModifiers (CardIdTarget $ allyCardId $ toAttrs a)
    pushAll [Discard (toTarget a) | a <- discarding]
    returning <- flip filterM (filter ((`notElem` map toId discarding) . toId) $ toList g.entities.allies) \a ->
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
             [ Label (ikey "label.returnToOwnersHand") [ReturnToHand t]
             , Label (ikey "label.shuffleAllyIntoDeck") [ShuffleIntoOwnersDeck t]
             ]
         | t <- both
         ]
    -- separated, each area resolves the phase in turn before the next phase begins; the
    -- areas to join another do so at the beginning of the travel phase, and the token
    -- passes once every area has refreshed
    let following = NextPhase phase
        (step, g') =
          eachAreaThen
            (\status -> [BeginPhase phase | status == AreaWaiting] <> [EndPhase phase])
            (if phase == RefreshPhase then passToken else id)
            (if phase == QuestPhase then Run [JoinAreas, FocusArea (maybe g.firstPlayer separationFirstPlayer $ gameSeparation g), following] else following)
            g
        passToken s = s {separationFirstPlayer = fromMaybe s.separationFirstPlayer $ headMay $ drop 1 $ seatsFrom s.separationFirstPlayer g.playerOrder}
    pushEnd step
    pure
      $ g'
      & effectsL %~ filter ((`notElem` [UntilEndOfPhase, UntilEndOfPhaseOf phase]) . effectDuration)
      & leftPlayLocationsL .~ mempty
      & damageDealtL %~ filter (\(s, _, _) -> case s of EnemyAttackSource _ -> True; _ -> False)
  PlayAlly pid card -> do
    thisAlly <- entering =<< newAlly pid card
    push $ enteredPlayThisRound thisAlly
    push $ allyEntersPlay pid thisAlly
    pure
      $ g
      & entitiesL
      . alliesL
      %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  PutAllyIntoPlayUnder pid card -> do
    thisAlly <- entering =<< newAlly (fromMaybe pid card.owner) card
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    push $ enteredPlayThisRound thisAlly
    push $ allyEntersPlay pid thisAlly
    pure
      $ g
      & entitiesL
      . alliesL
      %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  -- attached as one played is, but not played
  -- with nothing it may attach to, it goes to its owner's discard pile, as an attachment
  -- freed of its guard does
  PutAttachmentIntoPlay pid card -> do
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    hosts <- cardHosts pid (toCardCode card)
    let unattached = attachMatcherFor pid (toCardCode card) `elem` [PlayIntoStagingArea, PlayIntoPlayArea]
    push $ if null hosts && not unattached && isNothing (guardedTypes card) then PutInDiscard card else PlayAttachment pid card
    pure g
  PutSideQuestIntoPlay pid card -> do
    sideQuest <- newSideQuest pid card
    pure $ g & entitiesL . questsL %~ insertEntity sideQuest
  PutHeroIntoPlay pid card -> do
    thisHero <- newHero pid card
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisHero]
    pure $ g & entitiesL . heroesL %~ insertEntity thisHero
  PutObjectiveHeroIntoPlayUnder pid card -> do
    thisHero <- lookupHero (toCardCode card) card.id <$> getRandom <*> pure pid
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
        passed = nextAttack . case [eid | Window After (EntersStagingArea (EnemyTarget eid)) <- windows] of
          eid : _ -> map \e -> case [m | NextEnemyInStaging m <- effectModifiers e] of
            [] -> e
            ms -> e {effectTarget = EnemyTarget eid, effectModifiers = ms}
          [] -> id
        -- an effect waiting for the character's next attack lasts for that attack once it is
        -- declared as an attacker
        attackers = [t | Window After (DeclaredAsAttacker t _) <- windows]
        nextAttack = map \e -> case [m | ForNextAttack m <- effectModifiers e] of
          ms@(_ : _) | effectTarget e `elem` attackers -> e {effectModifiers = ms, effectDuration = UntilEndOfAttack}
          _ -> e
    pure $ g & effectsL %~ passed . (if attackOver then filter ((/= UntilEndOfAttack) . effectDuration) else id)
  ResponseWindow windows order used passes
    | passes < length order
    , (pid : _) <- order -> do
        abilities <- getWindowAbilities windows
        own <-
          filterM
            (\ab -> (== Just pid) <$> abilityController ab)
            [ab | ab <- abilities, isResponseAbility ab, not ab.abilityAnyPlayer, abilityKey ab `notElem` used]
        -- one any player may trigger, as Azain Silverbeard's, is offered to each, read as
        -- the player it is offered to
        anyone <-
          filterM (withinLimitFor pid)
            . filter (\ab -> isResponseAbility ab && ab.abilityAnyPlayer && abilityKey ab `notElem` used)
            =<< asPlayer pid (getWindowAbilities windows)
        let available = own <> anyone
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
    when (cdCardType (toCardDef card) == AllyType) $ void $ runEach msg $ createAlly card (AllyId UUID.nil) pid
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
  PutOnBottomOfOwnersDeck target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll
      [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    pure $ g' & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . deckL %~ (<> [toCard a])
  -- an attachment returned to its owner's hand, as Bard son of Brand and Bartering return them
  ReturnToHand (AttachmentTarget aid) | Just a <- Map.lookup aid g.entities.attachments -> do
    let owner = (toAttrs a).owner
    pure $ g & entitiesL . attachmentsL %~ Map.delete aid & playersL . ix owner . handL %~ (<> [toCard a])
  ShuffleIntoOwnersDeck (AttachmentTarget aid) | Just a <- Map.lookup aid g.entities.attachments -> do
    let owner = (toAttrs a).owner
    deck <- shuffleM . (toCard a :) . (.deck) =<< getPlayer owner
    pure $ g & entitiesL . attachmentsL %~ Map.delete aid & playersL . ix owner . deckL .~ deck
  -- just before the attack's After windows, while what lasted for it still does
  AttackDefended _ eid defenders -> do
    undamaged <- filterM (fmap (notElem DamagedWhileDefending) . getModifiers) defenders
    readying <- elem ReadyUndamagedDefenders <$> getModifiers GameTarget
    pushAll [Ready d | readying, d <- undamaged, inPlay g d]
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
  -- a reanimated card leaves its owner's deck, hand or discard pile for the staging area,
  -- a Reanimated Dead; leaving play, it goes to its owner's discard pile as an Orc Guard does
  Reanimate cid eid
    | Just (owner, pc) <- headMay [(p.id, c) | p <- toList g.players, PlayerCard c <- p.deck <> p.hand <> p.discard, c.id == cid] -> do
        let dead = createEnemy (MkEncounterCard cid (toCardCode reanimatedDead)) eid
            without = filter ((/= cid) . (.id))
        push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget eid)]
        pure
          $ g
          & playersL
          . ix owner
          %~ ((deckL %~ without) . (handL %~ without) . (discardL %~ without))
          & entitiesL
          . enemiesL
          %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) dead)
          & scenarioL
          %~ overAttrs (orcGuardsL %~ Map.insert cid pc)
  -- an enemy put into play engaged enters play, as Haradrim Elite reads
  PutEnemyIntoPlayEngaged pid card -> do
    enemy <- newEnemy card
    pushAll [EngageEnemy pid enemy.id, CheckWindows [mkAfter $ EntersPlay $ toTarget enemy]]
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
  -- taken from the encounter discard pile, if it lies there, as From the Depths discards it
  GrappleCard card -> do
    enemy <- newEnemy card
    push $ ResolveGrapple enemy.id
    pure $ g & entitiesL . enemiesL %~ insertEntity enemy & scenarioL %~ overAttrs (encounterDiscardL %~ filter (/= card))
  ResolveGrapple eid | Just e <- Map.lookup eid g.entities.enemies -> do
    lead <- getLead
    let inPlay' l = l.placement `elem` [StagingArea, ActiveLocation]
        locations = filter inPlay' (toList g.entities.locations)
        picked = case [w | Grapple w <- toList (cdKeywords $ toCardDef e)] of
          w : _ -> do
            let grappling l = length [x | x <- toList g.entities.enemies, x.placement == AttachedTo (LocationTarget $ toId l)]
                value l
                  | w `elem` [MostEnemiesGrappled, FewestEnemiesGrappled] = grappling l
                  | otherwise = fromMaybe 0 $ printedFor w (toCardDef l)
                best = (if lowest w then minimumMay else maximumMay) (map value locations)
            [toId l | l <- locations, Just (value l) == best]
          _ -> []
        grapple lid = PlaceEnemy eid (AttachedTo $ LocationTarget lid)
    case picked of
      [] -> pushWhen (e.placement == Unplaced) $ PlaceEnemy eid StagingArea
      [lid] -> push $ grapple lid
      tied -> push $ questionLabelWith (ikey "label.grapple") (Just $ toTarget eid) [] lead [targetLabel lid [grapple lid] | lid <- tied]
    pure g
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
  PutObjectiveIntoPlayGuardedBy card cid -> do
    thisObjective <- newObjective card
    pushAll [SetObjectiveGuard (toId thisObjective) cid, CheckWindows [mkAfter $ EntersStagingArea (toTarget thisObjective)]]
    pure $ g & entitiesL . objectivesL %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  PutObjectiveIntoPlayAttached card host -> do
    thisObjective <- newObjective card
    pure $ g & entitiesL . objectivesL %~ insertEntity (overAttrs (\a -> a {objectivePlacement = AttachedTo host}) thisObjective)
  RemoveFromGame (ObjectiveTarget oid) -> pure $ g & entitiesL . objectivesL %~ Map.delete oid
  -- a treachery attached as a condition leaves play for good, as Daechanar removes a Sorcery card
  RemoveFromGame (TreacheryTarget tid) -> pure $ g & entitiesL . treacheriesL %~ Map.delete tid
  -- an enemy leaves play for good, as Change of Plans removes the Corsair Raiders
  -- an enemy leaves play for good, as Captain Sahír after his betrayal: what is on it is
  -- discarded
  RemoveFromGame target@(EnemyTarget eid) | Map.member eid g.entities.enemies -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . enemiesL %~ Map.delete eid
  -- a side quest removed from the game, as Stop the Executioners! removes itself
  RemoveFromGame target@(QuestTarget qid) | Just q <- Map.lookup qid g.entities.quests, isSideQuest q -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . questsL %~ Map.delete qid
  -- a hero leaves play for good, not for its owner's discard pile, its attachments
  -- discarded as they would be were it discarded
  SetAsideHero hid -> runGameMessage (RemoveFromGame $ HeroTarget hid) g
  RemoveFromGame target@(HeroTarget hid) | Map.member hid g.entities.heroes -> do
    g' <- leftPlay g target
    pushAll $ discardAttached g target
    pure $ g' & entitiesL . heroesL %~ Map.delete hid
  -- an ally leaves play for good, as Eagle of the North does however it leaves
  RemoveFromGame target@(AllyTarget aid) | Map.member aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll $ discardAttached g target
    pure $ g' & entitiesL . alliesL %~ Map.delete aid
  -- what is attached to it leaves play with it, as it would were it discarded
  RemoveFromGame target@(LocationTarget lid) -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . locationsL %~ Map.delete lid
  -- a location placed underneath a card leaves play, its tokens and attachments discarded;
  -- so does a character or an attachment captured from play (Escape from Mount Gram), and
  -- a treachery placed there as it resolves, as Closing In
  PlaceUnderneath _ cards -> do
    let under :: IsCard a => a -> Bool
        under c = (toCard c).id `elem` map (.id) cards
        gone = filter under (toList g.entities.locations)
        heroes = filter under (toList g.entities.heroes)
        allies = filter under (toList g.entities.allies)
        enemies = filter under (toList g.entities.enemies)
        characters = map toTarget heroes <> map toTarget allies
        on' :: HasField "placement" a Placement => a -> Bool
        on' a = a.placement `elem` map (AttachedTo . toTarget) gone
    g' <- foldM leftPlay g characters
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, on' a]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, on' t]
      <> [Discard (toTarget o) | o <- toList g.entities.objectives, on' o]
      <> concatMap (discardAttached g) characters
      <> [CheckEliminations | notNull heroes]
      -- an enemy placed underneath a location leaves play, as Murderous Turncoat
      <> concatMap (discardAttached g . toTarget) enemies
    pure
      $ g'
      & entitiesL . locationsL %~ (\ls -> foldr (Map.delete . toId) ls gone)
      & entitiesL . enemiesL %~ Map.filter (not . under)
      & entitiesL . heroesL %~ Map.filter (not . under)
      & entitiesL . alliesL %~ Map.filter (not . under)
      & entitiesL . attachmentsL %~ Map.filter (not . under)
      & entitiesL . treacheriesL %~ Map.filter (not . under)
  PutObjectiveIntoPlay card -> do
    thisObjective <- newObjective card
    let guarded = \case
          Guarded -> True
          GuardedBy _ -> True
          _ -> False
    pushWhen (any guarded $ cdKeywords (toCardDef card)) $ GuardObjective (toId thisObjective)
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
    pushAll $ CheckWindows [mkAfter $ QuestDefeated (QuestTarget qid)] : discardAttachedToQuest g qid
    -- "Limit 1 copy in the victory display": a copy defeated while one is there goes to its
    -- owner's discard pile (The Sands of Harad rules)
    let shown = any ((== toCardCode q) . toCardCode) (toAttrs g.scenario).victoryDisplay
    case toCard q of
      PlayerCard pc | shown && cdLimitOneInVictoryDisplay (toCardDef q) -> do
        push $ PutInDiscard pc
        pure $ g & entitiesL . questsL %~ Map.delete qid
      card -> pure $ g & entitiesL . questsL %~ Map.delete qid & scenarioL %~ overAttrs (victoryDisplayL %~ (card :))
  CompleteQuest qid -> do
    pushAll $ CheckWindows [mkAfter $ QuestDefeated (QuestTarget qid)] : discardAttachedToQuest g qid <> [AdvanceQuest]
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
      <> freeGuarded g target
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
      <> attachedEnemies g target
      <> [AddToEncounterDiscard e.shadowCards | notNull e.shadowCards]
      <> freeGuarded g target
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
  -- a treachery adding itself as it resolves, as An Evil Fortune does
  AddToVictoryDisplay (TreacheryTarget tid)
    | Just t <- Map.lookup tid g.entities.treacheries ->
        pure $ g & entitiesL . treacheriesL %~ Map.delete tid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard t :))
  -- an objective claimed into the victory display, as a Clue of The Steward's Fear
  AddToVictoryDisplay (ObjectiveTarget oid)
    | Just o <- Map.lookup oid g.entities.objectives ->
        pure $ g & entitiesL . objectivesL %~ Map.delete oid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard o :))
  MakeEscapeTest t -> do
    order <- getPlayerOrder
    own <- charactersControlledBy t.escapePlayer
    limits <- (\ms -> [k | EscapeTestCommitLimit k <- ms]) <$> getModifiers t.escapePlayer
    -- one with a limit, as Ill-fated Guard sets, commits no more once he has reached it
    let full = any (<= length (filter (`elem` own) t.escapeCommitted)) limits
    ready <-
      if full
        then pure []
        else
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
      $ [ targetLabel c [Exhaust c, CheckWindows [mkAfter $ CommittedToEscapeTest c], MakeEscapeTest t {escapeCommitted = t.escapeCommitted <> [c]}]
        | c <- ready
        ]
      <> [ Label (ikey "label.doneCommittingToEscape") $ case t.escapeParty of
             next : rest -> [MakeEscapeTest t {escapePlayer = next, escapeParty = rest}]
             -- the players may act once the characters are committed, before the cards
             -- are dealt, the test's only action window
             [] -> map OptionalActionWindow order <> [ResolveEscapeTest t]
         ]
    pure g
  ExhaustToReduceCost pid costId trait -> do
    healers <-
      exhaustableCharacters
        (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait trait)
        (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait trait)
    let owed = maybe 0 (staticCost . (.activeCostCost)) (Map.lookup costId (gameActiveCosts g))
    push
      $ if null healers || owed <= 0
        then PayCost costId
        else
          questionLabel (ikeyWith "label.exhaustToReduceCost" [intVar "count" owed]) pid
            $ [ targetLabel c [Exhaust c, ReduceActiveCost costId 1, ExhaustToReduceCost pid costId trait]
              | c <- healers
              ]
            <> [Label (ikey "label.payRemainingCost") [PayCost costId]]
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
  -- a Guarded (X) attachment is guarded before it is attached (MEC65 rules)
  PlayAttachment pid card | Just types <- guardedTypes card -> g <$ push (ResolveGuardedPlayerCard pid card types)
  PlayAttachment pid card -> playAttachment pid card g
  -- free of encounters, it attaches to an eligible card, or with none is discarded
  AttachFreedCard pid card -> do
    hosts <- cardHosts pid (toCardCode card)
    if null hosts then g <$ push (PutInDiscard card) else playAttachment pid card g
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
  CheckRestricted host -> g <$ checkRestricted g host
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
  -- the card keeps its id under the other card type, as its own text finds it by
  AllyBecomesHero aid | Just a <- Map.lookup aid g.entities.allies -> do
    let hid = HeroId (unAllyId aid)
        attrs = toAttrs a
        controller = fromMaybe attrs.allyOwner (placementController a.placement)
        new =
          overAttrs
            ( \h ->
                h
                  { heroTokens = allyTokens attrs
                  , heroExhausted = allyExhausted attrs
                  , heroCommitted = allyCommitted attrs
                  , heroAttacking = allyAttacking attrs
                  , heroFacedown = allyFacedown attrs
                  }
            )
            $ lookupHero (toCardCode a) attrs.allyCardId hid controller
    pure $ retargetCharacter (toTarget a) (toTarget new) $ g & entitiesL . alliesL %~ Map.delete aid & entitiesL . heroesL %~ insertEntity new
  HeroBecomesAlly hid | Just h <- Map.lookup hid g.entities.heroes, cdCardType (toCardDef h) == AllyType -> do
    let aid = AllyId (unHeroId hid)
        attrs = toAttrs h
        new =
          overAttrs
            ( \a ->
                a
                  { allyPlacement = InPlayArea attrs.heroOwner
                  , allyTokens = heroTokens attrs
                  , allyExhausted = heroExhausted attrs
                  , allyCommitted = heroCommitted attrs
                  , allyAttacking = heroAttacking attrs
                  , allyFacedown = heroFacedown attrs
                  }
            )
            $ lookupAlly (toCardCode h) attrs.heroCardId aid attrs.heroOwner
    pure $ retargetCharacter (toTarget h) (toTarget new) $ g & entitiesL . heroesL %~ Map.delete hid & entitiesL . alliesL %~ insertEntity new
  -- it does not leave play: it keeps its id, and whether it is exhausted, as the card it is
  AllyBecomesAttachment aid host | Just a <- Map.lookup aid g.entities.allies -> do
    let attrs = toAttrs a
        new =
          overAttrs (\x -> x {attachmentExhausted = allyExhausted attrs})
            $ lookupAttachment (toCardCode a) attrs.allyCardId (AttachmentId $ unAllyId aid) attrs.allyOwner
    pushAll $ discardAttached g (toTarget a) <> [AttachCard (toId new) host, CheckRestricted host]
    pure $ g & entitiesL . alliesL %~ Map.delete aid & entitiesL . attachmentsL %~ insertEntity new
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
        let grapples = [ResolveGrapple (toId enemy) | any isGrapple (cdKeywords def)]
        pushAll . (<> grapples <> [WhileInPlay (toTarget enemy) [AmbushEngagementCheck (toId enemy), ResolveSurge (toTarget enemy) card]])
          =<< revealing (toTarget enemy)
        pure
          $ g
          & entitiesL
          . enemiesL
          %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        -- a guarded one, as Across the Ettenmoors' objective-locations, is guarded by the
        -- next card before its surge
        let guarded = [GuardLocation (toId location) | Guarded `member` cdKeywords def]
        pushAll . (<> guarded <> [ResolveSurge (toTarget location) card]) =<< revealing (toTarget location)
        pure
          $ g
          & entitiesL
          . locationsL
          %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      TreacheryType -> do
        treachery <- newTreachery card
        -- counted as it is revealed, for "the first treachery card revealed each round"
        let counted = CreateEffect $ Effect (TreacherySource $ toId treachery) GameTarget [TreacheryRevealedThisRound] UntilEndOfRound
        pushAll . (counted :) . (<> [FinishRevelation (toId treachery), WhileInPlay (toTarget treachery) surge])
          =<< revealing (toTarget treachery)
        pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
      -- a player card with the Encounter keyword, as Ranger of the North: it enters play
      -- under the first player, its When Revealed effect giving it to a player
      AllyType | Just owner <- find (any ((== card.id) . (.id)) . (.setAsideCards)) (toList g.players) -> do
        lead <- getLead
        aid <- getRandom
        thisAlly <- entering $ overAttrs (\a -> a {allyPlacement = InPlayArea lead}) $ lookupAlly (toCardCode card) card.id aid owner.id
        -- it enters play as it is revealed, but whose it is is chosen as its When Revealed
        -- resolves, so the windows for it entering play open once that is done
        pushAll
          . (enteredPlayThisRound thisAlly :)
          . (<> [WhileInPlay (toTarget thisAlly) [AllyEnteredPlayUnderController aid, ResolveSurge (toTarget thisAlly) card]])
          =<< revealing (toTarget thisAlly)
        pure $ g & entitiesL . alliesL %~ insertEntity thisAlly & playersL . ix owner.id . setAsideCardsL %~ filter ((/= card.id) . (.id))
      -- a player event with the Encounter keyword, as Wind from the Sea: its When Revealed,
      -- which cannot be canceled (MEC61 rules), is resolved by the event, built from its card
      EventType | any (any ((== card.id) . (.id)) . (.setAsideCards)) (toList g.players) -> do
        pushAll =<< revealing (CardIdTarget card.id)
        pure g
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
  -- a set-aside player event with the Encounter keyword, revealed: it leaves play once
  -- resolved, and so is removed from the game (MEC61 rules)
  WhenRevealed (CardIdTarget cid) | Just owner <- find (any ((== cid) . (.id)) . (.setAsideCards)) (toList g.players) -> do
    for_ [c | c <- owner.setAsideCards, c.id == cid] \c -> for_ (lookupEvent (toCardCode c) cid owner.id) $ void . runEach msg
    pure $ g & playersL . ix owner.id . setAsideCardsL %~ filter ((/= cid) . (.id))
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
    pushAll $ freeGuarded g (TreacheryTarget tid)
    -- its effects have resolved or been canceled, so it guards nothing any more
    let cid = (.cardId) . toAttrs <$> Map.lookup tid g.entities.treacheries
        guarding o = isJust cid && (toAttrs o).objectiveGuard == cid
    pure
      $ g
      & entitiesL
      . objectivesL
      . traverse
      %~ (\o -> if guarding o then overAttrs (\a -> a {objectiveGuard = Nothing}) o else o)
  -- a card out of play comes into play as a captive: see 'GuardCard'
  -- an ally card no longer in its owner's discard pile, taken already, stays where it is
  GuardCard card cid -> do
    aid <- getRandom
    lead <- getLead
    let (owner, available) = case card of
          PlayerCard pc -> case pc.owner of
            Just o -> (o, maybe False (any ((== pc.id) . (.id)) . (.discard)) (Map.lookup o g.players))
            Nothing -> (lead, False)
          EncounterCard _ -> (lead, True)
        captive = overAttrs (\a -> a {allyPlacement = AttachedTo (CardIdTarget cid)}) $ lookupAlly (toCardCode card) card.id aid owner
    if available
      then do
        pushAll [RemoveFromDiscard owner card.id | PlayerCard _ <- [card]]
        pure $ g & entitiesL . alliesL %~ insertEntity captive
      else pure g
  -- as a Ship objective of the fleet comes into play (Dream-chaser rules)
  PutObjectiveAllyIntoPlayUnder pid card -> do
    aid <- getRandom
    thisAlly <- entering $ overAttrs (\a -> a {allyPlacement = InPlayArea pid}) $ lookupAlly (toCardCode card) card.id aid pid
    push $ enteredPlayThisRound thisAlly
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity thisAlly
  -- a card whose other side is another kind of card turns over, as Captain Sahír from enemy
  -- to objective-ally and back: it does not leave play, but what was on it is discarded and
  -- its tokens go, and the side it turns to is under the player that side names
  Flip (EnemyTarget eid)
    | Just e <- Map.lookup eid g.entities.enemies
    , Just other <- cdOtherSide (toCardDef e)
    , cdCardType (toCardDef other) == ObjectiveAllyType -> do
        pid <- objectiveAllyController (toCardDef other)
        pushAll $ discardAttached g (EnemyTarget eid)
        let thisAlly = overAttrs (\a -> a {allyPlacement = InPlayArea pid}) $ lookupAlly other (toCard e).id (AllyId $ unEnemyId eid) pid
        pure $ g & entitiesL . enemiesL %~ Map.delete eid & entitiesL . alliesL %~ insertEntity thisAlly
  -- and back to its enemy side, in the staging area
  Flip (AllyTarget aid)
    | Just a <- Map.lookup aid g.entities.allies
    , Just other <- cdOtherSide (toCardDef a)
    , cdCardType (toCardDef other) == EnemyType -> do
        pushAll $ discardAttached g (AllyTarget aid)
        let thisEnemy = overAttrs (\e -> e {enemyPlacement = StagingArea}) $ lookupEnemy other (toCard a).id (EnemyId $ unAllyId aid)
        pure $ g & entitiesL . alliesL %~ Map.delete aid & entitiesL . enemiesL %~ insertEntity thisEnemy
  -- a location whose other side is an objective ally, as Woodmen Village's Haldan, turns
  -- over out of the locations: the effect turning it puts that side where it says, under
  -- the location's card id
  Flip target@(LocationTarget lid)
    | Just l <- Map.lookup lid g.entities.locations
    , Just other <- cdOtherSide (toCardDef l)
    , cdCardType (toCardDef other) == ObjectiveAllyType -> do
        pushAll $ discardAttached g target
        pure $ g & entitiesL . locationsL %~ Map.delete lid
  -- a double-sided location turns over, as an Uncharted location does
  Flip (LocationTarget lid) | Just l <- turnLocationOver =<< Map.lookup lid g.entities.locations -> do
    pushAll $ [ArrangeIslandMap | isJust l.position] <> [CheckWindows [mkAfter $ LocationFlipped lid]]
    pure $ g & entitiesL . locationsL %~ Map.insert lid l
  -- as Shrine to Morgoth floods: turned over, its progress back on the side it turns to
  FlipKeepingProgress lid | Just l <- Map.lookup lid g.entities.locations, Just l' <- turnLocationOver l -> do
    push $ CheckWindows [mkAfter $ LocationFlipped lid]
    let progress = countTokens Progress (locationTokens $ toAttrs l)
    pure $ g & entitiesL . locationsL %~ Map.insert lid (overAttrs (\a -> a {locationTokens = addTokens Progress progress (locationTokens a)}) l')
  AddFromGrottoDeck n -> do
    let (top, rest) = splitAt n (toAttrs g.scenario).grottoDeck
    pushAll $ map AddToStaging top
    pure $ g & scenarioL %~ overAttrs (grottoDeckL .~ rest)
  -- the card enters play with its other side faceup
  AddUnderwaterFromGrottoDeck -> case (toAttrs g.scenario).grottoDeck of
    top : rest -> do
      push $ AddToStaging $ maybe top (MkEncounterCard top.id) (cdOtherSide $ toCardDef top)
      pure $ g & scenarioL %~ overAttrs (grottoDeckL .~ rest)
    [] -> pure g
  PutOnBottomOfGrottoDeck card -> do
    let def = toCardDef card
        grotto = case cdOtherSide def of
          Just other | Grotto `notMember` cdCardTraits def -> MkEncounterCard card.id other
          _ -> card
    pure $ g & scenarioL %~ overAttrs (grottoDeckL %~ (<> [grotto]))
  ShuffleIntoGrottoDeck lid | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll $ discardAttached g (LocationTarget lid)
    deck <- shuffleM (grottoDeckCard (toAttrs l) : (toAttrs g.scenario).grottoDeck)
    pure $ g & entitiesL . locationsL %~ Map.delete lid & scenarioL %~ overAttrs (grottoDeckL .~ deck)
  LookAtFacedownSide pid lid ->
    pure $ g & entitiesL . locationsL . ix lid %~ overAttrs (\a -> a {locationLookedAt = ordNub (pid : a.lookedAt)})
  AddFromUnchartedDeck n -> do
    let (top, rest) = splitAt n (toAttrs g.scenario).unchartedDeck
    pushAll $ map AddToStaging top
    pure $ g & scenarioL %~ overAttrs (unchartedDeckL .~ rest)
  ShuffleIntoUnchartedDeck lid | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll $ discardAttached g (LocationTarget lid)
    deck <- shuffleM (locationDeckCard (toAttrs l) : (toAttrs g.scenario).unchartedDeck)
    pure $ g & entitiesL . locationsL %~ Map.delete lid & scenarioL %~ overAttrs (unchartedDeckL .~ deck)
  -- each keeps its place in the staging area, but the cards are dealt among them afresh,
  -- each with a new card id, so nothing seen of one follows its card
  ShuffleUnchartedLocations -> do
    let staged = [l | l <- toList g.entities.locations, l.placement == StagingArea, isUncharted l]
    codes <- shuffleM $ map (toCardCode . locationDeckCard . toAttrs) staged
    dealt <- for (zip staged codes) \(l, code) -> do
      cid <- getRandom
      pure $ overAttrs (\a -> a {locationPlacement = StagingArea, locationPosition = l.position}) $ enterFaceUp $ createLocation (MkEncounterCard cid code) (toId l)
    pure $ g & entitiesL . locationsL %~ \ls -> foldr (\l -> Map.insert (toId l) l) ls dealt
  PrepareIslandMap cards -> do
    laid <- for cards \(pos, card) -> overAttrs (\a -> a {locationPlacement = OnIslandMap, locationPosition = Just pos}) <$> newLocation card
    pure $ g & entitiesL . locationsL %~ \ls -> foldr (\l -> Map.insert (toId l) l) ls laid
  SwapIslandMapPositions a b
    | Just la <- Map.lookup a g.entities.locations
    , Just lb <- Map.lookup b g.entities.locations -> do
        push ArrangeIslandMap
        let moveTo l = overAttrs (\x -> x {locationPosition = l.position})
        pure $ g & entitiesL . locationsL %~ Map.adjust (moveTo lb) a . Map.adjust (moveTo la) b
  ArrangeIslandMap -> do
    let laid = filter (isJust . (.position)) (toList g.entities.locations)
        actives = [p | l <- laid, l.placement == ActiveLocation, Just p <- [l.position]]
        aroundActive l = maybe False (\p -> any ((p `elem`) . adjacentPositions) actives) l.position
    placements <- for laid \l -> do
      excluded <- elem NotInStagingArea <$> getModifiers (toTarget l)
      pure
        ( toId l
        , if
            | l.placement == ActiveLocation -> ActiveLocation
            | aroundActive l && not excluded -> StagingArea
            | otherwise -> OnIslandMap
        )
    pure $ g & entitiesL . locationsL %~ \ls -> foldr (\(lid, p) -> Map.adjust (overAttrs (\a -> a {locationPlacement = p})) lid) ls placements
  PutObjectiveAllyIntoPlay oid pid | Just o <- Map.lookup oid g.entities.objectives -> do
    thisAlly <-
      entering
        -- the damage it took in the staging area stays on it, as Celador's
        $ overAttrs (\a -> a {allyPlacement = InPlayArea pid, allyTokens = addTokens Damage (countTokens Damage $ objectiveTokens $ toAttrs o) mempty})
        $ lookupAlly (toCardCode o) (toCard o).id (AllyId $ unObjectiveId oid) pid
    push $ enteredPlayThisRound thisAlly
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . objectivesL %~ Map.delete oid & entitiesL . alliesL %~ insertEntity thisAlly
  ShuffleIntoEncounterDeck target@(LocationTarget lid) | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target] <> freeGuarded g target
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
      <> attachedEnemies g target
      <> [AddToEncounterDiscard e.shadowCards | notNull e.shadowCards]
      <> freeGuarded g target
    let card = MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e)
        removed = g & entitiesL . enemiesL %~ Map.delete eid
    toPlayersDeck <- elem LeavesPlayToAnyPlayersDeck <$> getModifiers target
    if toPlayersDeck
      then removed <$ toAnyPlayersDeck g card
      else do
        deck <- shuffleM (card : (toAttrs g.scenario).encounterDeck)
        pure $ removed & scenarioL %~ overAttrs (encounterDeckL .~ deck)
  -- an encounter card drawn into a hand is answered by its card, out of play, as Recurring
  -- Nightmare's Forced effect (as Arkham's drawn encounter cards resolve their revelations)
  EncounterCardEnteredHand _ card | cdCardType (toCardDef card) == EnemyType -> do
    _ <- runEach msg . createEnemy card =<< getRandom
    pure g
  AddToStaging card | gainsController (toCardDef card) -> do
    thisObjective <- newObjective card
    controller <- objectiveAllyController (toCardDef card)
    push $ PutObjectiveAllyIntoPlay (toId thisObjective) controller
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
          $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
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
        PlayerCard pc
          | AllyTarget _ <- target
          , Just owner <- pcOwner pc -> do
              toHand <- elem AlliesLeavePlayToHand <$> getModifiers GameTarget
              pure
                $ if toHand
                  then removed & playersL . ix owner . handL %~ (<> [PlayerCard pc])
                  else addToDiscard (PlayerCard pc) removed
        card -> pure $ addToDiscard card removed
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
  [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
    <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
    <> [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target]
    <> attachedEnemies g target
    <> discardShadowsOf g target
    <> freeGuarded g target

{- | An enemy attached to the host, as Grasping Tentacle to a character, is discarded as its
host leaves play. One attached to an enemy is a Mount, as Mordor Warg and Racing Warg are,
whose "Forced: When attached enemy leaves play, add it to the staging area" is resolved
here: it goes to the staging area instead.
-}
attachedEnemies :: Game -> Target -> [Message]
attachedEnemies g target =
  [ case target of
      EnemyTarget _ -> PlaceEnemy (toId e) StagingArea
      _ -> Discard (toTarget e)
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
them, and again until 2 are left; a card may count as 2, as Raiment of War.
-}
checkRestricted :: Game -> Target -> GameT ()
checkRestricted g host = do
  let slots :: HasCardDef a => a -> Int
      slots = cdRestrictedSlots . toCardDef
      restrictedOn =
        [(toTarget x, slots x) | x <- toList g.entities.attachments, x.placement == AttachedTo host, isRestricted x || gainsRestricted x]
          <> [(toTarget o, slots o) | o <- toList g.entities.objectives, o.placement == AttachedTo host, isRestricted o]
      -- an attachment given Restricted with its text, as Wild Stallion's Mount text
      gainsRestricted x = GainKeyword Restricted `elem` map modifierType (Map.findWithDefault [] (toTarget x) (gameModifiers g))
  controller <- characterController host
  for_ controller \pid ->
    when (sum (map snd restrictedOn) > 2)
      $ push
      $ questionLabelWith
        (ikey "label.discardRestricted")
        (Just host)
        []
        pid
        [targetLabel x [Discard x, CheckRestricted host] | (x, _) <- restrictedOn]

-- | The ally as it enters play: exhausted while allies enter play so, as Flight to Gondor
-- has them.
entering :: Ally -> GameT Ally
entering a = do
  exhausted <- elem AlliesEnterPlayExhausted <$> getModifiers GameTarget
  pure $ if exhausted then overAttrs (\x -> x {allyExhausted = True}) a else a

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
-- | The player assigns the next point of the damage to one of the characters, or, with all
-- assigned or none left to take it, it is dealt.
divideDamage :: Source -> PlayerId -> Int -> [Target] -> [Target] -> (Int -> [Target] -> Message) -> GameT ()
divideDamage source pid n assigned characters next
  | n > 0 && notNull characters =
      push
        $ questionLabelWith (ikeyWith "label.dealDamageDivided" [intVar "n" n]) (sourceTarget source) [] pid
          [targetLabel c [next (n - 1) (c : assigned)] | c <- characters]
  | otherwise = pushAll $ dealAssigned source assigned

dealAssigned :: Source -> [Target] -> [Message]
dealAssigned source assigned = [DealDamage source c k | (c, k) <- Map.toList (Map.fromListWith (+) [(c, 1) | c <- assigned])]

-- | Enemies engaged with a player, highest engagement cost first.
engagedByCost :: PlayerId -> GameT [EnemyId]
engagedByCost pid = byCost =<< select (EnemyEngagedWith $ PlayerWithId pid)

byCost :: [EnemyId] -> GameT [EnemyId]
byCost enemies = do
  costs <- traverse getEngagementCost enemies
  pure $ map fst $ sortOn (Down . snd) $ zip enemies costs

-- | The enemies that attack the player in the combat phase, by engagement cost: those
-- engaged with him, and those in the staging area that attack him as if engaged.
attacksOf :: PlayerId -> GameT [EnemyId]
attacksOf pid = byCost . ordNub =<< (<>) <$> select (EnemyEngagedWith $ PlayerWithId pid) <*> attacksAsIfEngaged pid

-- | The enemies the players may engage from the staging area: those there, not one only
-- considered to be, as Tide-runner engaged or in the Stormcaller's area.
engageableFromStaging :: GameT [EnemyId]
engageableFromStaging = filterM (fmap ((== StagingArea) . (.placement)) . getEnemy) =<< select EnemyInStagingArea

-- | The enemies in the staging area that attack the player as if engaged, as Snow-Giant.
attacksAsIfEngaged :: PlayerId -> GameT [EnemyId]
attacksAsIfEngaged pid = filterM asIf =<< select EnemyInStagingArea
 where
  asIf e = getModifiers e >>= \ms -> anyM (\m -> selectAny (PlayerWithId pid <> m)) [m | AttacksAsIfEngagedWith m <- ms]

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
  -- a player event with the Encounter keyword, as Wind from the Sea, built for its owner
  EventType -> setAsideOwner card.id >>= traverse_ \owner -> for_ (lookupEvent (toCardCode card) card.id owner) $ void . runEach msg
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
      excessToQuests = ExcessDamageRemovesQuestProgress `elem` mods
      excessToShip = ExcessDamageToShipObjective `elem` mods
      -- a card it goes to, as Dense Thickets' shadow sends it to a Ranger objective
      excessTo = [t | ExcessDamageTo t <- mods, t /= target]
      excessToAflame = ExcessDamageToAflameLocation `elem` mods
      excessToDeck = ExcessDamageDiscardsFromDeck `elem` mods
  -- what is dealt past the character's remaining hit points (an attack turned on a
  -- location, as a Battleground takes it, has none)
  let character = case target of
        HeroTarget _ -> True
        AllyTarget _ -> True
        _ -> False
  excess <-
    if character
      then (\remaining -> max 0 (n - remaining)) <$> ((-) <$> getHitPoints target <*> getDamage target)
      else pure 0
  active <- select IsActiveLocation
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  es <- getsGame gameEntities
  ships <- filter (/= target) . map toTarget <$> shipObjectivesOf pid
  let toShip =
        [ chooseOrRunOne pid [targetLabel s [DealDamage (EnemyAttackSource eid) s excess] | s <- ships]
        | excessToShip
        , excess > 0
        , notNull ships
        ]
  -- each point on an Aflame location of the player's choice, so a location the first
  -- points destroy takes no more
  let toAflame = replicate (if excessToAflame then excess else 0) (DamageAflameLocation (EnemyAttackSource eid) pid)
  let captured = case target of
        HeroTarget h -> toCard <$> Map.lookup h es.heroes
        AllyTarget a -> toCard <$> Map.lookup a es.allies
        _ -> Nothing
  pure $ case captured of
    -- it captures the character instead, as Cruel Torturer reads
    Just card | CaptureInsteadOfAttackDamage `elem` mods, n > 0 -> [PlaceUnderneath (EnemyTarget eid) [card]]
    _ ->
      DealDamage (EnemyAttackSource eid) target n
        : concat [[RaiseThreat pid excess, CheckWindows [mkAfter $ ThreatRaisedByEncounter pid excess]] | excessToThreat, excess > 0]
        <> replicate (if excessToOthers then excess else 0) (DamageAnotherCharacter (EnemyAttackSource eid) pid target)
        <> concat [[PlaceTokens (EnemyAttackSource eid) (LocationTarget l) Damage excess, CheckDefeated (EnemyAttackSource eid) (LocationTarget l)] | excessToLocation, excess > 0, l <- active]
        <> [RemoveTokens (EnemyAttackSource eid) (toTarget q) Progress (min excess q.progress) | excessToQuests, excess > 0, q <- quests, q.progress > 0]
        <> toShip
        <> [chooseOrRunOne pid [targetLabel t [DealDamage (EnemyAttackSource eid) t excess] | t <- excessTo] | notNull excessTo, excess > 0]
        <> toAflame
        <> [DiscardTopOfDeck pid excess | excessToDeck, excess > 0]
        <> [CheckWindows [mkAfter $ AttackDealtExcessDamage eid target excess] | excess > 0]

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
  -- none of them, as Umbar Captain bars them from Ship enemies
  let barred = CannotBeAttackedByCharactersOf pid `elem` mods
  filter (\c -> not barred && c `notElem` attacking)
    . ordNub
    <$> ( filterM (canAttack eid)
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
  mods <- getModifiers eid
  others <- if engaged then filter (\p -> p /= pid && CannotBeAttackedByCharactersOf p `notElem` mods) <$> getPlayerOrder else pure []
  ranged <- filterM (canAttack eid) =<< withKeyword Ranged . concat =<< traverse readyAttackers others
  -- none with a keyword it bars, as Pitch-dark Thicket's shadow bars ranged
  let barred = [k | CannotBeAttackedByKeyword k <- mods]
  filterM (\c -> characterCardDef c >>= \def -> not <$> anyM (\k -> hasKeyword k c def) barred) (own <> ranged)

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
  cmods <- getModifiers c
  def <- characterCardDef c
  andM
    [ -- a Ship objective fights only Ship enemies (Dream-chaser rules)
      orM [not <$> isShipObjective c, selectAny (EnemyWithId eid <> shipEnemy)]
    , -- and only a Ship objective fights one such as Raider Flagship
      if OnlyAttackedByShipObjectives `elem` mods then isShipObjective c else pure True
    , allM (\(t, k) -> orM [hasTrait t c def, hasKeyword k c def]) [(t, k) | OnlyFoughtBy t k <- mods]
    , allM (\k -> hasKeyword k c def) [k | OnlyFoughtByKeyword k <- mods]
    , if NotFoughtByDamaged `elem` mods then (== 0) <$> getDamage c else pure True
    , barredBy [m | CannotFight m <- cmods]
    , case c of
        HeroTarget hid | NotFoughtByHeroesWithoutResources `elem` mods -> (> 0) . (.resources) <$> getHero hid
        _ -> pure True
    ]
 where
  barredBy ms = not <$> anyM (\m -> selectAny (EnemyWithId eid <> m)) ms

-- | Whether the character may attack the enemy: as 'canFight', unless it cannot attack it.
canAttack :: EnemyId -> Target -> GameT Bool
canAttack eid c = do
  barred <- (\ms -> anyM (\m -> selectAny (EnemyWithId eid <> m)) [m | CannotAttackMatching m <- ms]) =<< getModifiers c
  full <- (<= 0) . fromMaybe 1 <$> attackerRoom eid
  if barred || full then pure False else canFight eid c

-- | How many more characters may attack the enemy this round, when it is limited, as Graxar.
attackerRoom :: EnemyId -> GameT (Maybe Int)
attackerRoom eid = do
  mods <- getModifiers eid
  pure $ subtract (length [() | DeclaredAttackerThisRound <- mods]) <$> minimumMay [n | AttackersPerRoundLimit n <- mods]

-- | The character is declared as an attacker against the enemy.
declaredAttacker :: Target -> EnemyId -> [Message]
declaredAttacker a eid =
  [ CreateEffect $ Effect GameSource (EnemyTarget eid) [DeclaredAttackerThisRound] UntilEndOfRound
  , CheckWindows [mkAfter $ DeclaredAsAttacker a eid]
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
  -- only a Ship objective defends against a Ship enemy (Dream-chaser rules)
  shipOnly <- andM [selectAny (EnemyWithId eid <> shipEnemy), not <$> isShipObjective c]
  -- nor one that cannot defend against it, as an ally against Uruk of Mordor
  barred <- (\ms -> anyM (\m -> selectAny (EnemyWithId eid <> m)) [m | CannotDefendAgainst m <- ms]) =<< getModifiers c
  if (noAllies && isAlly) || exhausts || shipOnly || barred then pure False else canFight eid c

-- | Whether the character is a Ship objective (Dream-chaser rules).
isShipObjective :: Target -> GameT Bool
isShipObjective = \case
  AllyTarget aid -> selectAny (AllyWithId aid <> shipObjective)
  _ -> pure False

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

-- | The encounter cards attached to the stage as conditions, as Old Hatreds, leave play with it.
discardAttachedToQuest :: Game -> QuestId -> [Message]
discardAttachedToQuest g qid =
  [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo (QuestTarget qid)]
    <> [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo (QuestTarget qid)]
    <> freeGuarded g (QuestTarget qid)

-- | What was attached to, or lasting on, a character is on the one it became.
retargetCharacter :: Target -> Target -> Game -> Game
retargetCharacter old new g =
  g
    & entitiesL . attachmentsL . traverse %~ (\a -> if a.placement == AttachedTo old then overAttrs (\x -> x {attachmentPlacement = AttachedTo new}) a else a)
    & entitiesL . treacheriesL . traverse %~ (\t -> if t.placement == AttachedTo old then overAttrs (\x -> x {treacheryPlacement = AttachedTo new}) t else t)
    & entitiesL . objectivesL . traverse %~ (\o -> if o.placement == AttachedTo old then overAttrs (\x -> x {objectivePlacement = AttachedTo new}) o else o)
    & effectsL . traverse %~ (\e -> if effectTarget e == old then e {effectTarget = new} else e)

-- | Whether the source is a player's attack on an enemy, its damage.
isPlayerAttack :: Source -> Bool
isPlayerAttack = \case
  PlayerAttackSource _ -> True
  _ -> False

-- | Whether a character is still in play.
-- | Where progress placed on the card goes instead, as Blinding Blizzard takes it.
progressRedirect :: Game -> Target -> Maybe Target
progressRedirect g target =
  find
    (\t -> t /= target && inPlay g t)
    [t | ProgressPlacedOnInstead t <- map modifierType (Map.findWithDefault [] target (gameModifiers g))]

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
                <> [cardMatches m card | NextIgnoresResourceMatch m <- effectModifiers e]
                <> [fromHand | NextCardGainsDoomed _ <- effectModifiers e]
            )
      spentEffects = filter spent (gameEffects g)
      -- one that does not require a resource match, as King of Dale's ally, is paid as neutral
      unmatched = or [cardMatches m card | e <- spentEffects, NextIgnoresResourceMatch m <- effectModifiers e]
      asPaid = \case
        LordOfTheRings.ActiveCost.ResourceCost _ c | unmatched -> LordOfTheRings.ActiveCost.ResourceCost Neutral c
        c -> c
  let activeCost =
        ActiveCost
          { activeCostId = costId
          , activeCostCost = raiseCost extra (asPaid $ cardCost card)
          , activeCostCard = card
          , activeCostPlayer = pid
          , activeCostWindows = windows
          , activeCostHost = host
          , activeCostFromHand = fromHand
          , activeCostDoomed = sum [n | e <- spentEffects, NextCardGainsDoomed n <- effectModifiers e]
          , activeCostPaidFrom = []
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
                    [ SpendResourcesFrom pid [(toTarget aid, k)]
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
    [] -> case [t | Modifier _ (ReducedByExhausting t) <- ownMods] of
      -- one that costs less for characters exhausted, as The Houses of Healing, asks first
      t : _ -> ExhaustToReduceCost pid costId t
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
  pushAll
    [ CreateEffect $ Effect (effectSource e) (CardIdTarget card.id) ms UntilEndOfRound
    | e <- spentEffects
    , SpentOnCardGives ms <- effectModifiers e
    ]
  let gained e = effectSource e == CardIdSource card.id && effectTarget e == CardIdTarget card.id
  pure $ g & activeCostsL %~ insertEntity activeCost & effectsL %~ filter (\e -> not (spent e || gained e))

-- | The Doomed a card is printed with.
printedDoomed :: HasCardDef a => a -> Int
printedDoomed a = sum [n | Doomed n <- toList (cdKeywords $ toCardDef a)]

-- | The player an optional engagement asks, and whether it is the first he is asked.
optionalEngagement :: Message -> Maybe (PlayerId, Bool)
optionalEngagement = \case
  OptionalEngagement pid -> Just (pid, True)
  DoStep 1 (OptionalEngagement pid) -> Just (pid, False)
  _ -> Nothing

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

-- | The locations in the staging area that may take questing progress before the quest,
-- as Explorer's Almanac's, with the progress each still takes.
mayTakeQuestingProgress :: GameT [(LocationId, Int)]
mayTakeQuestingProgress = do
  staged <- select LocationInStagingArea
  flip mapMaybeM staged \lid -> do
    may <- elem MayTakeQuestingProgress <$> getModifiers lid
    l <- getLocation lid
    room <- subtract l.progress <$> getLocationQuestPoints lid
    pure $ guard (may && room > 0) $> (lid, room)

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
