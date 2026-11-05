module LordOfTheRings.Game.Loop (module LordOfTheRings.Game.Loop) where

import Data.Map.Strict qualified as Map (
  delete,
  lookup,
  singleton,
  toList,
 )
import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Classes.Entity (Entity (toAttrs, toId))
import LordOfTheRings.Classes.HasGame (
  HasGame (getGame),
  getsGame,
 )
import LordOfTheRings.Classes.HasQueue (
  HasQueue (pushAll),
  peekQueue,
  popMessage,
  push,
  setQueue,
 )
import LordOfTheRings.Game.Base (
  Game,
  questionL,
  questionVersionL,
 )
import LordOfTheRings.GameT (GameT, debug, putGame)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (
  getModifiers,
  preloadModifiers,
 )
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Log.Narrator (narrationFor)
import LordOfTheRings.Log.Refs (sendLogInOpenBlock)
import LordOfTheRings.Matcher (
  AllyMatcher (AnyAlly),
  HeroMatcher (AnyHero, HeroControlledBy),
  PlayerMatcher (PlayerWithId),
  getLead,
  selectAny,
  selectCharacters,
 )
import LordOfTheRings.Message (
  Message (
    AddToVictoryDisplay,
    Ask,
    ChooseAndDiscardFromHand,
    ClaimFreeObjective,
    ClaimObjective,
    Discard,
    DiscardFromHand,
    DiscardTopOfEncounterDeck,
    DiscardedTopOfEncounterDeck,
    GuardObjective,
    LeavePlay,
    SetObjectiveGuard
  ),
  uiToRun,
 )
import LordOfTheRings.Modifier (
  ModifierType (
    CannotDiscardFromHand,
    DiscardedAtNoWillpower,
    FirstPlayerClaimsWhenFree,
    HandSizeLimit
  ),
 )
import LordOfTheRings.Objective (ObjectiveAttrs (objectiveGuard))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Player.Runner (discardableHand)
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Question (
  Question (ChooseOneAtATime, QuestionLabel, SearchCards),
  questionChoices,
  questionRebuild,
 )
import LordOfTheRings.Source (
  Source (GameSource, ObjectiveSource),
 )
import LordOfTheRings.Target (
  Target (ObjectiveTarget, PlayerTarget),
  Targetable (toTarget),
 )

import LordOfTheRings.Game.End (
  checkUnblockedQuests,
  endWaitingGame,
  gameEnding,
 )
import LordOfTheRings.Game.Modifiers (
  gainedAbilities,
  withHandModifiers,
 )
import LordOfTheRings.Game.Query (getPlayers)
import LordOfTheRings.Game.Runner (runGame)

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
      putGame . gainedAbilities =<< withHandModifiers =<< preloadModifiers g
      checkUnblockedQuests g
      discardAtNoWillpower
      discardDownToHandLimit
      claimFreeObjectives
      endWaitingGame
      pure True

{- | A player with more cards in hand than he may have, as In Enemy Territory allows him
5, chooses and discards cards down to it at once.
-}
discardDownToHandLimit :: GameT ()
discardDownToHandLimit = do
  over <- getsGame gameEnding
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
  unless (gameEnding g) do
    queued <- peekQueue
    let guards = map ((.id) . toCard) (toList g.entities.enemies) <> map ((.id) . toCard) (toList g.entities.locations)
        free o = o.placement == StagingArea && maybe True (`notElem` guards) (objectiveGuard $ toAttrs o)
        claiming = flip any queued \case
          ClaimFreeObjective _ -> True
          ClaimObjective {} -> True
          -- one claimed into the victory display, as a free Dragon Sign
          AddToVictoryDisplay (ObjectiveTarget _) -> True
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
  over <- getsGame gameEnding
  unless over do
    characters <- selectCharacters AnyHero AnyAlly
    doomed <- flip filterM characters \c -> andM [elem DiscardedAtNoWillpower <$> getModifiers c, (<= 0) <$> getWillpower c]
    queued <- peekQueue
    pushAll [Discard c | c <- doomed, Discard c `notElem` queued, LeavePlay (Discard c) `notElem` queued]

-- | Each hero's gained abilities, kept in step with its 'GainGameTextOf' modifiers.
