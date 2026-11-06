-- | Questions built the way they read: the choices are actions run in a queue of their
-- own, and the messages they push become the choice's.
--
-- > chooseOneM pid do
-- >   questionLabeled "gandalf.chooseOne"
-- >   causedBy attrs
-- >   labeled "gandalf.draw" $ drawCards attrs pid 3
-- >   targets enemies \e -> dealDamage attrs e 4
module LordOfTheRings.Message.Lifted.Choose (module LordOfTheRings.Message.Lifted.Choose) where

import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import LordOfTheRings.Card (Card)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Helpers.Modifiers (fellowshipCard, getModifiers, immuneToEncounterCard, isMasterCard, objectiveAllyTarget)
import LordOfTheRings.I18n
import LordOfTheRings.Modifier (ModifierType (ImmuneToNonFellowshipPlayerCardEffects, ImmuneToNonMasterCardEffects, ImmuneToPlayerCardEffects, ImmuneToPlayerEvents))
import LordOfTheRings.Id
import LordOfTheRings.Matcher (PlayerMatcher (PlayerWithId, PlayerAtValourThreat), getMainQuestProgress, getMainQuests, selectAny)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted.Queue
import LordOfTheRings.Prelude
import LordOfTheRings.Queue
import LordOfTheRings.Question
import LordOfTheRings.Source (Sourceable (..))
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Progress))

data ChooseState = ChooseState
  { chooseLabel :: Maybe Text
  , chooseSource :: Maybe Target
  , chooseTargets :: [Target]
  , chooseCards :: [Card]
  }

newtype ChooseT m a = ChooseT {unChooseT :: StateT ChooseState (WriterT [UI Message] m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ChooseT where
  lift = ChooseT . lift . lift

instance (HasGame m) => HasGame (ChooseT m) where
  getGame = lift getGame

runChooseT :: (Monad m) => ChooseT m a -> m (ChooseState, [UI Message])
runChooseT body = do
  ((_, st), choices) <- runWriterT $ runStateT (unChooseT body) (ChooseState Nothing Nothing [] [])
  pure (st, choices)

-- | The player chooses one of the choices; nothing is asked when there are none.
chooseOneM :: (ReverseQueue m) => PlayerId -> ChooseT m a -> m ()
chooseOneM pid body = do
  (st, choices) <- runChooseT body
  unless (null choices) $ push $ ask' pid st choices

-- | A choice whose candidates must be rebuilt when another Epic group changes
-- the public board while this player is deciding.
chooseOneRebuildingM :: ReverseQueue m => PlayerId -> Message -> ChooseT m a -> m ()
chooseOneRebuildingM pid rebuild body = do
  (st, choices) <- runChooseT body
  unless (null choices) $ case ask' pid st choices of
    Ask who question -> push $ Ask who $ PlayerWindow rebuild question
    _ -> pure ()

-- | As 'chooseOneM', but a lone choice is taken without asking.
chooseOrRunOneM :: (ReverseQueue m) => PlayerId -> ChooseT m a -> m ()
chooseOrRunOneM pid body = do
  (st, choices) <- runChooseT body
  case choices of
    [] -> pure ()
    [x] -> push $ uiToRun x
    _ -> push $ ask' pid st choices

-- | The player chooses the choices one at a time, each taken as it is chosen, until none
-- are left: the order to resolve them in. A lone choice is taken without asking.
chooseOneAtATimeM :: (ReverseQueue m) => PlayerId -> ChooseT m a -> m ()
chooseOneAtATimeM pid body = do
  (st, choices) <- runChooseT body
  case choices of
    [] -> pure ()
    [x] -> push $ uiToRun x
    _ -> push $ askWith ChooseOneAtATime pid st choices

-- | The player chooses an amount from the least to the most, as Arkham's 'chooseAmount',
-- asked as the prompt, a key under @label.@, caused by the source; a lone amount is taken
-- without asking.
chooseAmountM
  :: (ReverseQueue m, Targetable source) => PlayerId -> Text -> source -> Int -> Int -> (Int -> QueueT Message m ()) -> m ()
chooseAmountM pid key source lo hi f = do
  choices <- for [lo .. hi] \n -> Label (ikeyWith "label.amount" [intVar "n" n]) <$> capture (f n)
  case choices of
    [] -> pure ()
    [x] -> push $ uiToRun x
    _ -> push $ Ask pid $ QuestionLabel (ikey $ "label." <> key) (Just $ toTarget source) [] $ ChooseAmount lo choices

ask' :: PlayerId -> ChooseState -> [UI Message] -> Message
ask' = askWith ChooseOne

askWith :: ([UI Message] -> Question Message) -> PlayerId -> ChooseState -> [UI Message] -> Message
askWith question pid st choices = Ask pid $ case st.chooseLabel of
  Just l -> QuestionLabel l st.chooseSource st.chooseTargets shown
  Nothing -> shown
 where
  shown = if null st.chooseCards then question choices else SearchCards st.chooseCards $ question choices

-- | The prompt, a key under @label.@ in the locale files.
questionLabeled :: (Monad m) => Text -> ChooseT m ()
questionLabeled key = questionLabeledWith key []

questionLabeledWith :: (Monad m) => Text -> [I18nVar] -> ChooseT m ()
questionLabeledWith key vars = ChooseT $ modify \st -> st {chooseLabel = Just $ ikeyWith ("label." <> key) vars}

-- | The card causing the question, marked on the board.
causedBy :: (Monad m, Targetable a) => a -> ChooseT m ()
causedBy a = ChooseT $ modify \st -> st {chooseSource = Just $ toTarget a}

-- | Cards the question is aimed at, marked on the board.
aimedAt :: (Monad m, Targetable a) => [a] -> ChooseT m ()
aimedAt as = ChooseT $ modify \st -> st {chooseTargets = st.chooseTargets <> map toTarget as}

-- | Cards shown to the player choosing, as a search shows what it looked through.
showing :: (Monad m) => [Card] -> ChooseT m ()
showing cs = ChooseT $ modify \st -> st {chooseCards = st.chooseCards <> cs}

-- | A choice named by a key under @label.@ in the locale files.
labeled :: (ReverseQueue m) => Text -> QueueT Message m () -> ChooseT m ()
labeled key = labeledWith key []

-- | As 'labeled', with variables for the key.
labeledWith :: (ReverseQueue m) => Text -> [I18nVar] -> QueueT Message m () -> ChooseT m ()
labeledWith key vars action = do
  msgs <- lift $ capture action
  ChooseT $ lift $ tell [Label (ikeyWith ("label." <> key) vars) msgs]

-- | A card's Valour trigger as a choice, offered only while the player's threat is 40 or
-- higher (80 while the main quest is dire); beside the card's other trigger in
-- 'chooseOrRunOneM', the player below it is not asked:
--
-- > chooseOrRunOneM pid do
-- >   labeled "hornsCry.action" $ ...
-- >   valourLabeled pid $ ...
valourLabeled :: (ReverseQueue m) => PlayerId -> QueueT Message m () -> ChooseT m ()
valourLabeled pid action = do
  high <- lift $ selectAny (PlayerWithId pid <> PlayerAtValourThreat)
  when high $ labeled "valour" action

{- | "Either remove N progress from the main quest, or ..." as a choice, offered only while
the main quest has the N to remove: the option chosen must be performed in full, if able
(FAQ 1.44).

> chooseOrRunOneM pid do
>   removeMainQuestProgressLabeled attrs 2
>   labeled "southronSoldier.attack" $ immediateAttack pid attrs.id
-}
removeMainQuestProgressLabeled :: (ReverseQueue m, Sourceable source) => source -> Int -> ChooseT m ()
removeMainQuestProgressLabeled source n = do
  progress <- lift getMainQuestProgress
  quests <- lift getMainQuests
  when (progress >= n)
    $ labeledWith "removeMainQuestProgress" [intVar "n" n]
    $ for_ quests \q -> push (RemoveTokens (toSource source) q Progress n)

-- | A choice made by clicking the target. A card immune to player card effects is not
-- offered when a player card, the one 'causedBy' names, is causing the question (an
-- objective ally is no player card), nor one
-- immune to player events when an event (its card id) is, nor The One Ring when any but a
-- Master card is.
targeting :: (ReverseQueue m, Targetable a) => a -> QueueT Message m () -> ChooseT m ()
targeting a action = do
  source <- ChooseT $ gets (.chooseSource)
  mods <- lift $ getModifiers a
  g <- lift getGame
  master <- lift $ maybe (pure False) isMasterCard source
  let immune = case source of
        Just s ->
          (ImmuneToPlayerCardEffects `elem` mods && isPlayerCardTarget s && not (objectiveAllyTarget g s))
            || (ImmuneToPlayerEvents `elem` mods && isEvent s)
            || (ImmuneToNonMasterCardEffects `elem` mods && not master)
            || (ImmuneToNonFellowshipPlayerCardEffects `elem` mods && isPlayerCardTarget s && not (fellowshipCard g s))
            || immuneToEncounterCard g mods s
        Nothing -> False
      isEvent = \case CardIdTarget _ -> True; _ -> False
  unless immune do
    msgs <- lift $ capture action
    ChooseT $ lift $ tell [targetLabel a msgs]

-- | A choice for each target.
targets :: (ReverseQueue m, Targetable a) => [a] -> (a -> QueueT Message m ()) -> ChooseT m ()
targets as f = for_ as \a -> targeting a (f a)
