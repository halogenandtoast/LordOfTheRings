-- | Questions built the way they read: the choices are actions run in a queue of their
-- own, and the messages they push become the choice's.
--
-- > chooseOneM pid do
-- >   questionLabeled "gandalf.chooseOne"
-- >   causedBy attrs
-- >   labeled "gandalf.draw" $ drawCards pid 3
-- >   targets enemies \e -> dealDamage attrs e 4
module LordOfTheRings.Message.Lifted.Choose (module LordOfTheRings.Message.Lifted.Choose) where

import Control.Monad.State.Strict (StateT, modify, runStateT)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import LordOfTheRings.Card (Card)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.I18n
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted.Queue
import LordOfTheRings.Prelude
import LordOfTheRings.Queue
import LordOfTheRings.Question
import LordOfTheRings.Target

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

-- | As 'chooseOneM', but a lone choice is taken without asking.
chooseOrRunOneM :: (ReverseQueue m) => PlayerId -> ChooseT m a -> m ()
chooseOrRunOneM pid body = do
  (st, choices) <- runChooseT body
  case choices of
    [] -> pure ()
    [x] -> push $ uiToRun x
    _ -> push $ ask' pid st choices

ask' :: PlayerId -> ChooseState -> [UI Message] -> Message
ask' pid st choices = Ask pid $ case st.chooseLabel of
  Just l -> QuestionLabel l st.chooseSource st.chooseTargets shown
  Nothing -> shown
 where
  shown = if null st.chooseCards then ChooseOne choices else SearchCards st.chooseCards $ ChooseOne choices

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

-- | A choice made by clicking the target.
targeting :: (ReverseQueue m, Targetable a) => a -> QueueT Message m () -> ChooseT m ()
targeting a action = do
  msgs <- lift $ capture action
  ChooseT $ lift $ tell [targetLabel a msgs]

-- | A choice for each target.
targets :: (ReverseQueue m, Targetable a) => [a] -> (a -> QueueT Message m ()) -> ChooseT m ()
targets as f = for_ as \a -> targeting a (f a)
