module TestImport (
  module X,
  module TestImport,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame as X
import LordOfTheRings.Classes.HasQueue as X
import LordOfTheRings.Decklist
import LordOfTheRings.Game as X
import LordOfTheRings.GameT as X
import LordOfTheRings.Id as X
import LordOfTheRings.Matcher as X (getPlayer)
import LordOfTheRings.Message as X
import LordOfTheRings.Prelude as X
import LordOfTheRings.Question as X
import Test.Hspec as X hiding (shouldBe, shouldReturn, shouldSatisfy)
import Test.Hspec.Expectations.Lifted as X (shouldBe, shouldReturn, shouldSatisfy)

-- | Run a test against a one player game built from the starter deck.
gameTest :: (PlayerId -> GameT ()) -> IO ()
gameTest body = do
  (player, heroes) <- either (error . show) pure =<< loadPlayer starterDeck
  env <- newGameEnv (newGame player heroes) (const $ pure ()) 1
  runGameT env (body player.id)

-- | Push a message and run the queue until it empties or a question is asked.
run :: Message -> GameT ()
run msg = push msg >> void mainLoop

getQuestion :: PlayerId -> GameT (Maybe (Question Message))
getQuestion pid = (.question) <$> getPlayer pid

-- | Answer the open question with the first choice that matches.
chooseOptionMatching :: (HasCallStack) => Text -> PlayerId -> (Choice Message -> Bool) -> GameT ()
chooseOptionMatching reason pid f = do
  question <- getQuestion pid
  let choices = case question of
        Just (ChooseOne cs) -> cs
        Just (ChooseOneAtATime cs) -> cs
        Nothing -> error $ "no question when trying to " <> unpack reason
  case find f choices of
    Nothing -> error $ "could not " <> unpack reason
    Just choice -> do
      g <- getGame
      putGame $ g {players = Map.adjust (\p -> p {question = Nothing}) pid g.players}
      pushAll $ choiceMessages pid choice
      void mainLoop

chooseLabel :: (HasCallStack) => Text -> PlayerId -> GameT ()
chooseLabel l pid = chooseOptionMatching ("choose " <> l) pid \case
  Label l' _ -> l == l'
  _ -> False
