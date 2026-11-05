{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- Compare byte-for-byte traces between revisions with:
-- stack exec -- runghc tools/BuildSmoke.hs > /tmp/trace.json
module Main (main) where

import Control.Monad.Random (evalRand, mkStdGen)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Decklist
import LordOfTheRings.Game
import LordOfTheRings.GameT
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Scenario

main :: IO ()
main = BL.putStrLn . encode =<< traverse smoke
  [(name, scenario, seed) | (name, scenario) <- [("mirkwood" :: Text, passageThroughMirkwood), ("anduin", journeyAlongTheAnduin), ("dol-guldur", escapeFromDolGuldur)], seed <- [1, 7, 42]]
 where
  smoke (name, scenario, seed) = do
    let deck = fromJustNote "leadership deck" $ lookup "leadership" starterDecks
        (player, heroes) = either (error . show) id $ evalRand (loadPlayer "smoke" deck) (mkStdGen seed)
    logs <- newIORef ([] :: [Value])
    env <- newGameEnv (newGameIn scenario player heroes) (\msg -> modifyIORef' logs (<> [toJSON msg])) (seed + 1)
    trace <- runGameT env do
      push Setup
      void mainLoop
      forM [0 .. 79 :: Int] \step -> do
        game <- getGame
        queue <- peekQueue
        gen <- readIORef (gameRandomGen env)
        let snapshot = object ["game" .= game, "queue" .= queue, "generator" .= show gen]
        -- Prefer the last choice to pass action windows. Vary the choice in
        -- occasional steps to exercise alternate paths without an unbounded loop.
        case Map.toList game.question of
          (pid, question) : _ | let count = length (questionChoices question), count > 0 ->
            void $ answerQuestion pid (if step `mod` 7 == 0 then step `mod` count else count - 1)
          _ -> pure ()
        pure snapshot
    logged <- readIORef logs
    pure $ object ["scenario" .= name, "seed" .= seed, "trace" .= trace, "log" .= logged]
