{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Model (module Model) where

import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import LordOfTheRings.Prelude

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User sql=users
  username Text
  email Text
  passwordDigest Text
  createdAt UTCTime
  UniqueUsername username
  UniqueEmail email
  deriving Show

LotrGame sql=lotr_games
  userId UserId OnDeleteCascade
  name Text
  playerName Text
  currentData Value
  currentQueue Value
  randomGen Text
  step Int
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

-- | Enough to step a game back one answer: a patch from the game after the answer to
-- the game before it, and the queue and random generator from before it.
LotrStep sql=lotr_steps
  gameId LotrGameId OnDeleteCascade
  step Int
  patchDown Value
  queue Value
  randomGen Text
  round Int
  phase Text
  createdAt UTCTime
  UniqueGameStep gameId step
  deriving Show

-- | An imported deck list. Slots name our card code when the importer could place
-- the card, and always keep the code the source used.
LotrDeck sql=lotr_decks
  userId UserId OnDeleteCascade
  name Text
  description Text
  startingThreat Int Maybe
  source Text
  sourceId Text Maybe
  sourceUrl Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

LotrDeckSlot sql=lotr_deck_slots
  deckId LotrDeckId OnDeleteCascade
  kind Text
  position Int
  sourceCode Text
  cardCode Text Maybe
  quantity Int
  deriving Show
|]
