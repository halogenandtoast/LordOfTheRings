{-# LANGUAGE OverloadedRecordDot #-}

module LordOfTheRings.Matcher (module LordOfTheRings.Matcher) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.Type
import LordOfTheRings.Card
import {-# SOURCE #-} LordOfTheRings.Game.Base
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id
import LordOfTheRings.Prelude

class Query a where
  data Matcher a
  select :: (HasGame m) => Matcher a -> m [a]

instance Query Hero where
  data Matcher Hero = PlayerHero PlayerId | HeroWithName Name
  select matcher = do
    heroes <- Map.elems <$> getsGame (.entities.heroes)
    case matcher of
      PlayerHero pid -> pure $ filter ((== pid) . (.player)) heroes
      HeroWithName name -> pure $ filter ((== name) . (.name)) heroes

instance Query Ally where
  data Matcher Ally = AllyWithName Name
  select matcher = do
    allies <- Map.elems <$> getsGame (.entities.allies)
    case matcher of
      AllyWithName name -> pure $ filter ((== name) . (.name)) allies

instance Query Player where
  data Matcher Player = ActivePlayer
  select matcher = do
    case matcher of
      ActivePlayer -> (: []) <$> (getPlayer =<< getsGame (.activePlayer))

selectJust :: (HasGame m, Query a) => Matcher a -> m a
selectJust matcher = do
  as <- select matcher
  case as of
    [a] -> pure a
    _ -> error "selectJust: expected exactly one result"

selectOne :: (HasGame m, Query a) => Matcher a -> m (Maybe a)
selectOne matcher = do
  as <- select matcher
  case as of
    (a : _) -> pure (Just a)
    _ -> pure Nothing

selectNone :: (HasGame m, Query a) => Matcher a -> m Bool
selectNone matcher = null <$> select matcher
