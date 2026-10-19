module LordOfTheRings.Classes.Query (module LordOfTheRings.Classes.Query) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Prelude

class Query a where
  type QueryElement a
  select :: (HasGame m) => a -> m [QueryElement a]

selectOne :: (HasGame m, Query a) => a -> m (Maybe (QueryElement a))
selectOne = fmap headMay . select

selectJust :: (HasCallStack, HasGame m, Query a, Show a) => a -> m (QueryElement a)
selectJust matcher = fromJustNote ("selectJust: nothing matched " <> show matcher) <$> selectOne matcher

selectAny :: (HasGame m, Query a) => a -> m Bool
selectAny = fmap notNull . select

selectNone :: (HasGame m, Query a) => a -> m Bool
selectNone = fmap null . select

selectCount :: (HasGame m, Query a) => a -> m Int
selectCount = fmap length . select
