module LordOfTheRings.Entity (module LordOfTheRings.Entity) where

import GHC.Records

class (HasField "id" a (Id a)) => Entity a where
  type Id a
  type Attrs a
  overAttrs :: (Attrs a -> Attrs a) -> a -> a
