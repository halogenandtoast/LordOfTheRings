{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Ally.Type (module LordOfTheRings.Ally.Type) where

import Control.Lens
import Data.Aeson
import Data.Coerce
import GHC.Generics
import GHC.Records
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Run

class (Run a, Show a, ToJSON a) => IsAlly a where
  allyAttrs :: a -> AllyAttrs
  default allyAttrs :: (Coercible a AllyAttrs) => a -> AllyAttrs
  allyAttrs = coerce
  overAllyAttrs :: (AllyAttrs -> AllyAttrs) -> a -> a
  default overAllyAttrs :: (Coercible a AllyAttrs) => (AllyAttrs -> AllyAttrs) -> a -> a
  overAllyAttrs f = coerce f

data Ally where
  Ally :: (IsAlly a) => a -> Ally

instance HasField "name" Ally Name where
  getField (Ally a) = (allyAttrs a).name

instance HasField "name" AllyAttrs Name where
  getField a = a.card.def.name

instance HasPlacement Ally where
  placementL =
    lens
      (\(Ally a) -> (allyAttrs a) ^. placementL)
      (\(Ally a) p -> Ally $ overAllyAttrs (placementL .~ p) a)

data AllyAttrs = AllyAttrs
  { id :: AllyId
  , card :: PlayerCard 'AllyKind
  , sphere :: Sphere
  , placement :: Placement
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance HasPlacement AllyAttrs where
  placementL = lens (.placement) (\x y -> x {placement = y})

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a
