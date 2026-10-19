{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Ally.Type (module LordOfTheRings.Ally.Type) where

import Control.Lens
import {-# SOURCE #-} LordOfTheRings.Ability
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Name
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

class (HasAbilities a, RunMessage a, Show a, ToJSON a) => IsAlly a where
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

instance HasAbilities Ally where
  getAbilities (Ally a) = getAbilities a

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

instance Targetable Ally where
  toTarget (Ally a) = toTarget (allyAttrs a)

instance Sourceable Ally where
  toSource (Ally a) = toSource (allyAttrs a)

instance Targetable AllyAttrs where
  toTarget a = AllyTarget a.id

instance Sourceable AllyAttrs where
  toSource a = AllySource a.id
