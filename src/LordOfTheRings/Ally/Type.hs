{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Ally.Type (module LordOfTheRings.Ally.Type) where

import Control.Lens
import {-# SOURCE #-} LordOfTheRings.Ability
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Name
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

class
  ( Typeable a
  , Show a
  , ToJSON a
  , HasAbilities a, RunMessage a
  , Entity a
  , EntityId a ~ AllyId
  , EntityAttrs a ~ AllyAttrs
  )
  => IsAlly a

data Ally = forall a. (IsAlly a) => Ally a

instance Entity Ally where
  type EntityId Ally = AllyId
  type EntityAttrs Ally = AllyAttrs
  toId = toId . toAttrs
  toAttrs (Ally a) = toAttrs a
  overAttrs f (Ally a) = Ally $ overAttrs f a

instance HasCardCode Ally where
  toCardCode = toCardCode . toAttrs

instance HasCardCode AllyAttrs where
  toCardCode a = toCardCode a.card

instance Entity AllyAttrs where
  type EntityId AllyAttrs = AllyId
  type EntityAttrs AllyAttrs = AllyAttrs
  toId = (.id)
  toAttrs a = a
  overAttrs f a = f a


instance HasField "name" Ally Name where
  getField (Ally a) = (toAttrs a).name

instance HasField "name" AllyAttrs Name where
  getField a = cdName (toCardDef a.card)

instance HasAbilities Ally where
  getAbilities (Ally a) = getAbilities a

instance HasPlacement Ally where
  placementL =
    lens
      (\(Ally a) -> (toAttrs a) ^. placementL)
      (\(Ally a) p -> Ally $ overAttrs (placementL .~ p) a)

data AllyAttrs = AllyAttrs
  { id :: AllyId
  , card :: PlayerCard
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
  toTarget (Ally a) = toTarget (toAttrs a)

instance Sourceable Ally where
  toSource (Ally a) = toSource (toAttrs a)

instance Targetable AllyAttrs where
  toTarget a = AllyTarget a.id

instance Sourceable AllyAttrs where
  toSource a = AllySource a.id
