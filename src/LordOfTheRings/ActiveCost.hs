{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.ActiveCost (module LordOfTheRings.ActiveCost) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Message
import LordOfTheRings.Prelude

data Cost = ResourceCost Sphere CardCost | Free
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ActiveCost = ActiveCost
  { activeCostId :: CostId
  , activeCostCost :: Cost
  , activeCostCard :: Card
  , activeCostPlayer :: PlayerId
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ActiveCost

instance ToJSON ActiveCost where
  toJSON = genericToJSON (aesonOptions $ Just "activeCost")

instance Entity ActiveCost where
  type EntityId ActiveCost = CostId
  type EntityAttrs ActiveCost = ActiveCost
  toId = activeCostId
  toAttrs a = a
  overAttrs f a = f a

cardCost :: Card -> Cost
cardCost card = ResourceCost (fromMaybe Neutral $ cdSphere def) (fromMaybe (StaticCost 0) $ cdCost def)
 where
  def = toCardDef card

instance RunMessage ActiveCost where
  runMessage msg ac = case msg of
    PayCost acId | acId == ac.activeCostId -> case ac.activeCostCost of
      Free -> do
        push $ PaidCost acId
        pure ac
      ResourceCost _ DynamicCost -> do
        push $ PaidCost acId
        pure ac
      ResourceCost sphere (StaticCost n) -> do
        pushAll [SpendResources ac.activeCostPlayer sphere n, PayCost acId]
        pure $ ac & costCostL .~ Free
    PaidCost acId | acId == ac.activeCostId -> do
      case ac.activeCostCard of
        PlayerCard card -> case cdCardType (toCardDef card) of
          AllyType -> push $ PlayAlly ac.activeCostPlayer card
          _ -> pure ()
        EncounterCard _ -> pure ()
      pure ac
    _ -> pure ac
