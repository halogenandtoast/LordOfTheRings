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
import LordOfTheRings.Target (toTarget)
import LordOfTheRings.Window

data Cost = ResourceCost Sphere CardCost | Free
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActiveCost = ActiveCost
  { activeCostId :: CostId
  , activeCostCost :: Cost
  , activeCostCard :: Card
  , activeCostPlayer :: PlayerId
  , activeCostWindows :: [Window]
  -- ^ the windows the card is played in response to, if any
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ActiveCost

instance ToJSON ActiveCost where
  toJSON = genericToJSON (aesonOptions $ Just "activeCost")

instance FromJSON ActiveCost where
  parseJSON = genericParseJSON (aesonOptions $ Just "activeCost")

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

-- | A cost raised by the given amount (lowered, when it is negative), to no less than 0.
raiseCost :: Int -> Cost -> Cost
raiseCost n = \case
  ResourceCost sphere (StaticCost c) -> ResourceCost sphere (StaticCost $ max 0 $ c + n)
  cost -> cost

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
        let spend = case cdDistinctPayers (toCardDef ac.activeCostCard) of
              Just _ -> SpendResourcesFromDistinctHeroes ac.activeCostPlayer sphere n []
              Nothing -> SpendResources ac.activeCostPlayer sphere n
        pushAll [spend, PayCost acId]
        pure $ ac & costCostL .~ Free
    PaidCost acId | acId == ac.activeCostId -> do
      case ac.activeCostCard of
        PlayerCard card -> do
          case cdCardType (toCardDef card) of
            AllyType -> push $ PlayAlly ac.activeCostPlayer card
            AttachmentType -> push $ PlayAttachment ac.activeCostPlayer card
            EventType -> push $ PlayEvent ac.activeCostPlayer card ac.activeCostWindows
            _ -> pure ()
          push $ CheckWindows [mkAfter $ PlayedFromHand (toTarget card)]
        EncounterCard _ -> pure ()
      pure ac
    _ -> pure ac
