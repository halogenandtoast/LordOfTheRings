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
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Modifier (ModifierType (PlayedACardThisRound))
import LordOfTheRings.Source (Source (GameSource, PlayerSource))
import LordOfTheRings.Target (Target (PlayerTarget), toTarget)
import LordOfTheRings.Token (Token (Resource))
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
  , activeCostHost :: Maybe Target
  -- ^ the host an attachment was chosen to go on before it was paid for
  , activeCostFromHand :: Bool
  -- ^ played from its owner's hand, not from the discard pile
  , activeCostDoomed :: Int
  -- ^ the Doomed it gained from an effect spent on it, as Gríma's
  , activeCostPaidFrom :: [Target]
  -- ^ the heroes and allies whose resources paid for it, once for each resource
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ActiveCost

instance ToJSON ActiveCost where
  toJSON = genericToJSON (aesonOptions $ Just "activeCost")

instance FromJSON ActiveCost where
  parseJSON = genericParseJSON (aesonOptions $ Just "activeCost") . withDefaults [("fromHand", toJSON True), ("doomed", toJSON (0 :: Int)), ("paidFrom", toJSON ([] :: [Target]))]

instance Entity ActiveCost where
  type EntityId ActiveCost = CostId
  type EntityAttrs ActiveCost = ActiveCost
  toId = activeCostId
  toAttrs a = a
  overAttrs f a = f a

cardCost :: Card -> Cost
cardCost card = ResourceCost (fromMaybe Neutral $ cdSphere def) $ case cdCost def of
  Just XCost -> StaticCost 0
  cost -> fromMaybe (StaticCost 0) cost
 where
  def = toCardDef card

-- | A cost raised by the given amount (lowered, when it is negative), to no less than 0.
raiseCost :: Int -> Cost -> Cost
raiseCost n = \case
  ResourceCost sphere (StaticCost c) -> ResourceCost sphere (StaticCost $ max 0 $ c + n)
  cost -> cost

-- | The resources a cost asks for.
staticCost :: Cost -> Int
staticCost = \case
  ResourceCost _ (StaticCost c) -> c
  _ -> 0

instance RunMessage ActiveCost where
  runMessage msg ac = case msg of
    ReduceActiveCost acId n | acId == ac.activeCostId -> pure $ ac & costCostL %~ raiseCost (negate n)
    -- its player's resources spent while it is paid for pay for it
    RemoveTokens (PlayerSource pid) target Resource n | pid == ac.activeCostPlayer -> pure $ ac & costPaidFromL <>~ replicate n target
    PayCost acId | acId == ac.activeCostId -> case ac.activeCostCost of
      Free -> do
        push $ PaidCost acId
        pure ac
      ResourceCost sphere (StaticCost n) -> do
        let def = toCardDef ac.activeCostCard
            spend = case cdDistinctPayers def of
              Just _ -> SpendResourcesFromDistinctHeroes ac.activeCostPlayer sphere n []
              Nothing
                | cdSinglePayer def -> SpendResourcesFromOneHero ac.activeCostPlayer sphere n
                | otherwise -> SpendResources ac.activeCostPlayer sphere n
        pushAll [spend, PayCost acId]
        pure $ ac & costCostL .~ Free
      -- a chosen X the card pays as it resolves ('cardCost' turns an 'XCost' static)
      ResourceCost _ _ -> do
        push $ PaidCost acId
        pure ac
    PaidCost acId | acId == ac.activeCostId -> do
      case ac.activeCostCard of
        PlayerCard card -> do
          push $ CreateEffect $ Effect GameSource (PlayerTarget ac.activeCostPlayer) [PlayedACardThisRound] UntilEndOfRound
          pushAll $ maybe (playCard ac.activeCostPlayer card ac.activeCostWindows) (pure . PlayAttachmentOn ac.activeCostPlayer card) ac.activeCostHost
          push
            $ CheckWindows
            $ [mkAfter $ PlayedFromHand (toTarget card) | ac.activeCostFromHand]
            <> [mkAfter $ PlayedCardFromHand ac.activeCostPlayer (toCardCode card) | ac.activeCostFromHand]
            <> [mkAfter $ PlayedCard ac.activeCostPlayer (toCardCode card)]
            <> [mkAfter $ PaidFrom (toTarget card) ac.activeCostPaidFrom]
        EncounterCard _ -> pure ()
      pure ac
    _ -> pure ac

-- | A card, its cost paid, played into play or resolved.
playCard :: PlayerId -> PlayerCard -> [Window] -> [Message]
playCard pid card windows = case cdCardType (toCardDef card) of
  AllyType -> [PlayAlly pid card]
  AttachmentType -> [PlayAttachment pid card]
  -- the When window comes first, for an effect that cancels it, as Counter-Spell's
  EventType -> [CheckWindows [mkWhen $ PlayingEvent pid card.id], PlayEvent pid card windows]
  PlayerSideQuestType -> [PutSideQuestIntoPlay pid card]
  _ -> []
