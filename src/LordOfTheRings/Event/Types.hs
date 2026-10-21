{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Event.Types (module LordOfTheRings.Event.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Criteria
import LordOfTheRings.Id
import LordOfTheRings.Matcher.Window
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

-- | Events never stay in play: one is built from its card to be played, resolves its
-- 'PlayEvent' and is gone, so it keeps no state of its own.
class
  ( Typeable a
  , Show a
  , RunMessage a
  , HasModifiersFor a
  , Entity a
  , EntityId a ~ CardId
  , EntityAttrs a ~ EventAttrs
  )
  => IsEvent a

type EventCard a = CardBuilder PlayerId a

data Event = forall a. (IsEvent a) => Event a

data SomeEventCard = forall a. (IsEvent a) => SomeEventCard (EventCard a)

data EventAttrs = EventAttrs
  { eventCardId :: CardId
  , eventCardCode :: CardCode
  , eventOwner :: PlayerId
  , eventWindow :: Maybe WindowMatcher
  -- ^ when the event is a response, the window it is played in; otherwise it is an action
  , eventCriteria :: Criterion
  -- ^ what must be true for the event to be played
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''EventAttrs

event :: (EventAttrs -> a) -> CardDef -> EventCard a
event f cardDef = eventWith f cardDef id

-- | An event whose window or criteria depend on the player who would play it.
eventWith :: (EventAttrs -> a) -> CardDef -> (EventAttrs -> EventAttrs) -> EventCard a
eventWith f cardDef g =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId pid ->
        f
          $ g
            EventAttrs
              { eventCardId = cardId
              , eventCardCode = cdCardCode cardDef
              , eventOwner = pid
              , eventWindow = Nothing
              , eventCriteria = NoCriteria
              }
    }

instance Entity EventAttrs where
  type EntityId EventAttrs = CardId
  type EntityAttrs EventAttrs = EventAttrs
  toId = eventCardId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Event where
  type EntityId Event = CardId
  type EntityAttrs Event = EventAttrs
  toId = toId . toAttrs
  toAttrs (Event a) = toAttrs a
  overAttrs f (Event a) = Event $ overAttrs f a

instance Show Event where
  show (Event a) = show a

instance RunMessage EventAttrs where
  runMessage _ = pure

-- | An event modifies only itself, from its owner's hand: what it costs to play.
instance HasModifiersFor Event where
  getModifiersFor (Event a) = getModifiersFor a

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

instance HasCardDef EventAttrs where
  toCardDef = toCardDef . eventCardCode

instance Targetable EventAttrs where
  toTarget = CardIdTarget . eventCardId

instance Sourceable EventAttrs where
  toSource = CardIdSource . eventCardId

instance HasField "owner" EventAttrs PlayerId where
  getField = eventOwner
