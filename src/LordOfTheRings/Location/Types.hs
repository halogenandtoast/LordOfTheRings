{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Location.Types (module LordOfTheRings.Location.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Keyword (Keyword (Uncharted))
import LordOfTheRings.Location.Grid (Pos)
import LordOfTheRings.Name
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Trait (Trait (Grotto, Underwater))

class
  ( Typeable a
  , Show a
  , ToJSON a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ LocationId
  , EntityAttrs a ~ LocationAttrs
  )
  => IsLocation a

type LocationCard a = CardBuilder LocationId a

data Location = forall a. (IsLocation a) => Location a

data SomeLocationCard = forall a. (IsLocation a) => SomeLocationCard (LocationCard a)

data LocationAttrs = LocationAttrs
  { locationId :: LocationId
  , locationCardId :: CardId
  , locationCardCode :: CardCode
  , locationPlacement :: Placement
  , locationTokens :: Tokens
  , locationOtherSide :: Maybe CardCode
  -- ^ the side facedown, of a double-sided location turned over as it entered play or
  -- since: its card code is the side faceup
  , locationLookedAt :: [PlayerId]
  -- ^ the players who have looked at its facedown side since it was last turned over
  , locationPosition :: Maybe Pos
  -- ^ where it lies on the Island Map (Temple of the Deceived)
  , locationUnderneath :: [Card]
  -- ^ the cards underneath it, out of play, as the enemies Narrow Alleyway hides
  , locationUnderneathFaceup :: Bool
  -- ^ the cards underneath it lie faceup, for all to see, as the Warden under a Camp (The
  -- Land of Sorrow); facedown otherwise, as Seedy Tavern's
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''LocationAttrs

instance ToJSON LocationAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "location")

instance FromJSON LocationAttrs where
  parseJSON = genericParseJSON (aesonOptions $ Just "location") . withDefaults [("otherSide", Null), ("lookedAt", toJSON ([] :: [PlayerId])), ("position", Null), ("underneath", toJSON ([] :: [Card])), ("underneathFaceup", Bool False)]

location :: (LocationAttrs -> a) -> CardDef -> LocationCard a
location f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId ident ->
        f
          LocationAttrs
            { locationId = ident
            , locationCardId = cardId
            , locationCardCode = cdCardCode cardDef
            , locationPlacement = Unplaced
            , locationTokens = mempty
            , locationOtherSide = Nothing
            , locationLookedAt = []
            , locationPosition = Nothing
            , locationUnderneath = []
            , locationUnderneathFaceup = False
            }
    }

instance Entity LocationAttrs where
  type EntityId LocationAttrs = LocationId
  type EntityAttrs LocationAttrs = LocationAttrs
  toId = locationId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs
  toId = toId . toAttrs
  toAttrs (Location a) = toAttrs a
  overAttrs f (Location a) = Location $ overAttrs f a

instance HasModifiersFor Location where
  getModifiersFor (Location a) = getModifiersFor a

instance HasAbilities Location where
  getAbilities (Location a) = getAbilities a

instance Show Location where
  show (Location a) = show a

instance ToJSON Location where
  toJSON (Location a) = toJSON a

instance IsCard LocationAttrs where
  toCard a = EncounterCard $ MkEncounterCard {ecId = locationCardId a, ecCardCode = locationCardCode a}

instance IsCard Location where
  toCard = toCard . toAttrs

instance HasCardCode LocationAttrs where
  toCardCode = locationCardCode

instance HasCardCode Location where
  toCardCode = toCardCode . toAttrs

instance HasCardDef LocationAttrs where
  toCardDef = toCardDef . locationCardCode

instance HasCardDef Location where
  toCardDef = toCardDef . toAttrs

instance Targetable LocationAttrs where
  toTarget = LocationTarget . locationId

instance Targetable Location where
  toTarget = toTarget . toAttrs

instance Sourceable LocationAttrs where
  toSource = LocationSource . locationId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Location where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" LocationAttrs LocationId where
  getField = locationId

instance HasField "id" Location LocationId where
  getField = toId

instance HasField "underneath" LocationAttrs [Card] where
  getField = locationUnderneath

instance HasField "placement" LocationAttrs Placement where
  getField = locationPlacement

instance HasField "placement" Location Placement where
  getField = (.placement) . toAttrs

instance HasField "name" LocationAttrs Name where
  getField = cdName . toCardDef

instance HasField "name" Location Name where
  getField = (.name) . toAttrs

instance HasField "threat" LocationAttrs Int where
  getField = fromMaybe 0 . cdThreat . toCardDef

instance HasField "threat" Location Int where
  getField = (.threat) . toAttrs

instance HasField "questPoints" LocationAttrs Int where
  getField = fromMaybe 0 . cdQuestPoints . toCardDef

instance HasField "questPoints" Location Int where
  getField = (.questPoints) . toAttrs

instance HasField "progress" LocationAttrs Int where
  getField = countTokens Progress . locationTokens

instance HasField "progress" Location Int where
  getField = (.progress) . toAttrs

instance HasField "damage" LocationAttrs Int where
  getField = countTokens Damage . locationTokens

instance HasField "cardId" LocationAttrs CardId where
  getField = locationCardId

-- | The side facedown: the one it was turned over from, or the other side printed.
instance HasField "otherSide" LocationAttrs (Maybe CardCode) where
  getField a = locationOtherSide a <|> cdOtherSide (toCardDef a)

instance HasField "lookedAt" LocationAttrs [PlayerId] where
  getField = locationLookedAt

instance HasField "position" LocationAttrs (Maybe Pos) where
  getField = locationPosition

instance HasField "position" Location (Maybe Pos) where
  getField = (.position) . toAttrs

-- | With its Lost Island side faceup: an Uncharted location (The Grey Havens rules insert).
isUncharted :: HasCardDef a => a -> Bool
isUncharted = member Uncharted . cdKeywords . toCardDef

-- | An Uncharted location's card, whichever side is faceup: it returns to the Uncharted
-- deck rather than the discard pile or the encounter deck.
isUnchartedCard :: LocationAttrs -> Bool
isUnchartedCard a = isUncharted a || maybe False isUncharted a.otherSide

{- | The location card itself, as it goes into a deck: an Uncharted one's card is the side
under its Lost Island side.
-}
locationDeckCard :: LocationAttrs -> EncounterCard
locationDeckCard a = MkEncounterCard (locationCardId a) code
 where
  code = case a.otherSide of
    Just other | isUncharted a -> other
    _ -> locationCardCode a

-- | A card of The Drowned Ruins' Grotto deck, either side faceup: it cannot enter the
-- discard pile or the encounter deck, returning to the Grotto deck instead.
isGrottoCard :: LocationAttrs -> Bool
isGrottoCard a = isJust a.otherSide && any (`member` cdCardTraits (toCardDef a)) [Grotto, Underwater]

-- | A Grotto deck card as it goes back into the deck: Grotto side up.
grottoDeckCard :: LocationAttrs -> EncounterCard
grottoDeckCard a = MkEncounterCard (locationCardId a) code
 where
  code = case a.otherSide of
    Just other | Grotto `notMember` cdCardTraits (toCardDef a) -> other
    _ -> locationCardCode a

-- | Turned over to its other side, its progress removed and what was seen of it forgotten.
-- The card builds it afresh from that side, keeping where it is and its other tokens.
flipLocation :: (CardCode -> CardId -> LocationId -> Location) -> LocationAttrs -> Maybe Location
flipLocation build a = a.otherSide <&> \other ->
  overAttrs
    ( const
        a
          { locationCardCode = other
          , locationOtherSide = Just (locationCardCode a)
          , locationTokens = removeAllTokens Progress (locationTokens a)
          , locationLookedAt = []
          }
    )
    (build other (locationCardId a) (locationId a))
