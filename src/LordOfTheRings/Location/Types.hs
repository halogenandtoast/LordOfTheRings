{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Location.Types (module LordOfTheRings.Location.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Name
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

class
  ( Typeable a
  , Show a
  , ToJSON a
  , HasAbilities a
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
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''LocationAttrs

instance ToJSON LocationAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "location")

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

instance HasAbilities Location where
  getAbilities (Location a) = getAbilities a

instance Show Location where
  show (Location a) = show a

instance ToJSON Location where
  toJSON (Location a) = toJSON a

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
