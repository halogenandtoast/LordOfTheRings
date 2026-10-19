{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Ally.Types (module LordOfTheRings.Ally.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
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
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ AllyId
  , EntityAttrs a ~ AllyAttrs
  )
  => IsAlly a

type AllyCard a = CardBuilder (AllyId, PlayerId) a

data Ally = forall a. (IsAlly a) => Ally a

data SomeAllyCard = forall a. (IsAlly a) => SomeAllyCard (AllyCard a)

data AllyAttrs = AllyAttrs
  { allyId :: AllyId
  , allyCardId :: CardId
  , allyCardCode :: CardCode
  , allyOwner :: PlayerId
  , allyPlacement :: Placement
  , allyTokens :: Tokens
  , allyExhausted :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''AllyAttrs

instance ToJSON AllyAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "ally")

ally :: (AllyAttrs -> a) -> CardDef -> AllyCard a
ally f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId (hid, pid) ->
        f
          AllyAttrs
            { allyId = hid
            , allyCardId = cardId
            , allyCardCode = cdCardCode cardDef
            , allyOwner = pid
            , allyPlacement = Unplaced
            , allyTokens = mempty
            , allyExhausted = False
            }
    }

instance Entity AllyAttrs where
  type EntityId AllyAttrs = AllyId
  type EntityAttrs AllyAttrs = AllyAttrs
  toId = allyId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Ally where
  type EntityId Ally = AllyId
  type EntityAttrs Ally = AllyAttrs
  toId = toId . toAttrs
  toAttrs (Ally a) = toAttrs a
  overAttrs f (Ally a) = Ally $ overAttrs f a

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a

instance IsCard AllyAttrs where
  toCard a = PlayerCard $ MkPlayerCard {pcId = allyCardId a, pcCardCode = allyCardCode a, pcOwner = Just (allyOwner a)}

instance IsCard Ally where
  toCard = toCard . toAttrs

instance HasCardCode AllyAttrs where
  toCardCode = allyCardCode

instance HasCardCode Ally where
  toCardCode = toCardCode . toAttrs

instance HasCardDef AllyAttrs where
  toCardDef = toCardDef . allyCardCode

instance HasCardDef Ally where
  toCardDef = toCardDef . toAttrs

instance Targetable AllyAttrs where
  toTarget = AllyTarget . allyId

instance Targetable Ally where
  toTarget = toTarget . toAttrs

instance Sourceable AllyAttrs where
  toSource = AllySource . allyId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Ally where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" AllyAttrs AllyId where
  getField = allyId

instance HasField "owner" AllyAttrs PlayerId where
  getField = allyOwner

instance HasField "name" AllyAttrs Name where
  getField = cdName . toCardDef

instance HasField "sphere" AllyAttrs Sphere where
  getField = fromMaybe Neutral . cdSphere . toCardDef

instance HasModifiersFor Ally where
  getModifiersFor (Ally a) = getModifiersFor a

instance HasAbilities Ally where
  getAbilities (Ally a) = getAbilities a

instance HasField "placement" AllyAttrs Placement where
  getField = allyPlacement

instance HasField "id" Ally AllyId where
  getField = toId

instance HasField "owner" Ally PlayerId where
  getField = (.owner) . toAttrs

instance HasField "name" Ally Name where
  getField = (.name) . toAttrs

instance HasField "sphere" Ally Sphere where
  getField = (.sphere) . toAttrs

instance HasField "placement" Ally Placement where
  getField = (.placement) . toAttrs
