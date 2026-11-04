{-# LANGUAGE TemplateHaskell #-}

{- | A contract: a player card never in a deck, put into play with its player's heroes,
side A faceup (MEC77 rules, "Contracts"). It sits in its player's area, or attached to a
character once its text says so, as Messenger of the King's side B; it may flip, exhaust
and hold cards facedown underneath, as The Burglar's Turn's loot deck. As Arkham's story
cards it is its own kind of entity, with abilities, modifiers and two sides.
-}
module LordOfTheRings.Contract.Types (module LordOfTheRings.Contract.Types) where

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

class
  ( Typeable a
  , Show a
  , ToJSON a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ ContractId
  , EntityAttrs a ~ ContractAttrs
  )
  => IsContract a

type ContractCard a = CardBuilder (ContractId, PlayerId) a

data Contract = forall a. (IsContract a) => Contract a

data SomeContractCard = forall a. (IsContract a) => SomeContractCard (ContractCard a)

data ContractAttrs = ContractAttrs
  { contractId :: ContractId
  , contractCardId :: CardId
  , contractCardCode :: CardCode
  , contractOwner :: PlayerId
  , contractPlacement :: Placement
  -- ^ in its player's area, or attached to a character
  , contractFlipped :: Bool
  -- ^ side B faceup
  , contractExhausted :: Bool
  , contractUnderneath :: [Card]
  -- ^ the cards facedown underneath it, in order, as The Burglar's Turn's loot deck
  , contractMeta :: Value
  -- ^ what only one contract keeps, as Arkham's meta: its module reads and writes it
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ContractAttrs

instance ToJSON ContractAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "contract")

instance FromJSON ContractAttrs where
  parseJSON = genericParseJSON (aesonOptions $ Just "contract")

contract :: (ContractAttrs -> a) -> CardDef -> ContractCard a
contract f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId (ident, pid) ->
        f
          ContractAttrs
            { contractId = ident
            , contractCardId = cardId
            , contractCardCode = cdCardCode cardDef
            , contractOwner = pid
            , contractPlacement = InPlayArea pid
            , contractFlipped = False
            , contractExhausted = False
            , contractUnderneath = []
            , contractMeta = Null
            }
    }

instance Entity ContractAttrs where
  type EntityId ContractAttrs = ContractId
  type EntityAttrs ContractAttrs = ContractAttrs
  toId = contractId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Contract where
  type EntityId Contract = ContractId
  type EntityAttrs Contract = ContractAttrs
  toId = toId . toAttrs
  toAttrs (Contract a) = toAttrs a
  overAttrs f (Contract a) = Contract $ overAttrs f a

instance HasModifiersFor Contract where
  getModifiersFor (Contract a) = getModifiersFor a

instance HasAbilities Contract where
  getAbilities (Contract a) = getAbilities a

instance Show Contract where
  show (Contract a) = show a

instance ToJSON Contract where
  toJSON (Contract a) = toJSON a

instance IsCard ContractAttrs where
  toCard a = PlayerCard $ MkPlayerCard {pcId = contractCardId a, pcCardCode = contractCardCode a, pcOwner = Just (contractOwner a)}

instance IsCard Contract where
  toCard = toCard . toAttrs

instance HasCardCode ContractAttrs where
  toCardCode = contractCardCode

instance HasCardCode Contract where
  toCardCode = toCardCode . toAttrs

instance HasCardDef ContractAttrs where
  toCardDef = toCardDef . contractCardCode

instance HasCardDef Contract where
  toCardDef = toCardDef . toAttrs

instance Targetable ContractAttrs where
  toTarget = ContractTarget . contractId

instance Targetable Contract where
  toTarget = toTarget . toAttrs

instance Sourceable ContractAttrs where
  toSource = ContractSource . contractId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Contract where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" ContractAttrs ContractId where
  getField = contractId

instance HasField "id" Contract ContractId where
  getField = toId

instance HasField "owner" ContractAttrs PlayerId where
  getField = contractOwner

instance HasField "owner" Contract PlayerId where
  getField = (.owner) . toAttrs

instance HasField "placement" ContractAttrs Placement where
  getField = contractPlacement

instance HasField "placement" Contract Placement where
  getField = (.placement) . toAttrs

instance HasField "flipped" ContractAttrs Bool where
  getField = contractFlipped

instance HasField "flipped" Contract Bool where
  getField = (.flipped) . toAttrs

instance HasField "exhausted" ContractAttrs Bool where
  getField = contractExhausted

instance HasField "exhausted" Contract Bool where
  getField = (.exhausted) . toAttrs

instance HasField "underneath" ContractAttrs [Card] where
  getField = contractUnderneath

instance HasField "meta" ContractAttrs Value where
  getField = contractMeta

instance HasField "name" ContractAttrs Name where
  getField = cdName . toCardDef

instance HasField "cardId" ContractAttrs CardId where
  getField = contractCardId
