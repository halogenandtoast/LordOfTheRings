{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Treachery.Types (module LordOfTheRings.Treachery.Types) where

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
  , EntityId a ~ TreacheryId
  , EntityAttrs a ~ TreacheryAttrs
  )
  => IsTreachery a

type TreacheryCard a = CardBuilder TreacheryId a

data Treachery = forall a. (IsTreachery a) => Treachery a

data SomeTreacheryCard = forall a. (IsTreachery a) => SomeTreacheryCard (TreacheryCard a)

data TreacheryAttrs = TreacheryAttrs
  { treacheryId :: TreacheryId
  , treacheryCardId :: CardId
  , treacheryCardCode :: CardCode
  , treacheryPlacement :: Placement
  , treacheryTokens :: Tokens
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''TreacheryAttrs

instance ToJSON TreacheryAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "treachery")

treachery :: (TreacheryAttrs -> a) -> CardDef -> TreacheryCard a
treachery f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId ident ->
        f
          TreacheryAttrs
            { treacheryId = ident
            , treacheryCardId = cardId
            , treacheryCardCode = cdCardCode cardDef
            , treacheryPlacement = Unplaced
            , treacheryTokens = mempty
            }
    }

instance Entity TreacheryAttrs where
  type EntityId TreacheryAttrs = TreacheryId
  type EntityAttrs TreacheryAttrs = TreacheryAttrs
  toId = treacheryId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs
  toId = toId . toAttrs
  toAttrs (Treachery a) = toAttrs a
  overAttrs f (Treachery a) = Treachery $ overAttrs f a

instance HasModifiersFor Treachery where
  getModifiersFor (Treachery a) = getModifiersFor a

instance HasAbilities Treachery where
  getAbilities (Treachery a) = getAbilities a

instance Show Treachery where
  show (Treachery a) = show a

instance ToJSON Treachery where
  toJSON (Treachery a) = toJSON a

instance IsCard TreacheryAttrs where
  toCard a = EncounterCard $ MkEncounterCard {ecId = treacheryCardId a, ecCardCode = treacheryCardCode a}

instance IsCard Treachery where
  toCard = toCard . toAttrs

instance HasCardCode TreacheryAttrs where
  toCardCode = treacheryCardCode

instance HasCardCode Treachery where
  toCardCode = toCardCode . toAttrs

instance HasCardDef TreacheryAttrs where
  toCardDef = toCardDef . treacheryCardCode

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

instance Targetable TreacheryAttrs where
  toTarget = TreacheryTarget . treacheryId

instance Targetable Treachery where
  toTarget = toTarget . toAttrs

instance Sourceable TreacheryAttrs where
  toSource = TreacherySource . treacheryId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Treachery where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" TreacheryAttrs TreacheryId where
  getField = treacheryId

instance HasField "id" Treachery TreacheryId where
  getField = toId

instance HasField "placement" TreacheryAttrs Placement where
  getField = treacheryPlacement

instance HasField "placement" Treachery Placement where
  getField = (.placement) . toAttrs

instance HasField "name" TreacheryAttrs Name where
  getField = cdName . toCardDef

instance HasField "name" Treachery Name where
  getField = (.name) . toAttrs
