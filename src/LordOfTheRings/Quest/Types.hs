{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Quest.Types (module LordOfTheRings.Quest.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Name
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
  , EntityId a ~ QuestId
  , EntityAttrs a ~ QuestAttrs
  )
  => IsQuest a

type QuestCard a = CardBuilder QuestId a

data Quest = forall a. (IsQuest a) => Quest a

data SomeQuestCard = forall a. (IsQuest a) => SomeQuestCard (QuestCard a)

data QuestAttrs = QuestAttrs
  { questId :: QuestId
  , questCardId :: CardId
  , questCardCode :: CardCode
  , questTokens :: Tokens
  , questFlipped :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''QuestAttrs

instance ToJSON QuestAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "quest")

quest :: (QuestAttrs -> a) -> CardDef -> QuestCard a
quest f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId ident ->
        f
          QuestAttrs
            { questId = ident
            , questCardId = cardId
            , questCardCode = cdCardCode cardDef
            , questTokens = mempty
            , questFlipped = False
            }
    }

instance Entity QuestAttrs where
  type EntityId QuestAttrs = QuestId
  type EntityAttrs QuestAttrs = QuestAttrs
  toId = questId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Quest where
  type EntityId Quest = QuestId
  type EntityAttrs Quest = QuestAttrs
  toId = toId . toAttrs
  toAttrs (Quest a) = toAttrs a
  overAttrs f (Quest a) = Quest $ overAttrs f a

instance HasModifiersFor Quest where
  getModifiersFor (Quest a) = getModifiersFor a

instance HasAbilities Quest where
  getAbilities (Quest a) = getAbilities a

instance Show Quest where
  show (Quest a) = show a

instance ToJSON Quest where
  toJSON (Quest a) = toJSON a

instance IsCard QuestAttrs where
  toCard a = EncounterCard $ MkEncounterCard {ecId = questCardId a, ecCardCode = questCardCode a}

instance IsCard Quest where
  toCard = toCard . toAttrs

instance HasCardCode QuestAttrs where
  toCardCode = questCardCode

instance HasCardCode Quest where
  toCardCode = toCardCode . toAttrs

instance HasCardDef QuestAttrs where
  toCardDef = toCardDef . questCardCode

instance HasCardDef Quest where
  toCardDef = toCardDef . toAttrs

instance Targetable QuestAttrs where
  toTarget = QuestTarget . questId

instance Targetable Quest where
  toTarget = toTarget . toAttrs

instance Sourceable QuestAttrs where
  toSource = QuestSource . questId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Quest where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" QuestAttrs QuestId where
  getField = questId

instance HasField "id" Quest QuestId where
  getField = toId

instance HasField "name" QuestAttrs Name where
  getField = cdName . toCardDef

instance HasField "name" Quest Name where
  getField = (.name) . toAttrs

instance HasField "progress" QuestAttrs Int where
  getField = countTokens Progress . questTokens

instance HasField "progress" Quest Int where
  getField = (.progress) . toAttrs

instance HasField "questPoints" QuestAttrs Int where
  getField = fromMaybe 0 . cdQuestPoints . toCardDef

instance HasField "questPoints" Quest Int where
  getField = (.questPoints) . toAttrs

instance HasField "stage" QuestAttrs Int where
  getField = fromMaybe 1 . cdStage . toCardDef

instance HasField "stage" Quest Int where
  getField = (.stage) . toAttrs

instance HasField "flipped" QuestAttrs Bool where
  getField = questFlipped

instance HasField "flipped" Quest Bool where
  getField = (.flipped) . toAttrs

instance HasField "cardId" QuestAttrs CardId where
  getField = questCardId
