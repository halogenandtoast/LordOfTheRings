{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Objective.Types (module LordOfTheRings.Objective.Types) where

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
  , EntityId a ~ ObjectiveId
  , EntityAttrs a ~ ObjectiveAttrs
  )
  => IsObjective a

type ObjectiveCard a = CardBuilder ObjectiveId a

data Objective = forall a. (IsObjective a) => Objective a

data SomeObjectiveCard = forall a. (IsObjective a) => SomeObjectiveCard (ObjectiveCard a)

data ObjectiveAttrs = ObjectiveAttrs
  { objectiveId :: ObjectiveId
  , objectiveCardId :: CardId
  , objectiveCardCode :: CardCode
  , objectivePlacement :: Placement
  , objectiveTokens :: Tokens
  , objectiveGuard :: Maybe CardId
  -- ^ the encounter card guarding it; it cannot be claimed while that card is in play
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ObjectiveAttrs

instance ToJSON ObjectiveAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "objective")

instance FromJSON ObjectiveAttrs where
  parseJSON = genericParseJSON (aesonOptions $ Just "objective")

objective :: (ObjectiveAttrs -> a) -> CardDef -> ObjectiveCard a
objective f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId ident ->
        f
          ObjectiveAttrs
            { objectiveId = ident
            , objectiveCardId = cardId
            , objectiveCardCode = cdCardCode cardDef
            , objectivePlacement = Unplaced
            , objectiveTokens = mempty
            , objectiveGuard = Nothing
            }
    }

instance Entity ObjectiveAttrs where
  type EntityId ObjectiveAttrs = ObjectiveId
  type EntityAttrs ObjectiveAttrs = ObjectiveAttrs
  toId = objectiveId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Objective where
  type EntityId Objective = ObjectiveId
  type EntityAttrs Objective = ObjectiveAttrs
  toId = toId . toAttrs
  toAttrs (Objective a) = toAttrs a
  overAttrs f (Objective a) = Objective $ overAttrs f a

instance HasModifiersFor Objective where
  getModifiersFor (Objective a) = getModifiersFor a

instance HasAbilities Objective where
  getAbilities (Objective a) = getAbilities a

instance Show Objective where
  show (Objective a) = show a

instance ToJSON Objective where
  toJSON (Objective a) = toJSON a

instance IsCard ObjectiveAttrs where
  toCard a = EncounterCard $ MkEncounterCard {ecId = objectiveCardId a, ecCardCode = objectiveCardCode a}

instance IsCard Objective where
  toCard = toCard . toAttrs

instance HasCardCode ObjectiveAttrs where
  toCardCode = objectiveCardCode

instance HasCardCode Objective where
  toCardCode = toCardCode . toAttrs

instance HasCardDef ObjectiveAttrs where
  toCardDef = toCardDef . objectiveCardCode

instance HasCardDef Objective where
  toCardDef = toCardDef . toAttrs

instance Targetable ObjectiveAttrs where
  toTarget = ObjectiveTarget . objectiveId

instance Targetable Objective where
  toTarget = toTarget . toAttrs

instance Sourceable ObjectiveAttrs where
  toSource = ObjectiveSource . objectiveId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Objective where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" ObjectiveAttrs ObjectiveId where
  getField = objectiveId

instance HasField "id" Objective ObjectiveId where
  getField = toId

instance HasField "placement" ObjectiveAttrs Placement where
  getField = objectivePlacement

instance HasField "placement" Objective Placement where
  getField = (.placement) . toAttrs

instance HasField "name" ObjectiveAttrs Name where
  getField = cdName . toCardDef

instance HasField "name" Objective Name where
  getField = (.name) . toAttrs

instance HasField "cardId" ObjectiveAttrs CardId where
  getField = objectiveCardId
