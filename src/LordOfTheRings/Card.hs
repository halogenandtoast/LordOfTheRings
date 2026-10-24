{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Card (module LordOfTheRings.Card, module X) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs (allAllyCards)
import LordOfTheRings.Attachment.CardDefs (allAttachmentCards)
import LordOfTheRings.Card.CardDef as X
import LordOfTheRings.Card.CardType as X
import LordOfTheRings.Card.Code as X
import LordOfTheRings.Card.EncounterCard as X
import LordOfTheRings.Card.PlayerCard as X
import LordOfTheRings.Card.Synthetic (syntheticCards)
import LordOfTheRings.Enemy.CardDefs (allEnemyCards)
import LordOfTheRings.Event.CardDefs (allEventCards)
import LordOfTheRings.Hero.CardDefs (allHeroCards)
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs (allLocationCards)
import LordOfTheRings.Name
import LordOfTheRings.Objective.CardDefs (allObjectiveCards)
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs (allQuestCards)
import LordOfTheRings.Sphere as X
import LordOfTheRings.Target
import LordOfTheRings.Treachery.CardDefs (allTreacheryCards)

class IsCard a where
  toCard :: a -> Card

instance IsCard Card where
  toCard = id

class Display a where
  display :: a -> Text

instance Display Name where
  display (Name title mSubtitle) = case mSubtitle of
    Nothing -> title
    Just subtitle -> title <> ": " <> subtitle

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  deriving stock (Show, Eq, Generic)

instance ToJSON Card where
  toJSON = \case
    PlayerCard pc -> toJSON pc
    EncounterCard ec -> toJSON ec

-- | Player cards carry an owner and encounter cards do not.
instance FromJSON Card where
  parseJSON = withObject "Card" \o ->
    if KeyMap.member "owner" o
      then PlayerCard <$> parseJSON (Object o)
      else EncounterCard <$> parseJSON (Object o)

allPlayerCards :: Map CardCode CardDef
allPlayerCards = Map.unions [allHeroCards, allAllyCards, allAttachmentCards, allEventCards]

allEncounterCards :: Map CardCode CardDef
allEncounterCards =
  Map.unions
    [ allEnemyCards
    , allLocationCards
    , allTreacheryCards
    , allQuestCards
    , allObjectiveCards
    , syntheticCards
    ]

lookupCardDef :: CardCode -> Maybe CardDef
lookupCardDef cCode = Map.lookup cCode allPlayerCards <|> Map.lookup cCode allEncounterCards

instance HasCardDef CardCode where
  toCardDef cCode = fromJustNote ("missing card def: " <> show cCode) $ lookupCardDef cCode

instance HasCardDef PlayerCard where
  toCardDef = toCardDef . toCardCode

instance HasCardDef EncounterCard where
  toCardDef = toCardDef . toCardCode

instance HasCardCode Card where
  toCardCode = \case
    PlayerCard pc -> toCardCode pc
    EncounterCard ec -> toCardCode ec

instance HasCardDef Card where
  toCardDef = toCardDef . toCardCode

instance Targetable Card where
  toTarget = CardIdTarget . toCardId

instance Targetable PlayerCard where
  toTarget = CardIdTarget . pcId

instance Targetable EncounterCard where
  toTarget = CardIdTarget . ecId

toCardId :: Card -> CardId
toCardId = \case
  PlayerCard pc -> pcId pc
  EncounterCard ec -> ecId ec

instance HasField "id" Card CardId where
  getField = toCardId

instance HasField "name" Card Name where
  getField = cdName . toCardDef

instance HasField "unique" Card Bool where
  getField = cdUnique . toCardDef

instance HasField "sphere" Card (Maybe Sphere) where
  getField = cdSphere . toCardDef

instance HasField "cost" Card (Maybe CardCost) where
  getField = cdCost . toCardDef

instance HasField "cardType" Card CardType where
  getField = cdCardType . toCardDef

costAsResources :: CardCost -> Int
costAsResources = \case
  StaticCost n -> n
  DynamicCost -> 0

genPlayerCard :: MonadRandom m => CardDef -> Maybe PlayerId -> m PlayerCard
genPlayerCard def owner = do
  cardId <- getRandom
  pure $ MkPlayerCard {pcId = cardId, pcCardCode = cdCardCode def, pcOwner = owner}

genEncounterCard :: MonadRandom m => CardDef -> m EncounterCard
genEncounterCard def = do
  cardId <- getRandom
  pure $ MkEncounterCard {ecId = cardId, ecCardCode = cdCardCode def}

data CardBuilder ident a = CardBuilder
  { cbCardDef :: CardDef
  , cbCardBuilder :: CardId -> ident -> a
  }

instance HasCardCode (CardBuilder ident a) where
  toCardCode = cdCardCode . cbCardDef

instance Functor (CardBuilder ident) where
  fmap f CardBuilder {..} = CardBuilder {cbCardDef, cbCardBuilder = \cardId ident -> f (cbCardBuilder cardId ident)}
