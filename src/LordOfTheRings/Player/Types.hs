{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Player.Types (module LordOfTheRings.Player.Types) where

import LordOfTheRings.Ability.Types (UsedAbility)
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

data Player = Player
  { playerId :: PlayerId
  , playerName :: Text
  , playerHand :: [Card]
  , playerDeck :: [Card]
  , playerDiscard :: [Card]
  , playerThreat :: Int
  , playerStartingThreat :: Int
  -- ^ the threat the player began the game at, as Aragorn's Refresh Action reads
  , playerEliminated :: Bool
  , playerUsedAbilities :: [UsedAbility]
  , playerErrata :: Bool
  -- ^ the player's cards follow the FAQ's errata, as their deck was loaded
  , playerStartingHeroes :: [CardCode]
  -- ^ the heroes the player's deck began with, as the games list shows them
  , playerSetAsideCards :: [Card]
  {- ^ the cards with the Encounter keyword the player's deck set aside, as Ranger of the
  North: one Ranger Summons shuffles into the encounter deck stays here, known as the
  player's, until it is revealed
  -}
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''Player

instance ToJSON Player where
  toJSON = genericToJSON (aesonOptions $ Just "player")

instance FromJSON Player where
  parseJSON = genericParseJSON (aesonOptions $ Just "player") . withDefaults [("usedAbilities", toJSON ([] :: [UsedAbility])), ("startingThreat", toJSON (0 :: Int)), ("errata", toJSON True), ("startingHeroes", toJSON ([] :: [CardCode])), ("setAsideCards", toJSON ([] :: [Card]))]

newPlayer :: PlayerId -> Text -> [Card] -> Player
newPlayer pid name deck =
  Player
    { playerId = pid
    , playerName = name
    , playerHand = []
    , playerDeck = deck
    , playerDiscard = []
    , playerThreat = 0
    , playerStartingThreat = 0
    , playerEliminated = False
    , playerUsedAbilities = []
    , playerErrata = True
    , playerStartingHeroes = []
    , playerSetAsideCards = []
    }

instance Entity Player where
  type EntityId Player = PlayerId
  type EntityAttrs Player = Player
  toId = playerId
  toAttrs a = a
  overAttrs f a = f a

instance Targetable Player where
  toTarget = PlayerTarget . playerId

instance Sourceable Player where
  toSource = PlayerSource . playerId

instance HasField "id" Player PlayerId where
  getField = playerId

instance HasField "name" Player Text where
  getField = playerName

instance HasField "hand" Player [Card] where
  getField = playerHand

instance HasField "deck" Player [Card] where
  getField = playerDeck

instance HasField "discard" Player [Card] where
  getField = playerDiscard

instance HasField "threat" Player Int where
  getField = playerThreat

instance HasField "startingThreat" Player Int where
  getField = playerStartingThreat

instance HasField "errata" Player Bool where
  getField = playerErrata

instance HasField "usedAbilities" Player [UsedAbility] where
  getField = playerUsedAbilities

instance HasField "eliminated" Player Bool where
  getField = playerEliminated

instance HasField "startingHeroes" Player [CardCode] where
  getField = playerStartingHeroes

instance HasField "setAsideCards" Player [Card] where
  getField = playerSetAsideCards

{- | The cards in the player's hand that can leave it: an encounter card drawn into it, as
Ill Tidings, cannot.
-}
discardableHand :: Player -> [Card]
discardableHand p = [c | c@(PlayerCard _) <- p.hand]
