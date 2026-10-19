module LordOfTheRings.Decklist (module LordOfTheRings.Decklist) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Player.Types
import LordOfTheRings.Hero
import LordOfTheRings.Prelude

data DeckList = DeckList
  { heroes :: [CardCode]
  , cards :: [(CardCode, Int)]
  }

starterDeck :: DeckList
starterDeck =
  DeckList
    ["01012", "01007", "01002"]
    [ ("01031", 1)
    , ("01059", 2)
    , ("01014", 2)
    , ("01073", 3)
    , ("01062", 2)
    , ("01013", 3)
    , ("01060", 1)
    , ("01061", 2)
    , ("01045", 2)
    , ("01017", 2)
    , ("01016", 3)
    , ("01015", 2)
    , ("01027", 1)
    , ("01069", 2)
    , ("01070", 2)
    , ("01072", 2)
    , ("01026", 2)
    , ("01057", 1)
    , ("01050", 2)
    , ("01053", 1)
    , ("01020", 2)
    , ("01025", 1)
    , ("01048", 2)
    , ("01064", 1)
    , ("01023", 2)
    , ("01051", 2)
    , ("01046", 2)
    ]

newtype DeckException = DeckException Text
  deriving newtype (Show)

lookupPlayerCardDef :: CardCode -> Either DeckException CardDef
lookupPlayerCardDef cCode =
  maybe (Left $ DeckException $ "Missing card: " <> tshow cCode) Right $ Map.lookup cCode allPlayerCards

loadPlayer :: (MonadRandom m) => DeckList -> m (Either DeckException (Player, [Hero]))
loadPlayer dl = do
  pid <- getRandom
  let defs =
        (,)
          <$> traverse lookupPlayerCardDef dl.heroes
          <*> (concat <$> traverse (\(cCode, n) -> replicate n <$> lookupPlayerCardDef cCode) dl.cards)
  for defs \(heroDefs, cardDefs) -> do
    heroes <- traverse (newHero pid <=< (`genPlayerCard` Just pid)) heroDefs
    deck <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) cardDefs
    pure
      (newPlayer pid "halogen64" deck, heroes)
