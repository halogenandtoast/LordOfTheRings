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

-- | The reference client's starter deck: Starter Set Leadership.
starterDeck :: DeckList
starterDeck =
  DeckList
    ["01001", "01002", "01003"]
    $ [(code, 3) | code <- ["01013", "01014", "01015", "01016", "01017", "01018", "01019", "01020"]]
    <> [(code, 3) | code <- ["01021", "01022", "01023", "01024", "01025", "01026", "01027", "01073"]]
    <> [("02002", 2)]

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
