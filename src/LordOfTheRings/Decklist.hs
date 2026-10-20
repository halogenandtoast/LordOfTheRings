module LordOfTheRings.Decklist (module LordOfTheRings.Decklist) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Player.Types
import LordOfTheRings.Hero
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude

data DeckList = DeckList
  { heroes :: [CardCode]
  , cards :: [(CardCode, Int)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The reference client's starter deck: Starter Set Leadership.
starterDeck :: DeckList
starterDeck =
  DeckList
    ["01001", "01002", "01003"]
    $ [(code, 3) | code <- ["01013", "01014", "01015", "01016", "01017", "01018", "01019", "01020"]]
    <> [(code, 3) | code <- ["01021", "01022", "01023", "01024", "01025", "01026", "01027", "01073"]]
    <> [("02002", 2)]

-- | The core set's four sphere decks: each sphere's heroes and cards, as many of each as
-- the box holds, and one of its four Gandalfs.
coreStarterDecks :: [(Text, DeckList)]
coreStarterDecks =
  [ ("leadership", sphereDeck ["01001", "01002", "01003"] [(13, 3), (14, 2), (15, 2), (16, 3), (17, 2), (18, 2), (19, 1), (20, 2), (21, 2), (22, 2), (23, 2), (24, 2), (25, 1), (26, 2), (27, 1)])
  , ("tactics", sphereDeck ["01004", "01005", "01006"] [(28, 3), (29, 3), (30, 2), (31, 1), (32, 3), (33, 2), (34, 2), (35, 2), (36, 2), (37, 1), (38, 1), (39, 2), (40, 2), (41, 2), (42, 1)])
  , ("spirit", sphereDeck ["01007", "01008", "01009"] [(43, 2), (44, 3), (45, 2), (46, 2), (47, 2), (48, 2), (49, 2), (50, 2), (51, 3), (52, 2), (53, 1), (54, 1), (55, 2), (56, 2), (57, 1)])
  , ("lore", sphereDeck ["01010", "01011", "01012"] [(58, 3), (59, 2), (60, 1), (61, 2), (62, 2), (63, 3), (64, 2), (65, 2), (66, 2), (67, 2), (68, 1), (69, 2), (70, 2), (71, 1), (72, 2)])
  ]
 where
  sphereDeck heroes cards = DeckList heroes $ [(coreCode n, k) | (n, k) <- cards] <> [("01073", 1)]
  coreCode :: Int -> CardCode
  coreCode n = fromString $ "01" <> replicate (3 - length (show n)) '0' <> show n

newtype DeckException = DeckException Text
  deriving newtype (Show)

lookupPlayerCardDef :: CardCode -> Either DeckException CardDef
lookupPlayerCardDef cCode =
  maybe (Left $ DeckException $ "Missing card: " <> tshow cCode) Right $ Map.lookup cCode allPlayerCards

-- | Hero titles brought by more than one deck. Heroes are unique, so no two players may
-- control heroes with the same title, whichever versions they are.
sharedHeroes :: [DeckList] -> [Text]
sharedHeroes deckLists = [title | (title, n) <- mapToList counts, n > (1 :: Int)]
 where
  counts = Map.fromListWith (+) [(title, 1) | dl <- deckLists, title <- ordNub (mapMaybe heroTitle dl.heroes)]
  heroTitle code = (\(Name title _) -> title) . (.cdName) <$> Map.lookup code allPlayerCards

loadPlayer :: (MonadRandom m) => Text -> DeckList -> m (Either DeckException (Player, [Hero]))
loadPlayer name dl = do
  pid <- getRandom
  let defs =
        (,)
          <$> traverse lookupPlayerCardDef dl.heroes
          <*> (concat <$> traverse (\(cCode, n) -> replicate n <$> lookupPlayerCardDef cCode) dl.cards)
  for defs \(heroDefs, cardDefs) -> do
    heroes <- traverse (newHero pid <=< (`genPlayerCard` Just pid)) heroDefs
    deck <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) cardDefs
    pure
      (newPlayer pid name deck, heroes)
