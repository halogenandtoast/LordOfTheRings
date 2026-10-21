module LordOfTheRings.Decklist (module LordOfTheRings.Decklist) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Player.Types
import LordOfTheRings.Hero
import LordOfTheRings.I18n (ikey, ikeyWith, textVar)
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
-- the box holds, and one of its four Gandalfs. Then the four starter decks, each as its
-- box holds it, with the first three of its heroes.
starterDecks :: [(Text, DeckList)]
starterDecks =
  [ ("leadership", sphereDeck ["01001", "01002", "01003"] [(13, 3), (14, 2), (15, 2), (16, 3), (17, 2), (18, 2), (19, 1), (20, 2), (21, 2), (22, 2), (23, 2), (24, 2), (25, 1), (26, 2), (27, 1)])
  , ("tactics", sphereDeck ["01004", "01005", "01006"] [(28, 3), (29, 3), (30, 2), (31, 1), (32, 3), (33, 2), (34, 2), (35, 2), (36, 2), (37, 1), (38, 1), (39, 2), (40, 2), (41, 2), (42, 1)])
  , ("spirit", sphereDeck ["01007", "01008", "01009"] [(43, 2), (44, 3), (45, 2), (46, 2), (47, 2), (48, 2), (49, 2), (50, 2), (51, 3), (52, 2), (53, 1), (54, 1), (55, 2), (56, 2), (57, 1)])
  , ("lore", sphereDeck ["01010", "01011", "01012"] [(58, 3), (59, 2), (60, 1), (61, 2), (62, 2), (63, 3), (64, 2), (65, 2), (66, 2), (67, 2), (68, 1), (69, 2), (70, 2), (71, 1), (72, 2)])
  , ("dwarves-of-durin", DeckList ["02116", "131004", "03002"] [("04035", 3), ("131009", 3), ("01059", 3), ("03011", 3), ("12066", 3), ("06141", 3), ("01073", 3), ("132006", 3), ("04102", 3), ("01061", 2), ("131011", 3), ("04109", 3), ("132018", 3), ("04061", 3), ("03003", 3), ("131013", 3), ("131014", 3), ("06143", 3), ("04030", 3), ("04129", 3), ("03004", 3), ("131007", 3), ("131006", 3), ("03009", 3), ("16004", 3), ("17007", 3), ("17143", 1)])
  , ("elves-of-lorien", DeckList ["08001", "08112", "08056"] [("08065", 3), ("08063", 3), ("08117", 3), ("01073", 2), ("16003", 3), ("01060", 2), ("02123", 3), ("08002", 3), ("08114", 3), ("06037", 3), ("02102", 3), ("08064", 3), ("08121", 3), ("08058", 3), ("08092", 3), ("01050", 2), ("04108", 3), ("04059", 3), ("08027", 3), ("22036", 3), ("08009", 3), ("08087", 3), ("08028", 3), ("08088", 3), ("04131", 3), ("08060", 3), ("04137", 3)])
  , ("defenders-of-gondor", DeckList ["05002", "02050", "08084"] [("22028", 3), ("22002", 3), ("05004", 3), ("05007", 3), ("06135", 3), ("05003", 3), ("01014", 2), ("01073", 2), ("01029", 2), ("05018", 3), ("06108", 3), ("08140", 3), ("06005", 3), ("08113", 3), ("01026", 2), ("22083", 3), ("06136", 3), ("05008", 3), ("01034", 2), ("22029", 3), ("22003", 3), ("01024", 2), ("131015", 3), ("17141", 3), ("05017", 3), ("06064", 3), ("17009", 3), ("22090", 1), ("04103", 1)])
  , ("riders-of-rohan", DeckList ["07001", "17055", "22027"] [("02030", 3), ("02055", 3), ("01073", 2), ("06110", 3), ("01030", 2), ("17112", 3), ("02121", 3), ("02006", 3), ("07007", 3), ("22142", 3), ("07006", 3), ("02056", 3), ("08004", 3), ("22035", 3), ("17058", 1), ("22032", 3), ("01050", 2), ("02122", 3), ("01034", 2), ("06138", 3), ("17085", 3), ("22062", 3), ("17005", 3), ("02077", 3), ("06137", 3), ("06139", 3), ("07008", 3), ("08116", 3)])
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

-- | Why the deck breaks the deckbuilding rules, if it does: 1 to 3 heroes with no two
-- sharing a title, and no more copies of a title than its deck limit, 3 for most cards
-- and 1 for those limited to 1 per deck. Cards the game does not know are not counted.
deckProblem :: DeckList -> Maybe Text
deckProblem dl
  | null dl.heroes || length dl.heroes > 3 = Just $ ikey "error.heroCount"
  | notNull twice = Just $ ikeyWith "error.duplicateHeroes" [textVar "heroes" (intercalate ", " twice)]
  | notNull over = Just $ ikeyWith "error.overDeckLimit" [textVar "cards" (intercalate ", " over)]
  | otherwise = Nothing
 where
  title def = let Name t _ = cdName def in t
  defOf code = Map.lookup code allPlayerCards
  twice = [t | (t, n) <- mapToList (Map.fromListWith (+) [(title d, 1 :: Int) | Just d <- map defOf dl.heroes]), n > 1]
  copies = Map.fromListWith (\(a, l) (b, l') -> (a + b, min l l')) [(title d, (n, cdDeckLimit d)) | (code, n) <- dl.cards, Just d <- [defOf code]]
  over = [t | (t, (n, limit)) <- mapToList copies, n > limit]

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
