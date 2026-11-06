module LordOfTheRings.Decklist (module LordOfTheRings.Decklist) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import LordOfTheRings.Card hiding (ally)
import LordOfTheRings.Contract.CardDefs qualified as Contracts
import LordOfTheRings.Player.Types
import LordOfTheRings.Hero
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Keyword (Keyword (Encounter))
import LordOfTheRings.I18n (ikey, ikeyWith, intVar, textVar)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Id (ScenarioId (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Saga (sagaBarredTitles, sagaOf)
import LordOfTheRings.Trait (Trait (Master))

data DeckList = DeckList
  { heroes :: [CardCode]
  , cards :: [(CardCode, Int)]
  , errata :: Bool
  -- ^ play the deck's cards as the FAQ's errata reads them, not as printed
  , messengerOfTheKingAlly :: Maybe CardCode
  -- ^ RingsDB's intended MotKA ally, still chosen from the deck during contract setup
  , contract :: Maybe CardCode
  -- ^ the contract chosen with the deck, never in it: it does not count toward its size
  -- (MEC77 rules, "Contracts")
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

-- decks stored before the errata toggle play with errata, and those before contracts have none
instance FromJSON DeckList where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("errata", toJSON True), ("contract", Null), ("messengerOfTheKingAlly", Null)]

{- | A deck played with errata, as every deck is unless its player turns them off. A
contract among the cards, as a RingsDB deck list has it, is the deck's contract, not one of
its cards; a second one stays among them, for 'deckProblem' to refuse.
-}
deckList :: [CardCode] -> [(CardCode, Int)] -> DeckList
deckList hs cs = normalizeMessengerOfTheKing $ case break (isContract . fst) cs of
  (before, (code, _) : after) -> DeckList hs (before <> after) True Nothing (Just code)
  _ -> DeckList hs cs True Nothing Nothing
 where
  isContract code = (cdCardType <$> Map.lookup code allPlayerCards) == Just ContractType

-- | RingsDB represents a Messenger ally as a hero with "99" before its ally code.
-- Only actual eligible allies are aliases; an arbitrary unknown code stays unknown.
messengerOfTheKingOriginal :: CardCode -> Maybe CardCode
messengerOfTheKingOriginal (CardCode code) = do
  original <- CardCode <$> T.stripPrefix "99" code
  def <- Map.lookup original allPlayerCards
  guard (eligibleMessengerAlly def)
  pure original

eligibleMessengerAlly :: CardDef -> Bool
eligibleMessengerAlly def = cdCardType def == AllyType && cdUnique def && cdSphere def `elem` map Just [Leadership, Tactics, Spirit, Lore]

-- | The synthetic hero is one physical ally in the draw deck, in addition to any
-- ordinary ally copies in the export. RingsDB repeats heroes in slots; the importer
-- removes that repetition before calling this. Normalizing twice is harmless.
normalizeMessengerOfTheKing :: DeckList -> DeckList
normalizeMessengerOfTheKing dl = case mapMaybe messengerOfTheKingOriginal dl.heroes of
  [ally] | dl.contract == Just (toCardCode Contracts.messengerOfTheKing) ->
    dl
      { heroes = filter (isNothing . messengerOfTheKingOriginal) dl.heroes
      , cards = Map.toList $ Map.fromListWith (+) ((ally, 1) : dl.cards)
      , messengerOfTheKingAlly = Just ally
      }
  _ -> dl

-- | The deck with the contract chosen for it.
withContract :: CardCode -> DeckList -> DeckList
withContract code dl = normalizeMessengerOfTheKing $ dl {contract = Just code}

-- | The reference client's starter deck: Starter Set Leadership.
starterDeck :: DeckList
starterDeck =
  deckList
    ["01001", "01002", "01003"]
    $ [(code, 3) | code <- ["01013", "01014", "01015", "01016", "01017", "01018", "01019", "01020"]]
    <> [(code, 3) | code <- ["01021", "01022", "01023", "01024", "01025", "01026", "01027", "01073"]]
    <> [("02002", 2)]

-- | The core set's four sphere decks: each sphere's heroes and cards, as many of each as
-- the box holds, and one of its four Gandalfs. Then the four starter decks, each as its
-- official deck list has it: three of its heroes and 50 cards, leaving out the sideboard.
starterDecks :: [(Text, DeckList)]
starterDecks =
  [ ("leadership", sphereDeck ["01001", "01002", "01003"] [(13, 3), (14, 2), (15, 2), (16, 3), (17, 2), (18, 2), (19, 1), (20, 2), (21, 2), (22, 2), (23, 2), (24, 2), (25, 1), (26, 2), (27, 1)])
  , ("tactics", sphereDeck ["01004", "01005", "01006"] [(28, 3), (29, 3), (30, 2), (31, 1), (32, 3), (33, 2), (34, 2), (35, 2), (36, 2), (37, 1), (38, 1), (39, 2), (40, 2), (41, 2), (42, 1)])
  , ("spirit", sphereDeck ["01007", "01008", "01009"] [(43, 2), (44, 3), (45, 2), (46, 2), (47, 2), (48, 2), (49, 2), (50, 2), (51, 3), (52, 2), (53, 1), (54, 1), (55, 2), (56, 2), (57, 1)])
  , ("lore", sphereDeck ["01010", "01011", "01012"] [(58, 3), (59, 2), (60, 1), (61, 2), (62, 2), (63, 3), (64, 2), (65, 2), (66, 2), (67, 2), (68, 1), (69, 2), (70, 2), (71, 1), (72, 2)])
  , ("dwarves-of-durin", deckList ["02116", "131004", "03002"] [("04035", 1), ("131009", 2), ("01059", 3), ("03011", 3), ("12066", 3), ("06141", 3), ("01073", 3), ("132006", 2), ("04102", 3), ("01061", 2), ("131011", 2), ("04109", 2), ("132018", 3), ("04061", 2), ("03003", 2), ("131013", 2), ("131014", 2), ("06143", 3), ("04030", 2), ("04129", 3), ("03004", 2)])
  , ("elves-of-lorien", deckList ["08001", "08112", "08056"] [("08065", 3), ("08063", 3), ("08117", 3), ("01073", 2), ("16003", 3), ("01060", 2), ("02123", 2), ("08002", 3), ("08114", 2), ("06037", 2), ("02102", 2), ("08064", 2), ("08121", 3), ("08058", 3), ("08092", 2), ("01050", 2), ("04108", 2), ("04059", 2), ("08027", 3), ("22036", 2), ("08009", 2)])
  , ("defenders-of-gondor", deckList ["05002", "02050", "08084"] [("22028", 2), ("22002", 3), ("05004", 2), ("05007", 3), ("06135", 2), ("05003", 2), ("01014", 2), ("01073", 2), ("01029", 2), ("05018", 3), ("06108", 3), ("08140", 1), ("06005", 2), ("08113", 1), ("01026", 2), ("22083", 3), ("06136", 3), ("05008", 2), ("01034", 2), ("22029", 2), ("22003", 2), ("01024", 2), ("131015", 2)])
  , ("riders-of-rohan", deckList ["07001", "17055", "22027"] [("02030", 3), ("02055", 3), ("01073", 2), ("06110", 1), ("01030", 2), ("17112", 3), ("02121", 3), ("02006", 2), ("07007", 2), ("22142", 3), ("07006", 3), ("02056", 2), ("08004", 2), ("22035", 2), ("17058", 1), ("22032", 2), ("01050", 2), ("02122", 2), ("01034", 2), ("06138", 2), ("17085", 2), ("22062", 2), ("17005", 2)])
  ]
 where
  sphereDeck heroes cards = deckList heroes $ [(coreCode n, k) | (n, k) <- cards] <> [("01073", 1)]
  coreCode :: Int -> CardCode
  coreCode n = fromString $ "01" <> replicate (3 - length (show n)) '0' <> show n

newtype DeckException = DeckException Text
  deriving newtype (Show)

lookupPlayerCardDef :: CardCode -> Either DeckException CardDef
lookupPlayerCardDef cCode =
  maybe (Left $ DeckException $ "Missing card: " <> tshow cCode) Right $ Map.lookup cCode allPlayerCards

-- | Include the ally the imported deck intends to turn into a hero when checking
-- uniqueness and scenario restrictions, without counting it as a starting hero.
intendedHeroes :: DeckList -> [CardCode]
intendedHeroes dl = dl.heroes <> maybeToList dl.messengerOfTheKingAlly

-- | Hero titles brought by more than one deck. Heroes are unique, so no two players may
-- control heroes with the same title, whichever versions they are.
sharedHeroes :: [DeckList] -> [Text]
sharedHeroes deckLists = [title | (title, n) <- mapToList counts, n > (1 :: Int)]
 where
  counts = Map.fromListWith (+) [(title, 1) | dl <- deckLists, title <- ordNub (mapMaybe heroTitle (intendedHeroes dl))]
  heroTitle code = (\(Name title _) -> title) . (.cdName) <$> Map.lookup code allPlayerCards

-- | Hero titles the decks bring that the scenario's rules keep out of play: The Fords of
-- Isen's Gríma objective-ally is unique, so no player can use the Gríma hero there.
barredHeroes :: Text -> Bool -> [DeckList] -> [Text]
barredHeroes scenarioId inCampaign deckLists =
  ordNub $ [t | t <- barred, t `elem` titles] <> [t | t <- sagaBarredTitles (ScenarioId scenarioId) inCampaign, t `elem` (titles <> cardTitles)] <> sagaHeroes <> masterCards
 where
  -- "Cards with the Master trait cannot be used when playing saga expansions" (A Shadow
  -- in the East rules insert, "The One Ring"): The One Ring itself is one
  masterCards =
    if isJust (sagaOf $ ScenarioId scenarioId)
      then ordNub [title | dl <- deckLists, (code, _) <- dl.cards, Just d <- [Map.lookup code allPlayerCards], Master `member` cdCardTraits d, let Name title _ = d.cdName]
      else []
  -- a Baggins or Fellowship sphere hero comes from the scenario, never from a deck
  sagaHeroes = [title | dl <- deckLists, Just d <- map (`Map.lookup` allPlayerCards) (intendedHeroes dl), cdSphere d `elem` [Just Fellowship, Just Baggins], let Name title _ = d.cdName]
  cardTitles = [title | dl <- deckLists, (code, _) <- dl.cards, Just d <- [Map.lookup code allPlayerCards], let Name title _ = d.cdName]
  barred = case scenarioId of
    "the-fords-of-isen" -> ["Gríma"]
    -- nor Sméagol in the Shadows of Mirkwood cycle (MEC77 rules), nor in the sagas
    "the-hunt-for-gollum" -> ["Sméagol"]
    "conflict-at-the-carrock" -> ["Sméagol"]
    "a-journey-to-rhosgobel" -> ["Sméagol"]
    "the-hills-of-emyn-muil" -> ["Sméagol"]
    "the-dead-marshes" -> ["Sméagol"]
    "return-to-mirkwood" -> ["Sméagol"]
    _ -> []
  titles = [title | dl <- deckLists, Just d <- map (`Map.lookup` allPlayerCards) (intendedHeroes dl), let Name title _ = d.cdName]

-- | The most cards a deck may have in the scenario, its rules setting one: "Player decks
-- cannot exceed 50 cards" (Assault on Dol Rhugar, The Fortress of Nurn's setup).
scenarioDeckLimit :: Text -> Maybe Int
scenarioDeckLimit = \case
  "the-fortress-of-nurn" -> Just 50
  _ -> Nothing

-- | The decks over the scenario's limit, by their size.
oversizedDecks :: Text -> [DeckList] -> Maybe (Int, Int)
oversizedDecks scenarioId deckLists = do
  limit <- scenarioDeckLimit scenarioId
  size <- headMay [n | dl <- deckLists, let n = sum (map snd dl.cards), n > limit]
  pure (limit, size)

-- | Why the deck breaks the deckbuilding rules, if it does: 1 to 3 heroes with no two
-- sharing a title, and no more copies of a title than its deck limit, 3 for most cards
-- and 1 for those limited to 1 per deck. Cards the game does not know are not counted.
-- Its contract's restrictions must be met too (MEC77 rules, "Contracts").
deckProblem :: DeckList -> Maybe Text
deckProblem dl
  | Just ally <- dl.messengerOfTheKingAlly
  , dl.contract /= Just (toCardCode Contracts.messengerOfTheKing)
      || not (maybe False eligibleMessengerAlly $ Map.lookup ally allPlayerCards)
      || not (any (\(code, n) -> code == ally && n > 0) dl.cards) = Just $ ikeyWith "error.unknownCards" [textVar "cards" (tshow ally)]
  | notNull contractCards = Just $ ikey "error.oneContract"
  | null dl.heroes || length dl.heroes > maxHeroes = Just $ ikey "error.heroCount"
  | notNull twice = Just $ ikeyWith "error.duplicateHeroes" [textVar "heroes" (intercalate ", " twice)]
  | notNull over = Just $ ikeyWith "error.overDeckLimit" [textVar "cards" (intercalate ", " over)]
  | otherwise = contractProblem =<< (defOf =<< dl.contract)
 where
  title def = let Name t _ = cdName def in t
  defOf code = Map.lookup code allPlayerCards
  defs = [(d, n) | (code, n) <- dl.cards, Just d <- [defOf code]]
  heroDefs = mapMaybe defOf dl.heroes
  intendedHeroDefs = mapMaybe defOf (intendedHeroes dl)
  contractCards = [d | (d, _) <- defs, cdCardType d == ContractType]
  -- Bond of Friendship allows a fourth
  maxHeroes = if dl.contract == Just (toCardCode Contracts.bondOfFriendship) then 4 else 3
  twice = [t | (t, n) <- mapToList (Map.fromListWith (+) [(title d, 1 :: Int) | d <- intendedHeroDefs]), n > 1]
  copies = Map.fromListWith (\(a, l) (b, l') -> (a + b, min l l')) [(title d, (n, cdDeckLimit d)) | (d, n) <- defs]
  over = [t | (t, (n, limit)) <- mapToList copies, n > limit]
  -- treasures do not count toward a deck's 50 cards (MEC115 rules, "Treasure")
  deckSize = sum [n | (d, n) <- defs, not (isTreasure d)]
  named c = [textVar "contract" (display $ cdName c)]
  barred c key = Just $ ikeyWith key (named c)
  copyLimit c limit = case [t | (t, (n, _)) <- mapToList copies, n > limit] of
    [] -> Nothing
    ts -> Just $ ikeyWith "error.contractCopies" (named c <> [intVar "n" limit, textVar "cards" (intercalate ", " ts)])
  heroLimit c limit
    | length dl.heroes > limit = Just $ ikeyWith "error.contractHeroCount" (named c <> [intVar "n" limit])
    | otherwise = Nothing
  contractProblem c = case toCardCode c of
    code | code == toCardCode Contracts.aPerilousVoyage ->
      if deckSize < 100
        then Just $ ikeyWith "error.contractMinimumDeckSize" (named c <> [intVar "n" 100, intVar "count" deckSize])
        else Nothing
    -- "Your deck cannot include ally cards."
    code | code == toCardCode Contracts.forthTheThreeHunters -> if any ((== AllyType) . cdCardType . fst) defs then barred c "error.contractBarsAllies" else Nothing
    -- "You cannot include attachments in your deck."
    code | code == toCardCode Contracts.theBurglarsTurn -> if any ((== AttachmentType) . cdCardType . fst) defs then barred c "error.contractBarsAttachments" else Nothing
    -- "You cannot choose more than 1 starting hero."
    code | code == toCardCode Contracts.theGreyWanderer -> heroLimit c 1
    -- "You cannot choose more than 2 heroes during setup."
    code | code == toCardCode Contracts.messengerOfTheKing -> heroLimit c 2
    -- "You cannot include more than 1 copy of a card, by title, in your deck."
    code | code == toCardCode Contracts.councilOfTheWise -> copyLimit c 1
    -- "Your deck must be exactly 50 players cards and include exactly 10 cards from each
    -- of these four spheres: Leadership, Lore, Spirit, and Tactics. You cannot include
    -- more than 2 copies of any card by title in your deck. ... Each of your heroes must
    -- belong to a different sphere."
    code | code == toCardCode Contracts.bondOfFriendship ->
      let spheres = mapMaybe cdSphere heroDefs
          inSphere sphere = sum [n | (d, n) <- defs, cdSphere d == Just sphere]
          short = [tshow sphere | sphere <- [Leadership, Lore, Spirit, Tactics], inSphere sphere /= 10]
       in if
            | deckSize /= 50 -> Just $ ikeyWith "error.contractDeckSize" (named c <> [intVar "n" 50, intVar "count" deckSize])
            | notNull short -> Just $ ikeyWith "error.contractSphereCount" (named c <> [intVar "n" 10, textVar "spheres" (intercalate ", " short)])
            | length (ordNub spheres) /= length spheres -> Just $ ikeyWith "error.contractHeroSpheres" (named c)
            | otherwise -> copyLimit c 2
    _ -> Nothing

loadPlayer :: (MonadRandom m) => Text -> DeckList -> m (Either DeckException (Player, [Hero]))
loadPlayer name dl = do
  pid <- getRandom
  let defs =
        (,,)
          <$> traverse lookupPlayerCardDef dl.heroes
          <*> (concat <$> traverse (\(cCode, n) -> replicate n <$> lookupPlayerCardDef cCode) dl.cards)
          <*> traverse lookupPlayerCardDef dl.contract
  for defs \(heroDefs, cardDefs, contractDef) -> do
    heroes <- traverse (newHero pid <=< (`genPlayerCard` Just pid)) heroDefs
    cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) cardDefs
    -- a card with the Encounter keyword is set aside out of play, not dealt into the deck
    let (setAside, deck) = partition (member Encounter . cdKeywords . toCardDef) cards
    contractCard <- traverse (`genPlayerCard` Just pid) contractDef
    pure
      ((newPlayer pid name deck) {playerDeckbuildingCards = dl.cards, playerErrata = dl.errata, playerStartingHeroes = dl.heroes, playerMessengerOfTheKingAlly = dl.messengerOfTheKingAlly, playerSetAsideCards = setAside, playerContract = contractCard}, heroes)
