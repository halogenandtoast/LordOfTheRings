module LordOfTheRings.Location.Cards.DoorsOfDurin (doorsOfDurin) where

import Data.Text qualified as T
import LordOfTheRings.Card (CardDef (cdName), toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Name (Name (..))

newtype DoorsOfDurin = DoorsOfDurin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

doorsOfDurin :: LocationCard DoorsOfDurin
doorsOfDurin = location DoorsOfDurin Cards.doorsOfDurin

-- | Action: Each player may discard any number of cards from his hand. Then, discard the
-- top card of the encounter deck. If the first letter of the encounter card's title
-- matches that of one of the discarded player cards, add Doors of Durin to your victory
-- display. (Limit once per round.)
instance HasAbilities DoorsOfDurin where
  getAbilities (DoorsOfDurin a) = [limited (GroupLimit PerRound 1) $ anyPlayer $ mkAbility a 1 $ ActionAbility Free]

-- | Progress tokens that would be placed on Doors of Durin are instead placed on the
-- current quest card.
instance RunMessage DoorsOfDurin where
  runMessage msg l@(DoorsOfDurin a) = case msg of
    PlaceTokens source (isTarget a -> True) Progress n -> do
      placeProgressOnCurrentQuest source n
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayerMayDiscardFromHand a
      pure l
    EachDiscardAnyFromHand (isSource a -> True) [] discarded -> do
      -- the card on top now is the one discarded
      top <- getTopOfEncounterDeck
      discardTopOfEncounterDeck a 1
      for_ top \card ->
        when (initial (toCardDef card) `elem` map (initial . toCardDef) discarded) $ addToVictoryDisplay a
      pure l
    _ -> DoorsOfDurin <$> runMessage msg a
   where
    initial :: CardDef -> Maybe Char
    initial def = let Name title _ = cdName def in unaccent <$> headMay (T.unpack $ T.toLower title)
    -- every Latin letter with a diacritic, lowercased, as a title may start with
    unaccent :: Char -> Char
    unaccent ch = maybe ch snd $ find ((ch `elem`) . fst) folds
    folds :: [(String, Char)]
    folds =
      [ ("àáâãäåāăą", 'a')
      , ("çćĉċč", 'c')
      , ("ďđ", 'd')
      , ("èéêëēĕėęě", 'e')
      , ("ĝğġģ", 'g')
      , ("ĥħ", 'h')
      , ("ìíîïĩīĭįı", 'i')
      , ("ĵ", 'j')
      , ("ķ", 'k')
      , ("ĺļľŀł", 'l')
      , ("ñńņňŉ", 'n')
      , ("òóôõöøōŏő", 'o')
      , ("ŕŗř", 'r')
      , ("śŝşš", 's')
      , ("ţťŧ", 't')
      , ("ùúûüũūŭůűų", 'u')
      , ("ŵ", 'w')
      , ("ýÿŷ", 'y')
      , ("źżž", 'z')
      ]
