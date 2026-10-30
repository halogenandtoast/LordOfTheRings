module LordOfTheRings.Location.Cards.ForecastleDeck (forecastleDeck) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ForecastleDeck = ForecastleDeck LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forecastleDeck :: LocationCard ForecastleDeck
forecastleDeck = location ForecastleDeck Cards.forecastleDeck

-- | Forced: After you travel to Forecastle Deck, search the encounter deck and discard pile
-- for an enemy and reveal it. Shuffle the encounter deck.
instance HasAbilities ForecastleDeck where
  getAbilities (ForecastleDeck a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage ForecastleDeck where
  runMessage msg l@(ForecastleDeck attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToReveal lead attrs (CardWithType EnemyType)
      pure l
    _ -> ForecastleDeck <$> runMessage msg attrs
