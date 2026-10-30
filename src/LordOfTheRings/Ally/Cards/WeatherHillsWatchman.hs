module LordOfTheRings.Ally.Cards.WeatherHillsWatchman (weatherHillsWatchman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Signal))

newtype WeatherHillsWatchman = WeatherHillsWatchman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weatherHillsWatchman :: AllyCard WeatherHillsWatchman
weatherHillsWatchman = ally WeatherHillsWatchman Cards.weatherHillsWatchman

-- | Response: After Weather Hills Watchman enters play, search the top 5 cards of your deck
-- for a Signal card and add it to your hand. Shuffle the other cards back into your deck.
instance HasAbilities WeatherHillsWatchman where
  getAbilities (WeatherHillsWatchman a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage WeatherHillsWatchman where
  runMessage msg s@(WeatherHillsWatchman a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 (CardWithTrait Signal) False
      pure s
    _ -> WeatherHillsWatchman <$> runMessage msg a
