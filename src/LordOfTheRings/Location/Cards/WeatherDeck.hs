module LordOfTheRings.Location.Cards.WeatherDeck (weatherDeck) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Deck))

newtype WeatherDeck = WeatherDeck LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

weatherDeck :: LocationCard WeatherDeck
weatherDeck = location WeatherDeck Cards.weatherDeck

-- | While Weather Deck is in the staging area, each other Deck location in play gains
-- "Travel: Each player must exhaust a character he controls to travel here." Each copy in
-- the staging area gives it, so a Deck location may gain it more than once.
otherDecks :: LocationAttrs -> LocationMatcher
otherDecks a = LocationWithTrait Deck <> not_ (LocationWithId a.id)

instance HasModifiersFor WeatherDeck where
  getModifiersFor (WeatherDeck a) = when (a.placement == StagingArea) do
    players <- getRemainingPlayers
    readied <- for players \p ->
      length <$> selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
    givers <- select (LocationInStagingArea <> LocationWithCardCode (toCardCode Cards.weatherDeck))
    decks <- select (otherDecks a)
    for_ decks \l -> do
      let costs = length (filter (/= l) givers)
      when (any (< costs) readied) $ modified a l [CannotBeTraveledTo]

-- | Shadow: Exhaust a character you control.
instance RunMessage WeatherDeck where
  runMessage msg l@(WeatherDeck attrs) = case msg of
    TravelTo lid | attrs.placement == StagingArea -> do
      paid <- selectAny (otherDecks attrs <> LocationWithId lid)
      when paid $ eachPlayer \p -> chooseAndExhaust attrs p 1
      WeatherDeck <$> runMessage msg attrs
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure l
    _ -> WeatherDeck <$> runMessage msg attrs
