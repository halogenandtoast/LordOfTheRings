module LordOfTheRings.Location.Cards.WeatherDeck (weatherDeck) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame)
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
-- "Travel: Each player must exhaust a character he controls to travel here."
-- ponytail: only the first copy in the staging area gives it, so the cost is paid once
gives :: HasGame m => LocationAttrs -> m Bool
gives a
  | a.placement /= StagingArea = pure False
  | otherwise = not <$> selectAny (LocationInStagingArea <> LocationWithCardCode (toCardCode Cards.weatherDeck) <> LocationWithIdBelow a.id)

otherDecks :: LocationAttrs -> LocationMatcher
otherDecks a = LocationWithTrait Deck <> not_ (LocationWithId a.id)

instance HasModifiersFor WeatherDeck where
  getModifiersFor (WeatherDeck a) = whenM (gives a) do
    players <- getRemainingPlayers
    unable <- flip anyM players \p ->
      null <$> selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
    when unable do
      decks <- select (otherDecks a)
      for_ decks \l -> modified a l [CannotBeTraveledTo]

-- | Shadow: Exhaust a character you control.
instance RunMessage WeatherDeck where
  runMessage msg l@(WeatherDeck attrs) = case msg of
    TravelTo lid -> do
      paid <- (&&) <$> gives attrs <*> selectAny (otherDecks attrs <> LocationWithId lid)
      when paid $ eachPlayer \p -> chooseAndExhaust attrs p 1
      WeatherDeck <$> runMessage msg attrs
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure l
    _ -> WeatherDeck <$> runMessage msg attrs
