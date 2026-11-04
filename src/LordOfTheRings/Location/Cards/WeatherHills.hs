module LordOfTheRings.Location.Cards.WeatherHills (weatherHills) where

import LordOfTheRings.Card (Card (EncounterCard), toCard)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype WeatherHills = WeatherHills LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weatherHills :: LocationCard WeatherHills
weatherHills = location WeatherHills Cards.weatherHills

-- | Response: After the players travel here, place 1 non-unique enemy in play facedown
-- under this location. While an enemy is under this location, it is out of play.
-- Forced: After this location leaves play, return each enemy under it to the staging area
-- face-up.
instance HasAbilities WeatherHills where
  getAbilities (WeatherHills a) =
    [ restricted a 1 (exists $ not_ UniqueEnemy) $ response $ TraveledWindow After a.id
    , mkAbility a 2 $ forced $ LocationLeftPlayWindow After a.id
    ]

-- | The enemy leaves play for underneath it, its tokens, attachments and shadow cards gone,
-- as an enemy placed under a location does. Out of play, Weather Hills still holds them.
instance RunMessage WeatherHills where
  runMessage msg l@(WeatherHills a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (not_ UniqueEnemy)
      chooseOrRunOneM pid do
        questionLabeled "weatherHills.choose"
        causedBy a
        targets enemies \e -> do
          card <- toCard <$> getEnemy e
          placeUnderneath a [card]
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      for_ [c | EncounterCard c <- a.underneath] addToStaging
      pure l
    _ -> WeatherHills <$> runMessage msg a
