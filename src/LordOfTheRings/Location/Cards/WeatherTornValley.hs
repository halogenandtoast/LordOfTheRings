module LordOfTheRings.Location.Cards.WeatherTornValley (weatherTornValley) where

import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Trait (Trait (Weather))

newtype WeatherTornValley = WeatherTornValley LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

weatherTornValley :: LocationCard WeatherTornValley
weatherTornValley = location WeatherTornValley Cards.weatherTornValley

-- | Progress cannot be placed on Weather-torn Valley while it is in the staging area.
-- While Weather-torn Valley is the active location, Weather cards cannot be canceled: the
-- When Revealed effect of each Weather treachery.
instance HasModifiersFor WeatherTornValley where
  getModifiersFor (WeatherTornValley a) = do
    when (a.placement == StagingArea) $ modifySelf a [ProgressLimit 0]
    when (a.placement == ActiveLocation) do
      weather <- getsGame (filter ((Weather `elem`) . cdCardTraits . toCardDef) . toList . (.treacheries) . gameEntities)
      for_ weather \t -> modified a t [WhenRevealedCannotBeCanceled]

instance RunMessage WeatherTornValley where
  runMessage msg (WeatherTornValley a) = WeatherTornValley <$> runMessage msg a
