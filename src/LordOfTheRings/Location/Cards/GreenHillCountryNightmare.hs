module LordOfTheRings.Location.Cards.GreenHillCountryNightmare (greenHillCountryNightmare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype GreenHillCountryNightmare = GreenHillCountryNightmare LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

greenHillCountryNightmare :: LocationCard GreenHillCountryNightmare
greenHillCountryNightmare = location GreenHillCountryNightmare Cards.greenHillCountryNightmare

instance HasModifiersFor GreenHillCountryNightmare where
  getModifiersFor (GreenHillCountryNightmare a) = when (a.placement == StagingArea) $ modified a GameTarget [HideValue 1]

instance RunMessage GreenHillCountryNightmare where
  runMessage msg l@(GreenHillCountryNightmare a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      hideTest lead a 1
      GreenHillCountryNightmare <$> runMessage msg a
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attackNextPlayerAfterAttack eid pid
      pure l
    _ -> GreenHillCountryNightmare <$> runMessage msg a
