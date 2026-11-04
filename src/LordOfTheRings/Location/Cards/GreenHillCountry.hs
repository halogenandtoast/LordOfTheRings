module LordOfTheRings.Location.Cards.GreenHillCountry (greenHillCountry) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (failedHideTestThisRound)

newtype GreenHillCountry = GreenHillCountry LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

greenHillCountry :: LocationCard GreenHillCountry
greenHillCountry = location GreenHillCountry Cards.greenHillCountry

-- | While Green Hill Country is the active location, characters get +1 willpower while
-- committed to Hide tests.
instance HasModifiersFor GreenHillCountry where
  getModifiersFor (GreenHillCountry a) = when (a.placement == ActiveLocation) $ modified a GameTarget [HideTestWillpower 1]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if you have failed a Hide
-- test this round).
instance RunMessage GreenHillCountry where
  runMessage msg l@(GreenHillCountry a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      failed <- failedHideTestThisRound pid
      enemyGetsAttack (CardIdSource cid) eid (if failed then 2 else 1)
      pure l
    _ -> GreenHillCountry <$> runMessage msg a
