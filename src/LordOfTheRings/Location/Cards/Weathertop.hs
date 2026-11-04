module LordOfTheRings.Location.Cards.Weathertop (weathertop) where

import LordOfTheRings.Helpers.Fellowship (getSagaOneRing, theOneRingExhausted)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype Weathertop = Weathertop LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | While Weathertop is in the victory display, each enemy in play gets -1 threat: the
-- scenario gives it, Weathertop being out of play there.
weathertop :: LocationCard Weathertop
weathertop = location Weathertop Cards.weathertop

-- | Immune to player card effects. The players cannot travel here unless The One Ring can
-- be exhausted to.
instance HasModifiersFor Weathertop where
  getModifiersFor (Weathertop a) = do
    ring <- getSagaOneRing
    exhausted <- theOneRingExhausted
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | isNothing ring || exhausted]

-- | Travel: The first player must exhaust The One Ring to travel here.
instance RunMessage Weathertop where
  runMessage msg (Weathertop a) = case msg of
    TravelTo lid | lid == a.id -> do
      getSagaOneRing >>= traverse_ (exhaust . ObjectiveTarget . (.id))
      Weathertop <$> runMessage msg a
    _ -> Weathertop <$> runMessage msg a
