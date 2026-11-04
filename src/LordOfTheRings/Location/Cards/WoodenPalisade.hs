module LordOfTheRings.Location.Cards.WoodenPalisade (woodenPalisade) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (getDagnir)

newtype WoodenPalisade = WoodenPalisade LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodenPalisade :: LocationCard WoodenPalisade
woodenPalisade = location WoodenPalisade Cards.woodenPalisade

-- | While Wooden Palisade is the active location, Dagnir gets -2 [attack].
instance HasModifiersFor WoodenPalisade where
  getModifiersFor (WoodenPalisade a) =
    when (a.placement == ActiveLocation) $ getDagnir >>= traverse_ \d -> modified a (EnemyTarget d) [AttackModifier (-2)]

-- | Travel: The first player readies a hero he controls.
instance RunMessage WoodenPalisade where
  runMessage msg l@(WoodenPalisade a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      heroes <- select (HeroControlledBy (PlayerWithId lead) <> ExhaustedHero)
      chooseOrRunOneM lead do
        questionLabeled "woodenPalisade.ready"
        causedBy a
        targets heroes ready
      pure l
    _ -> WoodenPalisade <$> runMessage msg a
