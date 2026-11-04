module LordOfTheRings.Location.Cards.PlazaOfNurn (plazaOfNurn) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (castleX)

newtype PlazaOfNurn = PlazaOfNurn LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

plazaOfNurn :: LocationCard PlazaOfNurn
plazaOfNurn = location PlazaOfNurn Cards.plazaOfNurn

-- | X is 1 more than the number of Castle side quests in the victory display.
instance HasModifiersFor PlazaOfNurn where
  getModifiersFor (PlazaOfNurn a) = do
    x <- castleX
    modified a a [ThreatModifier x]

-- | Travel: Each player assigns X damage among characters he controls.
instance RunMessage PlazaOfNurn where
  runMessage msg l@(PlazaOfNurn a) = case msg of
    TravelTo lid | lid == a.id -> do
      x <- castleX
      eachPlayer \p -> dealDamageDivided a p x
      pure l
    _ -> PlazaOfNurn <$> runMessage msg a
