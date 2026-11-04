module LordOfTheRings.Treachery.Cards.StreetFightingWrathAndRuin (streetFightingWrathAndRuin) where

import LordOfTheRings.Helpers.ControlledLocation (controlledLocations)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StreetFightingWrathAndRuin = StreetFightingWrathAndRuin TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Wrath and Ruin's Street Fighting (22021), not Assault on Osgiliath's.
streetFightingWrathAndRuin :: TreacheryCard StreetFightingWrathAndRuin
streetFightingWrathAndRuin = treachery StreetFightingWrathAndRuin Cards.streetFightingWrathAndRuin

-- | When Revealed: Each player assigns X damage among characters he controls, where X is 1
-- more than the number of locations he controls.
-- Shadow: Assign X damage among characters you control, where X is 1 more than the number
-- of locations you control.
instance RunMessage StreetFightingWrathAndRuin where
  runMessage msg t@(StreetFightingWrathAndRuin attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> dealDamageDivided attrs pid . (+ 1) . length =<< controlledLocations pid
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      dealDamageDivided attrs pid . (+ 1) . length =<< controlledLocations pid
      pure t
    _ -> StreetFightingWrathAndRuin <$> runMessage msg attrs
