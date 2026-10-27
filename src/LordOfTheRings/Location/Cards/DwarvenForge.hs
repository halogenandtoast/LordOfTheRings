module LordOfTheRings.Location.Cards.DwarvenForge (dwarvenForge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DwarvenForge = DwarvenForge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dwarvenForge :: LocationCard DwarvenForge
dwarvenForge = location DwarvenForge Cards.dwarvenForge

-- | Lost: Each player must choose and discard 1 card from his hand.
instance RunMessage DwarvenForge where
  runMessage msg l@(DwarvenForge attrs) = case msg of
    TriggerLostEffects _ -> do
      eachPlayer \p -> chooseAndDiscardFromHand p attrs 1
      pure l
    _ -> DwarvenForge <$> runMessage msg attrs
