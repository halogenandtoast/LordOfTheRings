module LordOfTheRings.Location.Cards.TheTowerOfBaradDur (theTowerOfBaradDur) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Of the Power of Mordor deck (The Temple of Doom, The Fortress of Nurn).
newtype TheTowerOfBaradDur = TheTowerOfBaradDur LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTowerOfBaradDur :: LocationCard TheTowerOfBaradDur
theTowerOfBaradDur = location TheTowerOfBaradDur Cards.theTowerOfBaradDur

-- | Immune to player card effects.
-- The players cannot travel here.
-- Each player's threat elimination level is reduced by 5.
instance HasModifiersFor TheTowerOfBaradDur where
  getModifiersFor (TheTowerOfBaradDur a) = do
    modifySelf a [ImmuneToPlayerCardEffects, CannotBeTraveledTo]
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [EliminationThreatModifier (-5)]

instance RunMessage TheTowerOfBaradDur where
  runMessage msg (TheTowerOfBaradDur a) = TheTowerOfBaradDur <$> runMessage msg a
