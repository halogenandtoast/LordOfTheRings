module LordOfTheRings.Location.Cards.PitchDarkThicket (pitchDarkThicket) where

import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype PitchDarkThicket = PitchDarkThicket LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

pitchDarkThicket :: LocationCard PitchDarkThicket
pitchDarkThicket = location PitchDarkThicket Cards.pitchDarkThicket

-- | While Pitch-dark Thicket is the active location, all characters lose the ranged
-- keyword.
instance HasModifiersFor PitchDarkThicket where
  getModifiersFor (PitchDarkThicket a) = when (a.placement == ActiveLocation) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [LoseKeyword Ranged]

-- | Shadow: Attacking enemy cannot be attacked by characters with the ranged keyword until
-- the end of the phase.
instance RunMessage PitchDarkThicket where
  runMessage msg l@(PitchDarkThicket a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      phaseModifier a eid (CannotBeAttackedByKeyword Ranged)
      pure l
    _ -> PitchDarkThicket <$> runMessage msg a
