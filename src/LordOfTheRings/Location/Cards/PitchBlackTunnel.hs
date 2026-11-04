module LordOfTheRings.Location.Cards.PitchBlackTunnel (pitchBlackTunnel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PitchBlackTunnel = PitchBlackTunnel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Track 4.
pitchBlackTunnel :: LocationCard PitchBlackTunnel
pitchBlackTunnel = location PitchBlackTunnel Cards.pitchBlackTunnel

-- | X is the number of characters controlled by the first player.
instance HasModifiersFor PitchBlackTunnel where
  getModifiersFor (PitchBlackTunnel a) = do
    lead <- getLead
    x <- length <$> selectCharacters (HeroControlledBy $ PlayerWithId lead) (AllyControlledBy $ PlayerWithId lead)
    modifySelf a [ThreatModifier x | x > 0]

-- | Travel: Reveal the top card of the encounter deck to travel here.
instance RunMessage PitchBlackTunnel where
  runMessage msg l@(PitchBlackTunnel attrs) = case msg of
    TravelTo lid | lid == attrs.id -> do
      revealEncounterCard
      pure l
    _ -> PitchBlackTunnel <$> runMessage msg attrs
