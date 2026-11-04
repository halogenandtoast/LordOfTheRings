module LordOfTheRings.Location.Cards.TheBlackGate (theBlackGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype TheBlackGate = TheBlackGate LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theBlackGate :: LocationCard TheBlackGate
theBlackGate = location TheBlackGate Cards.theBlackGate

-- | Immune to player card effects. The players cannot travel here. The Black Gate gets +1
-- threat for each resource on it.
instance HasModifiersFor TheBlackGate where
  getModifiersFor (TheBlackGate a) = modifySelf a [ImmuneToPlayerCardEffects, CannotBeTraveledTo, ThreatPerToken Resource]

-- | Forced: At the beginning of the quest phase, place 1 resource here and raise each
-- player's threat by 1.
instance HasAbilities TheBlackGate where
  getAbilities (TheBlackGate a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TheBlackGate where
  runMessage msg l@(TheBlackGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      eachPlayer \p -> encounterRaisesThreat p 1
      pure l
    _ -> TheBlackGate <$> runMessage msg a
