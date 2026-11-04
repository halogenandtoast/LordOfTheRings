module LordOfTheRings.Treachery.Cards.SulfurCloud (sulfurCloud) where

import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)
import LordOfTheRings.Trait (Trait (Underwater))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SulfurCloud = SulfurCloud TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sulfurCloud :: TreacheryCard SulfurCloud
sulfurCloud = treachery SulfurCloud Cards.sulfurCloud

-- | When Revealed: If the active location is Underwater, choose a number and raise each
-- player's threat by that number. No more than X progress may be placed on Underwater
-- locations this phase, where X is twice the chosen number. If the active location is not
-- Underwater, Sulfur Cloud gains surge.
-- The first player chooses the number, up to what would raise the lowest threat to 50: a
-- larger one would eliminate every player. The limit holds on every Underwater location
-- together for the phase, one that turns Underwater later too.
instance RunMessage SulfurCloud where
  runMessage msg t@(SulfurCloud attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      underwater <- underwaterActive
      if underwater
        then do
          lead <- getLead
          threats <- traverse (fmap (.threat) . getPlayer) =<< getRemainingPlayers
          let most = maybe 0 (\lowest -> max 0 (50 - lowest)) (minimumMay threats)
          chooseAmountM lead "sulfurCloud.number" attrs 0 most \n -> do
            eachPlayer \p -> encounterRaisesThreat p n
            phaseModifier attrs GameTarget (PhaseProgressLimitOnTrait Underwater (2 * n))
        else revealEncounterCard
      pure t
    _ -> SulfurCloud <$> runMessage msg attrs
