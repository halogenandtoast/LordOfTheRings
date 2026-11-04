module LordOfTheRings.Location.Cards.TownGate (townGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (dagnirAttacksFirstPlayer, damageHrogarsHill)

newtype TownGate = TownGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

townGate :: LocationCard TownGate
townGate = location TownGate Cards.townGate

-- | While Town Gate is in the staging area, it gains: "Forced: At the end of the quest
-- phase, place 1 resource here. Then, if there are 3 resources here, discard Town Gate
-- and deal 4 damage to Hrogar's Hill."
instance HasAbilities TownGate where
  getAbilities (TownGate a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == StagingArea]

-- | Travel: Dagnir makes an immediate attack against the first player.
instance RunMessage TownGate where
  runMessage msg l@(TownGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      when (countTokens Resource a.locationTokens + 1 >= 3) do
        discard a
        damageHrogarsHill a 4
      pure l
    TravelTo lid | lid == a.id -> do
      dagnirAttacksFirstPlayer
      TownGate <$> runMessage msg a
    _ -> TownGate <$> runMessage msg a
