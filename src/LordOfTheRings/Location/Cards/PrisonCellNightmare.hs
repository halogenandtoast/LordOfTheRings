module LordOfTheRings.Location.Cards.PrisonCellNightmare (prisonCellNightmare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedUnder, rescueCaptured)

newtype PrisonCellNightmare = PrisonCellNightmare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 1, which the scenario resolves.
prisonCellNightmare :: LocationCard PrisonCellNightmare
prisonCellNightmare = location PrisonCellNightmare Cards.prisonCellNightmare

-- | While Prison Cell is the active location, it gains: "Planning Action: Rescue a random
-- card from underneath Prison Cell. Then, raise each player's threat by X, where X is that
-- card's printed cost. (Limit once per round.)"
instance HasAbilities PrisonCellNightmare where
  getAbilities (PrisonCellNightmare a) =
    [ limited (GroupLimit PerRound 1) $ anyPlayer $ restricted a 1 (DuringPhase PlanningPhase) $ ActionAbility Free
    | a.placement == ActiveLocation
    ]

instance RunMessage PrisonCellNightmare where
  runMessage msg l@(PrisonCellNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      held <- shuffleM =<< capturedUnder a.cardId
      for_ (take 1 held) \pc -> do
        rescueCaptured a.cardId pc.id
        eachPlayer \p -> encounterRaisesThreat p (printedCostOf pc)
      pure l
    _ -> PrisonCellNightmare <$> runMessage msg a
