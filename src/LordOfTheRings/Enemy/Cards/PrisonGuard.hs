module LordOfTheRings.Enemy.Cards.PrisonGuard (prisonGuard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedUnder, rescueCaptured)

newtype PrisonGuard = PrisonGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 1, which the scenario resolves.
prisonGuard :: EnemyCard PrisonGuard
prisonGuard = enemy PrisonGuard Cards.prisonGuard

-- | While you are engaged with Prison Guard, it gains: "Planning Action: Rescue a random
-- card from underneath Prison Guard. Then, raise each player's threat by X, where X is that
-- card's printed cost. (Limit once per round.)"
instance HasAbilities PrisonGuard where
  getAbilities (PrisonGuard a) =
    [ limited (GroupLimit PerRound 1)
        $ anyPlayer
        $ restricted a 1 (DuringPhase PlanningPhase <> exists (EnemyWithId a.id <> EnemyEngagedWith ActivePlayer))
        $ ActionAbility Free
    ]

instance RunMessage PrisonGuard where
  runMessage msg e@(PrisonGuard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      held <- shuffleM =<< capturedUnder attrs.cardId
      for_ (take 1 held) \pc -> do
        rescueCaptured attrs.cardId pc.id
        eachPlayer \p -> encounterRaisesThreat p (printedCostOf pc)
      pure e
    _ -> PrisonGuard <$> runMessage msg attrs
