module LordOfTheRings.Objective.Cards.PitTrap (pitTrap) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (captureMumak, capturedMumak, remainingHitPoints)

newtype PitTrap = PitTrap ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pitTrap :: ObjectiveCard PitTrap
pitTrap = objective PitTrap Cards.pitTrap

-- | Response: After the players travel to a location, if the active location's printed
-- threat is equal to or greater than the attached enemy's remaining hit points, the engaged
-- player adds Pit Trap and attached enemy to the victory display.
instance HasAbilities PitTrap where
  getAbilities (PitTrap a) =
    [ restricted a 1 (exists $ EnemyWithId eid <> EnemyEngagedWith AnyPlayer) $ response $ AnyTraveledWindow After
    | Just eid <- [capturedMumak a]
    ]

instance RunMessage PitTrap where
  runMessage msg o@(PitTrap attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (capturedMumak attrs) \eid -> do
        left <- remainingHitPoints (EnemyTarget eid)
        active <- traverse getLocation =<< select IsActiveLocation
        when (any ((>= left) . fromMaybe 0 . cdThreat . toCardDef) active) $ captureMumak attrs eid
      pure o
    _ -> PitTrap <$> runMessage msg attrs
