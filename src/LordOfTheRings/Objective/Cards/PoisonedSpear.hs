module LordOfTheRings.Objective.Cards.PoisonedSpear (poisonedSpear) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (captureMumak, capturedMumak, remainingHitPoints)

newtype PoisonedSpear = PoisonedSpear ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

poisonedSpear :: ObjectiveCard PoisonedSpear
poisonedSpear = objective PoisonedSpear Cards.poisonedSpear

-- | Response: After the players quest successfully, the engaged player discards the top
-- card of the encounter deck. If the discarded card's threat is equal to or greater than
-- the attached enemy's remaining hit points, add Poisoned Spear and attached enemy to the
-- victory display.
instance HasAbilities PoisonedSpear where
  getAbilities (PoisonedSpear a) =
    [ restricted a 1 (exists $ EnemyWithId eid <> EnemyEngagedWith AnyPlayer) $ response $ QuestedSuccessfullyWindow After
    | Just eid <- [capturedMumak a]
    ]

instance RunMessage PoisonedSpear where
  runMessage msg o@(PoisonedSpear attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (capturedMumak attrs) \eid -> do
        left <- remainingHitPoints (EnemyTarget eid)
        when (any ((>= left) . fromMaybe 0 . cdThreat . toCardDef) cards) $ captureMumak attrs eid
      pure o
    _ -> PoisonedSpear <$> runMessage msg attrs
