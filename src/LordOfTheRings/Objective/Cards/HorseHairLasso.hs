module LordOfTheRings.Objective.Cards.HorseHairLasso (horseHairLasso) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (captureMumak, capturedMumak, printedCost, remainingHitPoints)

newtype HorseHairLasso = HorseHairLasso ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

horseHairLasso :: ObjectiveCard HorseHairLasso
horseHairLasso = objective HorseHairLasso Cards.horseHairLasso

-- | Response: After the attached enemy is dealt any amount of damage, the engaged player
-- shuffles his deck and discards the top card. If the discarded card's cost is equal to or
-- greater than the attached enemy's remaining hit points, add Horse-hair Lasso and attached
-- enemy to the victory display.
instance HasAbilities HorseHairLasso where
  getAbilities (HorseHairLasso a) =
    [ restricted a 1 (exists $ EnemyWithId eid <> EnemyEngagedWith AnyPlayer)
        $ response
        $ DealtDamageWindow After (EnemyTarget eid)
    | Just eid <- [capturedMumak a]
    ]

-- | The top card is read once the deck is shuffled.
instance RunMessage HorseHairLasso where
  runMessage msg o@(HorseHairLasso attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (capturedMumak attrs) \eid -> do
        engaged <- placementController . (.placement) <$> getEnemy eid
        for_ engaged \pid -> do
          shufflePlayerDeck pid
          doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      for_ (capturedMumak attrs) \eid -> do
        engaged <- placementController . (.placement) <$> getEnemy eid
        for_ engaged \pid -> do
          top <- take 1 . (.deck) <$> getPlayer pid
          discardTopOfDeck pid 1
          left <- remainingHitPoints (EnemyTarget eid)
          when (any ((>= left) . printedCost) top) $ captureMumak attrs eid
      pure o
    _ -> HorseHairLasso <$> runMessage msg attrs
