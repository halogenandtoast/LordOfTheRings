module LordOfTheRings.Objective.Cards.NooseOfVines (nooseOfVines) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (captureMumak, capturedMumak, remainingHitPoints)

newtype NooseOfVines = NooseOfVines ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

nooseOfVines :: ObjectiveCard NooseOfVines
nooseOfVines = objective NooseOfVines Cards.nooseOfVines

-- | Response: After a character defends an attack against the attached enemy, if the
-- defending character has more remaining hit points than the attached enemy, add Noose of
-- Vines and attached enemy to the victory display.
instance HasAbilities NooseOfVines where
  getAbilities (NooseOfVines a) =
    [mkAbility a 1 $ response $ DefendedAttackAgainstWindow After eid | Just eid <- [capturedMumak a]]

-- | A defender destroyed by the attack has left play, with no hit points left to compare.
instance RunMessage NooseOfVines where
  runMessage msg o@(NooseOfVines attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (capturedMumak attrs) \eid -> do
        let defenders = [d | Window _ (DefendedAttack d eid') <- ws, eid' == eid]
        left <- remainingHitPoints (EnemyTarget eid)
        stronger <- filterM (fmap (> left) . remainingHitPoints) =<< filterM inPlay defenders
        unless (null stronger) $ captureMumak attrs eid
      pure o
    _ -> NooseOfVines <$> runMessage msg attrs
   where
    inPlay = \case
      HeroTarget hid -> selectAny (HeroWithId hid)
      AllyTarget aid -> selectAny (AllyWithId aid)
      _ -> pure False
