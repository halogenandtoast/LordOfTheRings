module LordOfTheRings.Objective.Cards.IthilienGuardian (ithilienGuardian) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype IthilienGuardian = IthilienGuardian ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | When Revealed: Add Ithilien Guardian to the staging area and Ithilien Guardian gains
-- surge. (A revealed objective goes to the staging area; its surge is printed in its def.)
ithilienGuardian :: ObjectiveCard IthilienGuardian
ithilienGuardian = objective IthilienGuardian Cards.ithilienGuardian

-- | While Ithilien Guardian is in the staging area, he is committed to the current quest.
instance HasModifiersFor IthilienGuardian where
  getModifiersFor (IthilienGuardian a) = when (a.placement == StagingArea) $ modified a a [CountsAsCommitted]

-- | Shadow: Deal 2 damage to the attacking enemy.
instance RunMessage IthilienGuardian where
  runMessage msg o@(IthilienGuardian attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      dealDamage attrs eid 2
      pure o
    _ -> IthilienGuardian <$> runMessage msg attrs
