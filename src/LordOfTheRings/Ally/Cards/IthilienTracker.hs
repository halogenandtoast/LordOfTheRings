module LordOfTheRings.Ally.Cards.IthilienTracker (ithilienTracker) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype IthilienTracker = IthilienTracker AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ithilienTracker :: AllyCard IthilienTracker
ithilienTracker = ally IthilienTracker Cards.ithilienTracker

-- | Action: Exhaust Ithilien Tracker to lower the threat of the next enemy added to the
-- staging area to 0 until the end of the phase.
instance HasAbilities IthilienTracker where
  getAbilities (IthilienTracker a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage IthilienTracker where
  runMessage msg s@(IthilienTracker a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      phaseModifier a GameTarget (NextEnemyInStaging DoesNotContributeThreat)
      pure s
    _ -> IthilienTracker <$> runMessage msg a
