module LordOfTheRings.Ally.Cards.MirkwoodRunner (mirkwoodRunner) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

newtype MirkwoodRunner = MirkwoodRunner AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodRunner :: AllyCard MirkwoodRunner
mirkwoodRunner = ally MirkwoodRunner Cards.mirkwoodRunner

-- | While Mirkwood Runner is attacking alone, the defending enemy does not count its
-- defense.
instance HasModifiersFor MirkwoodRunner where
  getModifiersFor (MirkwoodRunner a) = unless (a.placement == Unplaced) $ modifySelf a [IgnoresDefenseWhenAttackingAlone]
