module LordOfTheRings.Ally.Cards.GafferGamgee (gafferGamgee) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (placementController)

newtype GafferGamgee = GafferGamgee AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gafferGamgee :: AllyCard GafferGamgee
gafferGamgee = ally GafferGamgee Cards.gafferGamgee

-- | Combat Action: Return Gaffer Gamgee to your hand to choose an enemy with engagement
-- cost higher than your threat. Until the end of the round, that enemy cannot attack you.
instance HasAbilities GafferGamgee where
  getAbilities (GafferGamgee a) =
    [ restricted a 1 (DuringPhase CombatPhase <> exists (EnemyWithEngagementCostAboveThreatOf pid)) $ ActionAbility Free
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage GafferGamgee where
  runMessage msg s@(GafferGamgee a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyWithEngagementCostAboveThreatOf pid)
      returnToHand a
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \eid -> roundModifier a eid (CannotAttackPlayer pid)
      pure s
    _ -> GafferGamgee <$> runMessage msg a
