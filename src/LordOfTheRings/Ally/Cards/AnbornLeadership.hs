module LordOfTheRings.Ally.Cards.AnbornLeadership (anbornLeadership) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype AnbornLeadership = AnbornLeadership AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

anbornLeadership :: AllyCard AnbornLeadership
anbornLeadership = ally AnbornLeadership Cards.anbornLeadership

-- | Response: After an enemy is added to the staging area, exhaust Anborn to give that
-- enemy +5 engagement cost until the end of the round. Then, deal 1 damage to that enemy.
instance HasAbilities AnbornLeadership where
  getAbilities (AnbornLeadership a) =
    [mkAbility a 1 $ ResponseAbility (EnemyEntersStagingAreaWindow After) (exhaustCost a)]

instance RunMessage AnbornLeadership where
  runMessage msg s@(AnbornLeadership a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (EntersStagingArea (EnemyTarget eid)) <- windows]) \eid -> do
        roundModifier a eid (EngagementCostModifier 5)
        dealDamage a eid 1
      pure s
    _ -> AnbornLeadership <$> runMessage msg a
