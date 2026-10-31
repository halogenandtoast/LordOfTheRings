module LordOfTheRings.Enemy.Cards.MarshWight (marshWight) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)

newtype MarshWight = MarshWight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

marshWight :: EnemyCard MarshWight
marshWight = enemy MarshWight Cards.marshWight

instance HasAbilities MarshWight where
  getAbilities (MarshWight a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Forced: After Marsh-wight engages a player, that player makes an escape test, dealing 3
-- cards from the encounter deck. If this test is failed, place 2 resource tokens on Gollum
-- and Marsh-wight makes an immediate attack.
instance RunMessage MarshWight where
  runMessage msg e@(MarshWight attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [p | EngagedWith p <- [attrs.placement]] \pid ->
        escapeTest pid attrs 3 EscapeWillpower (placeOnGollum attrs 2 >> immediateAttack pid attrs.id) (pure ())
      pure e
    _ -> MarshWight <$> runMessage msg attrs
