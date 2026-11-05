module LordOfTheRings.Enemy.Cards.OrcOfTheWhiteHand (orcOfTheWhiteHand) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype OrcOfTheWhiteHand = OrcOfTheWhiteHand EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcOfTheWhiteHand :: EnemyCard OrcOfTheWhiteHand
orcOfTheWhiteHand = enemy OrcOfTheWhiteHand Cards.orcOfTheWhiteHand

instance HasAbilities OrcOfTheWhiteHand where
  getAbilities (OrcOfTheWhiteHand a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage OrcOfTheWhiteHand where
  runMessage msg e@(OrcOfTheWhiteHand a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      whenM (attackDestroyedACharacter a.id) $ increasePursuit 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      increasePursuit 1
      pure e
    _ -> OrcOfTheWhiteHand <$> runMessage msg a
