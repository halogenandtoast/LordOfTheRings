module LordOfTheRings.Treachery.Cards.BlindWrath (blindWrath) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Harad))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlindWrath = BlindWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blindWrath :: TreacheryCard BlindWrath
blindWrath = treachery BlindWrath Cards.blindWrath

instance RunMessage BlindWrath where
  runMessage msg t@(BlindWrath a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      roundModifiers a GameTarget $ map (EachMatchingEnemyInStagingArea $ CardWithTrait Harad)
        [ThreatModifier 2, EngagementCostModifier (-30), CannotBeOptionallyEngagedBy AnyPlayer]
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      placeEnemy eid StagingArea
      pure t
    _ -> BlindWrath <$> runMessage msg a
